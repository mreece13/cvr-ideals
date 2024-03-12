# R/functions_data.R

###########
## @param path -- the base path to the data
## @param st -- the full name of the state to extract, or "all" to get all of them
## @param partisan -- should the data to be filtered to partisan races only?
## @param num -- how many voters should be sampled? If left blank, selects everyone
get_data <- function(path, st, partisan_only = FALSE, num = 1e9){
  
  # define only the variables I need to run the analysis, to limit the size of the loaded data
  partial_schema <- schema(
    field("state", string()),
    field("cvr_id", string()),
    field("county_name", string()),
    field("candidate", string()),
    field("district", string()),
    field("magnitude", string()),
    field("office", string()),
    field("party_detailed", string())
  )
  
  if (st == "all"){
    base_data <- open_dataset(path,
                              partitioning = c("state", "county_name"),
                              schema = partial_schema,
                              format = "parquet") |> 
      filter(magnitude == "1", !is.na(office), !is.na(district)) |> 
      select(-magnitude)
  } else {
    base_data <- open_dataset(path,
                              partitioning = c("state", "county_name"),
                              schema = partial_schema,
                              format = "parquet") |> 
      filter(state == st, magnitude == "1", !is.na(office), !is.na(district), candidate != "undervote") |> 
      select(-magnitude) |> 
      mutate(race = str_c(office, district, sep = " - "))
  }
  
  # What are the contested races?
  small_candidates <- base_data |> 
    count(race, candidate) |> 
    filter(n <= 20) |> 
    distinct(race, candidate)
  
  contested_races <- base_data |> 
    anti_join(small_candidates) |> 
    distinct(race, candidate) |> 
    count(race) |> 
    filter(n > 1) |> 
    select(race)
  
  if (partisan_only){
    # What are the partisan races in the data?
    partisan_races <- base_data |> 
      distinct(race, party_detailed) |>
      filter(party_detailed != "NONPARTISAN") |> 
      distinct(race)
    
    contested_races <- inner_join(contested_races, partisan_races)
  }
  
  # pick some random people
  randoms <- base_data |>
    distinct(county_name, cvr_id) |>
    collect() |> 
    slice_sample(n=num)
  
  # return the data
  # one notable thing here is that there are some errors in the Colorado districts
  # for statewide races that need to be corrected
  base_data |> 
    inner_join(randoms, by = c("county_name", "cvr_id")) |>
    inner_join(contested_races, by = c("race")) |> 
    filter(!is.na(party_detailed)) |> 
    mutate(choice_rep = as.numeric(party_detailed == "REPUBLICAN")) |> 
    collect()
}

filter_byCounty <- function(data, county){
  filter(data, county_name == county)
}

get_stan_data <- function(data){
  
  # Assign unique IDs to races and candidates
  ids <- data |> 
    filter(candidate != "undervote") |> 
    distinct(race, candidate) |> 
    arrange(race, candidate) |> 
    group_by(race) |> 
    mutate(candidate_id = 1:n(),
           race_id = cur_group_id())
  
  # Join back to the original data
  df <- data |> 
    filter(candidate != "undervote") |> 
    left_join(ids, join_by(race, candidate))
  
  # Create the votes matrix
  votes_matrix <- df |> 
    select(county_name, cvr_id, race_id, candidate_id) |> 
    arrange(race_id, candidate_id) |>
    pivot_wider(names_from = race_id, values_from = candidate_id, values_fill = 0) |> 
    select(-cvr_id, -county_name) |> 
    as.matrix()
  
  num_cands <- df |> 
    distinct(race, race_id, candidate) |> 
    count(race_id) |> 
    pull(n)
  
  # Prepare data for Stan
  stan_data <- list(
    threaded = 0,
    J = df |> distinct(county_name, cvr_id) |> tally() |> pull(),
    K = n_distinct(ids$race),
    C = length(ids$candidate),
    votes = votes_matrix,
    sizes = num_cands
  )
  
  return(stan_data)
  
}