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
    field("magnitude", int8()),
    field("office", string()),
    field("party", string())
  )
  
  base_data <- open_dataset(path, partitioning = c("state", "county_name"), schema = partial_schema, format = "parquet") |>
    filter(state == st, magnitude == 1, !is.na(office), !is.na(district), candidate != "undervote", candidate != "overvote", candidate != "writein") |>
    select(-magnitude) |>
    mutate(race = str_c(office, district, sep = "_"))

  # pick some random people
  randoms <- base_data |>
    distinct(state, county_name, cvr_id) |>
    collect() |> 
    slice_sample(n=num)
  
  # What are the contested races?
  small_candidates <- base_data |> 
    inner_join(randoms, join_by(state, county_name, cvr_id)) |>
    count(race, candidate) |> 
    filter(n <= 10) |> 
    distinct(race, candidate)
  
  contested_races <- base_data |> 
    anti_join(small_candidates, join_by(race, candidate)) |>
    distinct(race, candidate) |> 
    count(race) |> 
    filter(n > 1) |> 
    select(race)
  
  if (partisan_only){
    # What are the partisan races in the data?
    partisan_races <- base_data |> 
      distinct(race, party) |>
      filter(party != "nonpartisan") |> 
      distinct(race)
    
    contested_races <- inner_join(contested_races, partisan_races)
  }
  
  base_data |> 
    inner_join(randoms, join_by(state, county_name, cvr_id)) |>
    inner_join(contested_races, join_by(race)) |> 
    filter(!is.na(party)) |> 
    mutate(choice_rep = as.numeric(party == "republican")) |> 
    collect()
}

filter_byCounty <- function(data, county){
  small_candidates <- data |>
    filter(county_name == county) |> 
    count(race, candidate) |> 
    filter(n <= 10) |> 
    distinct(race, candidate)

  data |>
    filter(county_name == county) |>
    anti_join(small_candidates, join_by(race, candidate))
}

get_stan_data <- function(data, dims=1){
  
  # Assign unique IDs to races and candidates
  ids <- data |> 
    filter(candidate != "undervote") |> 
    count(race, candidate) |> 
    arrange(race, desc(n)) |> 
    group_by(race) |> 
    mutate(candidate_id = 1:n(),
           race_id = cur_group_id())
  
  # Join back to the original data
  df <- data |> 
    filter(candidate != "undervote") |> 
    left_join(ids, join_by(race, candidate))
  
  # Create the votes matrix
  votes_matrix <- df |> 
    select(county_name, cvr_id, race_id, candidate_id, n) |> 
    arrange(race_id, desc(n)) |>
    select(-n) |> 
    pivot_wider(names_from = race_id, values_from = candidate_id, values_fill = 0) |> 
    select(-cvr_id, -county_name) |> 
    as.matrix()
  
  # Prepare data for Stan
  stan_data <- list(
    N_voters = distinct(df, county_name, cvr_id) |> tally() |> pull(),
    N_contests = n_distinct(ids$race),
    N_cands = length(ids$candidate),
    votes = votes_matrix,
    sizes = distinct(df, race, race_id, candidate) |> count(race_id) |> pull(n)
  )

  if (dims > 1)stan_data$N_dims = dims
  
  return(stan_data)
  
}