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
                              partitioning = "state",
                              schema = partial_schema,
                              format = "parquet") |> 
      filter(magnitude == "1", !is.na(office), !is.na(district)) |> 
      select(-magnitude)
  } else {
    base_data <- open_dataset(path,
                              partitioning = "state",
                              schema = partial_schema,
                              format = "parquet") |> 
      filter(state == st, magnitude == "1", !is.na(office), !is.na(district)) |> 
      select(-magnitude)
  }
  
  # What are the contested races?
  contested_races <- base_data |> 
    distinct(county_name, office, district, candidate) |> 
    arrange(county_name, office, district) |> 
    collect() |> 
    filter(n() > 1, .by = c(county_name, office, district)) |> 
    distinct(county_name, office, district)
  
  if (partisan_only){
    # What are the partisan races in the data?
    partisan_races <- base_data |> 
      filter(party_detailed != "NONPARTISAN") |> 
      distinct(county_name, office, district) |> 
      collect()
    
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
    inner_join(contested_races, by = c("county_name", "office", "district")) |> 
    mutate(choice_dem = as.numeric(party_detailed == "DEMOCRAT"),
           choice_rep = as.numeric(party_detailed == "REPUBLICAN")) |> 
    collect() |> 
    mutate(district = if_else(office %in% c("US HOUSE", "STATE SENATE", "STATE HOUSE"), 
                              str_remove(district, str_c(", ", county_name)),
                              district)) |> 
    mutate(race = str_c(office, district, sep = " - "))
}

filter_byCounty <- function(data, county){
  filter(data, county_name == county)
}

pick_random_voters <- function(data, n){
  slice_sample(distinct(data, cvr_id), n=n)
}

get_stan_data <- function(data){
  
  df <- data |> 
    # propositions are not quite right
    mutate(candidate = case_when(
      str_detect(race, "PROPOSITION") ~ str_c(race, candidate, sep = " - "),
      TRUE ~ candidate
    ))
  
  # Assign unique IDs to races and candidates
  races <- df |> 
    distinct(race) |> 
    arrange(race) |> 
    mutate(race_id = row_number())
  
  candidates <- df |> 
    distinct(candidate) |> 
    arrange(candidate) |> 
    mutate(candidate_id = row_number())
  
  # Create the candidate availability matrix
  candidate_availability <- df |> 
    summarize(available = n() > 0, .by = c(race, candidate)) |> 
    left_join(races, by = "race") |> 
    left_join(candidates, by = "candidate") |> 
    select(-race, -candidate) |> 
    collect() |> 
    drop_na(race_id, candidate_id) |> 
    complete(race_id, candidate_id, fill = list(available = FALSE)) |> 
    pivot_wider(names_from = candidate_id, values_from = available) |> 
    select(-race_id) |> 
    mutate(across(everything(), as.numeric)) |> 
    as.matrix()
  
  # Join back to the original data
  df <- df |> 
    left_join(races, by = "race") |> 
    left_join(candidates, by = "candidate")
  
  # some races are not classified perfectly in districts rn so they would show up as list-columns (bad)
  bad_races <- df |> 
    count(cvr_id, race_id) |> 
    filter(n > 1) |> 
    distinct(race_id) |> 
    pull(race_id)
  
  df <- df |> 
    filter(!(race_id %in% bad_races)) |>
    drop_na(race_id, candidate_id)
  
  # Create the votes matrix
  votes_matrix <- df |> 
    select(cvr_id, race_id, candidate_id) |> 
    arrange(race_id, candidate_id) |>
    pivot_wider(names_from = race_id, values_from = candidate_id, values_fill = 0) |> 
    select(-cvr_id)
  
  missing <- setdiff(races$race_id, colnames(votes_matrix)) |> as.character()
  
  if (length(missing) > 0){
    votes_matrix[, missing] <- 0
    votes_matrix <- relocate(votes_matrix, all_of(as.character(races$race_id))) |> 
      as.matrix()
  }
  
  eligibility_matrix <- ifelse(votes_matrix > 0, 1, 0)
  
  # Prepare data for Stan
  stan_data <- list(
    J = n_distinct(df$cvr_id),
    K = max(races$race_id),
    C = max(candidates$candidate_id),
    candidates = candidate_availability,
    eligibility = eligibility_matrix,
    votes = votes_matrix,
    parallelize = 1
  )
  
  return(stan_data)
  
}