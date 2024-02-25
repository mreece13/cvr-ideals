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
    mutate(district = if_else(office %in% c("US HOUSE", "STATE SENATE", "STATE HOUSE", "US SENATE", 
                                            "DISTRICT ATTORNEY", "CITY COUNCIL", "BOARD OF EDUCATION"), 
                              str_remove(district, str_c(", ", county_name)),
                              district)) |> 
    mutate(district = ifelse(office %in% c("US SENATE", "US PRESIDENT"), "STATEWIDE", district)) |> 
    mutate(race = str_c(office, district, sep = " - "))
}

filter_byCounty <- function(data, county){
  filter(data, county_name == county)
}

pick_random_voters <- function(data, n){
  slice_sample(distinct(data, cvr_id), n=n)
}

group_voters <- function(data, categorical){
  
  if (categorical){
    uniques <- data |> 
      arrange(race, candidate) |> 
      mutate(choice = str_c(race, candidate, sep = "|")) |> 
      select(state, county_name, cvr_id, choice) |> 
      nest(data = choice) |> 
      mutate(pattern = map_chr(data, ~ str_flatten(pull(.x, choice), collapse = "||")))
    
    grouped <- uniques |> 
      left_join(count(uniques, pattern) |> mutate(group_id = as.character(row_number()))) |> 
      select(state, county_name, data, n, group_id) |> 
      unnest(cols = data) |> 
      separate_wider_delim(choice, delim = "|", names = c("race", "candidate"))
  } else {
    uniques <- data |> 
      arrange(race, candidate) |> 
      mutate(choice = str_c(race, choice_rep, sep = "|")) |> 
      select(state, county_name, cvr_id, choice) |> 
      nest(data = choice) |> 
      mutate(pattern = map_chr(data, ~ str_flatten(pull(.x, choice), collapse = "||")))
    
    grouped <- uniques |> 
      left_join(count(uniques, pattern) |> mutate(group_id = as.character(row_number()))) |>
      select(state, county_name, data, n, group_id) |> 
      unnest(cols = data) |> 
      separate_wider_delim(choice, delim = "|", names = c("race", "choice_rep"))
    
  }
  
  return(grouped)
}

get_stan_data <- function(data){
  
  df <- data |> 
    # propositions are not quite right
    mutate(candidate = case_when(
      str_detect(race, "PROPOSITION") ~ str_c(race, candidate, sep = " - "),
      TRUE ~ candidate
    ),
    race = str_remove(race, ", ADAMS")) |> 
    filter(!(race %in% c("COUNTY JUDGE - ADAMS", "COURT OF APPEALS JUDGE - STATEWIDE", 
                         "DISTRICT COURT JUDGE - 17", "SUPREME COURT JUSTICE - STATEWIDE")))
  
  # Assign unique IDs to races and candidates
  ids <- df |> 
    distinct(race, candidate) |> 
    arrange(race, candidate) |> 
    group_by(race) |> 
    mutate(candidate_id = 1:n(),
           race_id = cur_group_id())
  
  # Join back to the original data
  df <- left_join(df, ids, join_by(race, candidate))
  
  # Create the votes matrix
  votes_matrix <- df |> 
    select(cvr_id, race_id, candidate_id) |> 
    arrange(race_id, candidate_id) |>
    pivot_wider(names_from = race_id, values_from = candidate_id, values_fill = 0) |> 
    select(-cvr_id) |> 
    as.matrix()
  
  sizes <- df |> 
    distinct(race, race_id, candidate) |> 
    count(race_id) |> 
    pull(n)
  
  # Prepare data for Stan
  stan_data <- list(
    J = n_distinct(df$cvr_id),
    K = max(ids$race_id),
    C = n_distinct(ids$candidate),
    votes = votes_matrix,
    sizes = sizes
  )
  
  return(stan_data)
  
}