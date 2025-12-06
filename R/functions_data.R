# R/functions_data.R

###########
## @param path -- the base path to the data
## @param st -- the full name of the state to extract, or "all" to get all of them
## @param partisan -- should the data to be filtered to partisan races only?
## @param num -- how many voters should be sampled? If left blank, selects everyone
get_data <- function(path, st, partisan_only = FALSE, num = 1e9, compare){
  
  # define only the variables I need to run the analysis, to limit the size of the loaded data
  partial_schema <- schema(
    field("state", string()),
    field("precinct", string()),
    field("cvr_id", string()),
    field("county_name", string()),
    field("candidate", string()),
    field("district", string()),
    field("magnitude", int8()),
    field("office", string()),
    field("party", string())
  )
  
  base_data <- open_dataset(path, partitioning = c("state", "county_name"), schema = partial_schema, format = "parquet") |>
    filter(state == st, magnitude == 1, !is.na(office), !str_detect(candidate, regex("undervote|overvote|writein", TRUE))) |>
    semi_join(compare, join_by(state, county_name)) |>
    select(-magnitude) |>
    mutate(
      race = paste(office, district, sep = "_")
    )
  
  # pick random precincts to get good coverage
  random_precincts <- base_data |> 
    distinct(state, county_name, precinct) |> 
    collect() |> 
    slice_sample(prop = 0.1, by = county_name)

  # pick some random people
  randoms <- base_data |>
    inner_join(random_precincts, join_by(state, county_name, precinct)) |>
    distinct(state, county_name, precinct, cvr_id) |>
    collect() |> 
    slice_sample(n=500, by = c(county_name, precinct)) |> 
    distinct(state, county_name, cvr_id)
  
  # What are the contested races?
  small_candidates <- base_data |> 
    inner_join(randoms, join_by(state, county_name, cvr_id)) |>
    # inner_join(random_precincts, join_by(state, county_name, precinct)) |>
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
      filter(!str_detect(party, regex("nonpartisan", TRUE))) |> 
      distinct(race)
    
    contested_races <- inner_join(contested_races, partisan_races)
  }
  
  base_data |> 
    inner_join(randoms, join_by(state, county_name, cvr_id)) |>
    # inner_join(random_precincts, join_by(state, county_name, precinct)) |>
    inner_join(contested_races, join_by(race)) |> 
    # mutate(choice_rep = as.numeric(party == "republican" | party == "REPUBLICAN")) |> 
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

get_stan_data <- function(data, dims = 1, parallelize = FALSE) {
  # Assign unique IDs to races and candidates
  ids <- data |>
    count(race, candidate) |>
    arrange(race, desc(n)) |>
    mutate(candidate_id = 1:n(), race_id = cur_group_id(), .by = race)

  # Join back to the original data
  data <- left_join(data, ids, join_by(race, candidate))

  # Create the votes matrix
  votes_matrix <- data |>
    select(county_name, cvr_id, race_id, candidate_id, n) |>
    arrange(race_id, desc(n)) |>
    select(-n) |>
    pivot_wider(names_from = race_id, values_from = candidate_id, values_fill = 0) |>
    select(-cvr_id, -county_name) |>
    as.matrix()

  # Prepare data for Stan
  stan_data <- list(
    N_voters = distinct(data, county_name, cvr_id) |> nrow(),
    N_contests = n_distinct(ids$race),
    N_cands = length(ids$candidate),
    votes = votes_matrix,
    sizes = distinct(data, race, race_id, candidate) |> count(race_id) |> pull(n),
    parallelize = as.numeric(parallelize)
  )

  if (dims > 1) {
    stan_data$N_dims = dims
  }

  return(stan_data)
}