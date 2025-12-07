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

get_stan_data <- function(data, dims = 1, parallelize = FALSE, ragged = FALSE) {
  
  if (ragged) {
    # For ragged format: Democrats get ID=1 (first/reference), others get sequential IDs
    # This ensures Democrat is always the reference category in categorical_logit
    ids <- data |>
      distinct(race, candidate, party) |>
      mutate(
        party = ifelse(str_detect(race, "^prop_"), NA, party)
      ) |> 
      distinct(race, candidate, party) |> 
      slice_head(n=1, by = c(race, candidate)) |> 
      arrange(race, party != "democrat", candidate) |>  # Democrats first within each race
      mutate(
        candidate_id = row_number(),  # Sequential IDs with Democrat=1
        race_id = cur_group_id(),
        .by = race
      ) |> 
      select(-party)
    
    # Create voter ID mapping
    voter_map <- data |>
      distinct(county_name, cvr_id) |>
      arrange(county_name, cvr_id) |>
      mutate(voter_id = row_number())
    
    # Create ragged arrays organized by contest
    votes_long <- data |>
      left_join(ids, join_by(race, candidate)) |> 
      left_join(voter_map, join_by(county_name, cvr_id)) |>
      select(voter_id, race_id, candidate_id) |>
      arrange(race_id, voter_id)  # Sort by contest, then voter
    
    # Count votes per contest
    n_votes_per_contest <- votes_long |>
      count(race_id) |>
      arrange(race_id) |>
      pull(n)
    
    # Prepare data for Stan (ragged format)
    stan_data <- list(
      N_voters = nrow(voter_map),
      N_contests = n_distinct(ids$race),
      N_cands = length(ids$candidate),
      N_votes = nrow(votes_long),  # Total number of actual votes
      n_votes_per_contest = n_votes_per_contest,
      voter_id = votes_long$voter_id,  # Flat array of voter IDs
      vote = votes_long$candidate_id,   # Flat array of candidate choices
      sizes = distinct(ids, race, race_id, candidate) |> count(race_id) |> pull(n)
    )
  } else {
    # For dense format: Democrats get ID=1 (first/reference), others get sequential IDs
    # Same as ragged format, just different storage structure
    # 0 is used to indicate "didn't vote in this race"
    ids <- data |>
      distinct(race, candidate, party) |>
      mutate(
        party = ifelse(str_detect(race, "^prop_"), NA, party)
      ) |> 
      distinct(race, candidate, party) |> 
      slice_head(n=1, by = c(race, candidate)) |> 
      arrange(race, party != "democrat", candidate) |>  # Democrats first within each race
      mutate(
        candidate_id = row_number(),  # Sequential IDs with Democrat=1
        race_id = cur_group_id(),
        .by = race
      ) |> 
      select(-party)
    
    # Create the dense votes matrix
    votes_matrix <- data |>
      left_join(ids, join_by(race, candidate)) |> 
      select(county_name, cvr_id, race_id, candidate_id) |>
      arrange(race_id, candidate_id) |>
      pivot_wider(names_from = race_id, values_from = candidate_id, values_fill = 0) |>
      select(-cvr_id, -county_name) |>
      as.matrix()

    # Prepare data for Stan (dense format)
    stan_data <- list(
      N_voters = distinct(data, county_name, cvr_id) |> nrow(),
      N_contests = n_distinct(ids$race),
      N_cands = length(ids$candidate),
      votes = votes_matrix,
      sizes = distinct(ids, race, race_id, candidate) |> count(race_id) |> pull(n),
      parallelize = as.numeric(parallelize)
    )
  }

  if (dims > 1) {
    stan_data$N_dims = dims
  }

  return(stan_data)
}