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
                              district))
}

filter_byCounty <- function(data, county){
  filter(data, county_name == county)
}

pick_random_voters <- function(data, n){
  slice_sample(distinct(data, cvr_id), n=n)
}