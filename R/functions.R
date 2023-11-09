# R/functions.R
get_cvrs <- function(base_path){
  
  partial_schema <- schema(
    field("state", string()),
    field("cvr_id", string()),
    field("county_name", string()),
    field("candidate", string()),
    field("district", string()),
    field("magnitude", string()),
    field("office", string())
  )
  
  open_dataset(base_path,
               partitioning = "state",
               schema = partial_schema,
               format = "parquet") |> 
    filter(state == "COLORADO", magnitude == 1) |> 
    select(-magnitude) |> 
    collect() |> 
    group_by(county_name) |> 
    nest() |> 
    mutate(data = map(data, ~ sample_individuals(.x, 25))) |> 
    unnest(cols = c(data)) |> 
    ungroup()
}

get_data_diagnostics <- function(data){
  people <- distinct(data, county_name, cvr_id) |> tally()
  counties <- distinct(data, county_name)
  offices <- distinct(data, office)
  candidates <- distinct(data, office, district, candidate)
  
  return(list("people" = people, 
              "counties" = counties, 
              "offices" = offices, 
              "candidates" = candidates))
}

fit_simple_model <- function(data){
  
  
  
}

plot_diagnostics <- function(model){
  
  
}