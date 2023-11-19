# R/functions_fit.R

# 1PL Rasch Model with intercept and random effects for nested districts within offices
# also include random effect for individuals
fit_rasch <- function(data, binomial = TRUE){
  
  if (n_distinct(data$county_name) > 1){
    form <- bf(choice_rep ~ county_name + (1 | office/district) + (1 | cvr_id))
  } else {
    form <- bf(choice_rep ~ (1 | office/district) + (1 | cvr_id))
  }
  
  if (binomial){
    fit <- brm(
      formula = form,
      family = bernoulli(),
      data = data,
      chains = 4,
      iter = 2000,
      seed = 02139,
      silent = 0,
      control = list(adapt_delta = 0.95)
    )
  } else {
    model <- cmdstan_model("R/mnm_varying_1pl.stan", compile = FALSE)
    model$compile(
      cpp_options = list(stan_threads = TRUE)
    )
    
    # Assign unique IDs to races and candidates
    races <- data |> 
      distinct(race) |> 
      arrange(race) |> 
      mutate(race_id = row_number())
    
    candidates <- data |> 
      distinct(candidate) |> 
      arrange(candidate) |> 
      mutate(candidate_id = row_number())
    
    # Create the candidate availability matrix
    candidate_availability <- data |> 
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
    data <- data |> 
      left_join(races, by = "race") |> 
      left_join(candidates, by = "candidate")
    
    # Create the votes matrix
    votes_matrix <- data |> 
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
      J = n_distinct(data$cvr_id),
      K = max(races$race_id),
      C = max(candidates$candidate_id),
      candidates = candidate_availability,
      eligibility = eligibility_matrix,
      votes = votes_matrix
    )
    
    fit <- model$sample(
      data = stan_data,
      chains = 4,
      iter_warmup = 1000,
      iter_sampling = 1000,
      seed = 02139,
      parallel_chains = 4,
      threads_per_chain = 16 
    )
  }
  
  return(fit)
  
}

# Here, 2PL Model with intercept and random effects for nested districts within offices
# also include random effect for individuals.
# most important formulation is the addition of a discrimination parameter
fit_2pl <- function(data, binomial = TRUE){
  
  if (n_distinct(data$county_name) > 1){
    form <- bf(choice_rep ~ exp(logalpha) * eta,
               eta ~ 1 + county_name + (1 | office/district) + (1 | cvr_id),
               logalpha ~ 1 + county_name + (1 | office/district),
               nl = TRUE)
  } else {
    form <- bf(choice_rep ~ exp(logalpha) * eta,
               eta ~ 1 + (1 | office/district) + (1 | cvr_id),
               logalpha ~ 1 + (1 | office/district),
               nl = TRUE)
  }
  
  if (binomial){
    fit <- brm(
      formula = form,
      family = bernoulli(),
      data = data,
      seed = 02139,
      silent = 0,
      control = list(adapt_delta = 0.95, max_treedepth = 15)
    )
  } else {
    model <- cmdstan_model("R/mnm_varying_2pl_optimized.stan", compile = FALSE)
    model$compile(
      cpp_options = list(stan_threads = TRUE)
    )
    
    # Assign unique IDs to races and candidates
    races <- data |> 
      distinct(race) |> 
      arrange(race) |> 
      mutate(race_id = row_number())
    
    candidates <- data |> 
      distinct(candidate) |> 
      arrange(candidate) |> 
      mutate(candidate_id = row_number())
    
    # Create the candidate availability matrix
    candidate_availability <- data |> 
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
    data <- data |> 
      left_join(races, by = "race") |> 
      left_join(candidates, by = "candidate")
    
    # Create the votes matrix
    votes_matrix <- data |> 
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
      J = n_distinct(data$cvr_id),
      K = max(races$race_id),
      C = max(candidates$candidate_id),
      candidates = candidate_availability,
      eligibility = eligibility_matrix,
      votes = votes_matrix
    )
    
    fit <- model$sample(
      data = stan_data,
      chains = 4,
      iter_warmup = 1000,
      iter_sampling = 1000,
      seed = 02139,
      parallel_chains = 4,
      threads_per_chain = 16 
    )
  }
  
  return(fit)
}