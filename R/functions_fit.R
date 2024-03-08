# R/functions_fit.R

fit_bernoulli <- function(data, type){
  
  if (type == "1pl"){
    form <- bf(
      choice_rep ~ 1 + (1 | race) + (1 | cvr_id),
      family = brmsfamily("bernoulli", link = "logit")
    )
    
    priors <-
      prior("normal(0, 2)", class = "Intercept") +
      prior("normal(0, 3)", class = "sd", group = "cvr_id") +
      prior("normal(0, 3)", class = "sd", group = "race")
  }
  
  if (type == "2pl"){
    form <- bf(
      choice_rep ~ exp(loggamma) * alpha - beta,
      nl = TRUE,
      alpha ~ 0 + (1 | cvr_id),
      beta ~ 1 + (1 |i| race),
      loggamma ~ 1 + (1 |i| race),
      family = brmsfamily("bernoulli", link = "logit")
    )
    
    priors <-
      prior("normal(0, 2)", class = "b", nlpar = "beta") +
      prior("normal(0, 1)", class = "b", nlpar = "loggamma") +
      prior("normal(0, 1)", class = "sd", group = "cvr_id", nlpar = "alpha") +
      prior("normal(0, 3)", class = "sd", group = "race", nlpar = "beta") +
      prior("normal(0, 1)", class = "sd", group = "race", nlpar = "loggamma")
  }
  
  brm(
    formula = form,
    prior = priors,
    data = data,
    chains = 4,
    iter = 2000,
    seed = 02139,
    silent = 0,
    file = str_c("fits/bernoulli_", type),
    file_refit = "on_change"
  )
  
}

fit_stan <- function(model, stan_data, file_name, variational = FALSE){
  
  print(str_c("Total voters in this data: ", as.character(stan_data$J)))
  
  if (variational){
    m <- cmdstan_model(str_c("R/", file_name, ".stan"), compile = FALSE)
    m$compile(cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)
    
    fit <- m$pathfinder(
      data = stan_data,
      seed = 02139,
      num_threads = 5
    )
    
    path <- str_c("fits/", file_name, "numV", as.character(stan_data$J), "_var.rds")
    
  } else {
    m <- cmdstan_model(str_c("R/", file_name, ".stan"), compile = TRUE)
    
    fit <- m$sample(
      data = stan_data,
      chains = 4,
      iter_warmup = 1000,
      iter_sampling = 1000,
      seed = 02139,
      parallel_chains = 4
      # threads_per_chain = 20
    )
    
    path <- str_c("fits/", file_name, "numV", as.character(stan_data$J), "_full.rds")
  }
  
  fit$save_object(path)
  
  return(path)
}