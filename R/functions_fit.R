
fit_bernoulli <- function(data, type){
  
  if (type == "1pl"){
    form <- bf(
      choice_rep ~ 1 + (1 | race) + (1 | cvr_id),
      family = brmsfamily("bernoulli", link = "logit")
    )
  }
  
  if (type == "2pl"){
    form <- bf(
      choice_rep ~ exp(loggamma) * alpha - beta,
      nl = TRUE,
      alpha ~ 0 + (1 | cvr_id),
      beta ~ 1 + (1 | race),
      loggamma ~ 0 + (1 | race),
      family = brmsfamily("bernoulli", link = "logit")
    )
  }
  
  brm(
    formula = form,
    data = data,
    chains = 4,
    iter = 2000,
    seed = 02139,
    silent = 0,
    file = glue("fits/bernoulli_{type}"),
    file_refit = "on_change"
  )
  
  glue("fits/bernoulli_{type}")
  
}

fit_stan <- function(model, stan_data, file_name, variational = FALSE, gpu = FALSE){
  
  print(glue("Total voters in this data: {stan_data$N_voters}"))
  
  if (variational) {
    m <- cmdstan_model(glue("R/{file_name}.stan"), compile = FALSE)
    m$compile(cpp_options = list(stan_threads = TRUE, stan_opencl = gpu))

    fit <- m$pathfinder(
      data = stan_data,
      seed = 02139,
      num_threads = 20
    )

    path <- glue("fits/{file_name}_numV{stan_data$N_voters}_var.rds")

    fit$save_object(path)
  } else {
    path <- glue("fits/{file_name}_numV{stan_data$N_voters}_full.rds")

    m <- cmdstan_model(glue("R/{file_name}.stan"), compile = FALSE)
    m$compile(cpp_options = list(stan_threads = !gpu, stan_opencl = gpu))

    fit <- m$sample(
      data = stan_data,
      chains = 4,
      iter_warmup = 1000,
      iter_sampling = 1000,
      seed = 02139,
      parallel_chains = 4,
      threads_per_chain = if (gpu) 1 else 20
    )

    fit$save_object(path)
  }
  
  return(path)
}