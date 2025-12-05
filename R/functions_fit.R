# R/functions_fit.R

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
    file = str_c("fits/bernoulli_", type),
    file_refit = "on_change"
  )
  
  str_c("fits/bernoulli_", type)
  
}

fit_stan <- function(model, stan_data, file_name, variational = FALSE, checkpoint = TRUE){
  
  print(str_c("Total voters in this data: ", as.character(stan_data$N_voters)))
  
  if (variational) {
    m <- cmdstan_model(str_c("R/", file_name, ".stan"), compile = FALSE)
    m$compile(cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)

    stan_data$threaded = 1

    fit <- m$pathfinder(
      data = stan_data,
      seed = 02139,
      num_threads = 20
    )

    path <- str_c("fits/", file_name, "_numV", as.character(stan_data$N_voters), "_var.rds")
  } else {
    path <- str_c("fits/", file_name, "_numV", as.character(stan_data$N_voters), "_full.rds")

    if (checkpoint) {

      fit = chkpt_stan(
        model_code = readLines(str_c("R/", file_name, ".stan")),
        data = stan_data,
        iter_warmup = 1000,
        iter_sampling = 1000,
        iter_per_chkpt = 100,
        path = "checkpoints",
        parallel_chains = 4,
        threads_per = 20,
        seed = 02139
      )

      draws = combine_chkpt_draws(fit)

      write_rds(draws, path)

    } else {
      m <- cmdstan_model(str_c("R/", file_name, ".stan"), compile = FALSE)
      m$compile(cpp_options = list(stan_threads = TRUE), force_recompile = FALSE)

      fit <- m$sample(
        data = stan_data,
        chains = 4,
        iter_warmup = 1000,
        iter_sampling = 1000,
        seed = 02139,
        parallel_chains = 4,
        threads_per_chain = 20
      )
    }

    fit$save_object(path)
  }
  
  return(path)
}