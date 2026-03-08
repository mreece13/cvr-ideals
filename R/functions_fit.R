
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

# to enable GPU support on slurm, I did (roughly) the following:
# mamba install clinfo ocl-icd-system
# module load cuda
# cat $LD_LIBRARY_PATH
# path_to_opencl_lib <- $LD_LIBRARY_PATH
# cpp_options = list(
#   paste0("LDFLAGS+= -L\"",path_to_opencl_lib,"\" -lOpenCL")
# )

# cmdstanr::cmdstan_make_local(cpp_options = cpp_options)
# cmdstanr::rebuild_cmdstan()

fit_stan <- function(model, stan_data, file_name, variational = FALSE, gpu = FALSE) {
  print(glue("Total voters in this data: {stan_data$N_voters}"))

  m <- cmdstan_model(glue("R/{file_name}.stan"), compile = FALSE)
  m$compile(cpp_options = list(stan_threads = TRUE, stan_opencl = gpu))

  fit_pathfinder <- m$pathfinder(
    data = stan_data,
    seed = 02139,
    num_threads = 20,
    psis_resample = FALSE
  )

  path <- glue("fits/{file_name}_numV{stan_data$N_voters}_var.rds")

  fit_pathfinder$save_object(path)

  if (variational) {
    return(path)
  }

  path <- glue("fits/{file_name}_numV{stan_data$N_voters}_full.rds")

  fit_mcmc <- m$sample(
    data = stan_data,
    chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    seed = 02139,
    parallel_chains = 4,
    threads_per_chain = if (gpu) 1 else 20,
    init = fit_pathfinder
  )

  fit_mcmc$save_object(path)

  return(path)
}