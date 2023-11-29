########################

library(cmdstanr)

model <- cmdstan_model("R/cat_1pl.stan", compile = FALSE)
model$compile(
  cpp_options = list(stan_threads = TRUE)
)

fit <- model$sample(
  data = targets::tar_read(stan_data),
  seed = 02139,
  parallel_chains = 4,
  threads_per_chain = 20
)

fit$save_object("fits/cat_1pl_new.rds")