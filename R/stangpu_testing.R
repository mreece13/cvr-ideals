rm(list=ls())
gc()

library(tidyverse)
library(cmdstanr)

data <- targets::tar_read(data_adams)
stan_data = targets::tar_read(stan_data_adams)

m <- cmdstan_model("R/cat_2pl_gpu.stan", compile = FALSE)
m$compile(cpp_options = list(stan_opencl = TRUE), force_recompile = FALSE)

fit <- m$sample(
  data = stan_data,
  chains = 2,
  iter_warmup = 500,
  iter_sampling = 100,
  opencl_ids = c(0, 0)
)

fit$save_object("fits/cat_2pl_gpu.rds")