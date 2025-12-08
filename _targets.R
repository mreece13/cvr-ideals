library(targets)

source("R/functions_data.R")
source("R/functions_fit.R")
# source("R/functions_plot.R")

options("brms.threads" = 20)
options("mc.cores" = 4)
options("brms.backend" = "cmdstanr")
options("future" = FALSE)

tar_option_set(
  packages = c("tidyverse", "brms", "arrow", "tidybayes", "cmdstanr", "posterior", "glue"),
  memory = "transient",
  error = "null",
  garbage_collection = TRUE
)

username <- Sys.info()["user"]
if (username == "mason") {
  PATH_PARQ = "../cvrs/data/pass3"
} else if (username == "mpreece") {
  PATH_PARQ = "~/orcd/pool/supercloud-cvrs/data/pass3"
}

list(
  # prep data
  tar_target(compare, readxl::read_excel("data/compare.xlsx", sheet = "by-county") |> filter(release == 1) |> distinct(state, county_name)),
  tar_target(data, get_data(path = PATH_PARQ, st = "COLORADO", num = 100000, compare = compare), format = "parquet"),
  # tar_target(data_partisan, get_data(path = PATH_PARQ, st = "COLORADO", partisan_only = TRUE, num = 100000)),
  tar_target(data_adams, filter_byCounty(data, county = "ADAMS"), format = "parquet"),
  # format data for Stan
  tar_target(stan_data, get_stan_data(data, ragged=TRUE)),
  tar_target(stan_data_adams, get_stan_data(data_adams, ragged=TRUE)),
  # fit `brms` models
  # tar_target(fit_bin_1pl, fit_bernoulli(data_partisan, type = "1pl"), format = "file"),
  # tar_target(fit_bin_2pl, fit_bernoulli(data_partisan, type = "2pl"), format = "file"),
  # fit stan models
  # tar_target(stan_2pl, "R/cat_2pl.stan", format = "file"),
  tar_target(stan_gpu, "R/irt_gpu.stan", format = "file"),
  # tar_target(fit_cat2pl_adams, fit_stan(stan_gpu, stan_data_adams, "irt_gpu"), format = "file"),
  tar_target(fit_cat2pl_var_adams, fit_stan(stan_2pl, stan_data_adams, "irt_gpu", variational = TRUE), format = "file"),
  # tar_target(fit_cat2pl_var, fit_stan(stan_2pl, stan_data, "irt_gpu", variational = TRUE), format = "file"),
  tar_target(fit_gpu_adams, fit_stan(stan_gpu, stan_data_adams, "irt_gpu", gpu = TRUE), format = "file")
  # plots
  # tar_target(p_ber_ideals, plot_ber_ideals(fit_bin_1pl, fit_bin_2pl)),
  # tar_target(p_ber_params, plot_ber_params(fit_bin_2pl)),
  # tar_target(p_traces, plot_traces(fit_bin_1pl, fit_bin_2pl, fit_cat2pl_adams))
  # tar_target(p_rhats, plot_rhats(fit_bin_1pl, fit_bin_2pl, fit_cat2pl_adams))
)
