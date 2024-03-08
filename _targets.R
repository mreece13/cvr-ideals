library(targets)

source("R/functions_data.R")
source("R/functions_fit.R")

options("brms.threads" = 20)
options("mc.cores" = 4)
options("brms.backend" = "cmdstanr")
options("future" = FALSE)

tar_option_set(packages = c("tidyverse", "brms", "arrow", "tidybayes", "bayesplot", "cmdstanr"),
               memory = "transient", 
               format = "qs",
               error = "null",
               garbage_collection = TRUE
)
tar_config_set(seconds_meta_append=15, 
               seconds_reporter=0.5
               )
list(
  # home directory for data
  tar_target(cvr_path, "../cvrs_shared/data/pass1/"),
  # prep data
  tar_target(data, get_data(path = cvr_path, st = "COLORADO", num=100000)),
  tar_target(data_partisan, get_data(path = cvr_path, st = "COLORADO", partisan_only = TRUE, num=100000)),
  tar_target(data_adams, filter_byCounty(data, county="ADAMS")),
  # format data for Stan
  tar_target(stan_data, get_stan_data(data)),
  tar_target(stan_data_adams, get_stan_data(data_adams)),
  # fit `brms` models
  tar_target(fit_bin_2pl, fit_bernoulli(data_partisan, type = "2pl")),
  tar_target(fit_bin_1pl, fit_bernoulli(data_partisan, type = "1pl")),
  # fit stan models
  tar_target(stan_2pl, "R/cat_2pl_streamlined.stan", format = "file"),
  tar_target(fit_cat2pl_adams, fit_stan(stan_2pl, stan_data_adams, "cat_2pl_streamlined")),
  tar_target(fit_cat2pl_var_adams, fit_stan(stan_2pl, stan_data_adams, "cat_2pl_streamlined", variational = TRUE)),
  tar_target(fit_cat2pl_var, fit_stan(stan_2pl, stan_data, "cat_2pl_streamlined", variational = TRUE)),
  # plots
  tar_target(p_ber_ideals, plot_ber_ideals(fit_bin_1pl, fit_bin_2pl)),
  tar_target(p_ber_params, plot_ber_params(fit_bin_2pl)),
  tar_target(p_traces, plot_traces(fit_bin_1pl, fit_bin_2pl, fit_cat2pl_adams)),
  tar_target(p_rhats, plot_rhats(fit_bin_1pl, fit_bin_2pl, fit_cat2pl_adams))
)
