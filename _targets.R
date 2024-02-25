library(targets)
library(tarchetypes)

source("R/functions_data.R")
source("R/functions_fit.R")
source("R/functions_plot.R")

options("brms.threads" = 20)
options("mc.cores" = 4)
options("brms.backend" = "cmdstanr")
options("future" = FALSE)

tar_option_set(packages = c("tidyverse", "brms", "arrow", "tidybayes", "bayesplot", "cmdstanr"),
               memory = "transient", 
               format = "qs",
               garbage_collection = TRUE
)
tar_config_set(seconds_meta_append=15, 
               seconds_reporter=0.5
               )
list(
  tar_target(cvr_path, "~/cvrs/data/cvr_qa_main/"),
  # tar_target(cvr_path, "~/Dropbox (MIT)/Research/cvrs/data/cvr_qa_main/"),
  tar_target(data_base, get_data(path = cvr_path, st = "COLORADO", num=100000)),
  tar_target(data_colorado, get_data(path = cvr_path, st = "COLORADO", partisan_only = TRUE, num=100000)),
  tar_target(data_base_adams, filter_byCounty(data=data_base, county="ADAMS")),
  # tar_target(data_grouped_bernoulli, group_voters(data_colorado, categorical = FALSE)),
  # tar_target(data_grouped_categorical, group_voters(data_base_adams, categorical = TRUE)),
  tar_target(fit_2pl_binomial_partisans_colorado, fit_bernoulli(data_colorado, type = "2pl")),
  tar_target(fit_rasch_binomial_partisans_colorado, fit_bernoulli(data_colorado, type = "rasch")),
  tar_target(stan_data, get_stan_data(data_base_adams)),
  tar_target(stan_2pl_code, "R/cat_2pl_streamlined.stan", format = "file"),
  tar_target(fit_2pl_categorical_colorado, fit_stan(stan_2pl_code, stan_data, "cat_2pl_streamlined"))
)
