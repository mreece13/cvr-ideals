library(targets)

source("R/functions_data.R")
source("R/functions_fit.R")
source("R/functions_plot.R")

options("brms.threads" = NULL)
options("mc.cores" = 4)
options("brms.backend" = "cmdstanr")
options("future" = FALSE)

tar_option_set(packages = c("tidyverse", "brms", "arrow", "tidybayes", "bayesplot"),
               memory = "transient", 
               garbage_collection = TRUE)
tar_config_set(seconds_meta_append=15, 
               seconds_reporter=0.5
               )
list(
  tar_target(cvr_path, "~/Dropbox (MIT)/Research/hidden-partisanship/data/cvr_qa_main/"),
  tar_target(data_colorado, get_data_partisans(path = cvr_path, st = "COLORADO", n=250)),
  tar_target(data_colorado_adams, filter_byCounty(data=data_colorado, county="ADAMS")),
  tar_target(randos_colorado, pick_random_voters(data_colorado, 15)),
  tar_target(randos_colorado_adams, pick_random_voters(data_colorado_adams, 10)),
  tar_target(fit_rasch_binomial_partisans_colorado_adams, 
             fit_rasch(data_colorado_adams, binomial = TRUE),
             format = "qs"),
  tar_target(fit_2pl_binomial_partisans_colorado_adams, 
             fit_2pl(data_colorado_adams, binomial = TRUE),
             format = "qs"),
  tar_target(fit_rasch_binomial_partisans_colorado,
             fit_rasch(data_colorado, binomial = TRUE)),
  tar_target(plot_rasch_binomial_partisans_colorado_adams_trace, plot_trace(fit_rasch_binomial_partisans_colorado_adams)),
  tar_target(plot_2pl_binomial_partisans_colorado_adams_trace, plot_trace(fit_2pl_binomial_partisans_colorado_adams)),
  tar_target(plot_rasch_binomial_partisans_colorado_trace, plot_trace(fit_rasch_binomial_partisans_colorado)),
  tar_target(plot_rasch_binomial_partisans_colorado_adams_voters, plot_voters(fit_rasch_binomial_partisans_colorado_adams)),
  tar_target(plot_2pl_binomial_partisans_colorado_adams_voters, plot_voters(fit_2pl_binomial_partisans_colorado_adams)),
  tar_target(plot_rasch_binomial_partisans_colorado_voters, plot_voters(fit_rasch_binomial_partisans_colorado)),
  tar_target(plot_rasch_binomial_partisans_colorado_adams_offices, plot_offices(fit_rasch_binomial_partisans_colorado_adams)),
  tar_target(plot_2pl_binomial_partisans_colorado_adams_offices, plot_offices(fit_2pl_binomial_partisans_colorado_adams)),
  tar_target(plot_rasch_binomial_partisans_colorado_offices, plot_offices(fit_rasch_binomial_partisans_colorado))
)