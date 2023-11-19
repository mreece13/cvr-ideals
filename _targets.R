library(targets)
library(tarchetypes)

source("R/functions_data.R")
source("R/functions_fit.R")
source("R/functions_plot.R")

options("brms.threads" = 16)
options("mc.cores" = 4)
options("brms.backend" = "cmdstanr")
options("future" = FALSE)

tar_option_set(packages = c("tidyverse", "brms", "arrow", "tidybayes", "bayesplot", "cmdstanr"),
               memory = "transient", 
               garbage_collection = TRUE)
tar_config_set(seconds_meta_append=15, 
               seconds_reporter=0.5
               )
list(
  tar_target(cvr_path, "~/cvrs/data/cvr_qa_main/"),
  # tar_target(cvr_path, "~/Dropbox (MIT)/Research/hidden-partisanship/data/cvr_qa_main/"),
  tar_target(data_base, get_data(path = cvr_path, st = "COLORADO", num=25000)),
  tar_target(data_colorado, get_data(path = cvr_path, st = "COLORADO", partisan_only = TRUE, num=25000)),
  tar_target(data_colorado_adams, filter_byCounty(data=data_colorado, county="ADAMS")),
  tar_target(data_base_adams, filter_byCounty(data=data_base, county="ADAMS")),
  tar_target(randos_colorado, pick_random_voters(data_colorado, 15)),
  tar_target(randos_colorado_adams, pick_random_voters(data_colorado_adams, 10)),
  tar_target(fit_rasch_binomial_partisans_colorado_adams, 
             fit_rasch(data_colorado_adams, binomial = TRUE),
             format = "qs"),
  tar_target(fit_2pl_binomial_partisans_colorado_adams, 
             fit_2pl(data_colorado_adams, binomial = TRUE),
             format = "qs"),
  tar_target(fit_rasch_binomial_partisans_colorado,
             fit_rasch(data_colorado, binomial = TRUE),
             format = "qs"),
  tar_target(fit_rasch_categorical_colorado, 
             fit_rasch(data_base_adams, binomial = FALSE),
             format = "qs"),
  tar_target(fit_2pl_categorical_colorado, 
             fit_2pl(data_base_adams, binomial = FALSE),
             format = "qs"),
  tar_target(plot_rasch_binomial_partisans_colorado_adams_voters, 
             plot_voters(fit_rasch_binomial_partisans_colorado_adams, randos_colorado_adams, twopl = FALSE)),
  tar_target(plot_2pl_binomial_partisans_colorado_adams_voters, 
             plot_voters(fit_2pl_binomial_partisans_colorado_adams, randos_colorado_adams, twopl = TRUE)),
  tar_target(plot_rasch_binomial_partisans_colorado_voters, 
             plot_voters(fit_rasch_binomial_partisans_colorado, randos_colorado, twopl = FALSE)),
  tar_target(plot_2pl_binomial_partisans_colorado_adams_offices, plot_offices(fit_2pl_binomial_partisans_colorado_adams)),
  tar_target(plot_rhat_comparison, 
             plot_rhats(list("Partisans, Binomial, Rasch -- Adams County, Colorado" = fit_rasch_binomial_partisans_colorado_adams, 
                             "Partisans, Binomial, 2PL -- Adams County, Colorado" = fit_2pl_binomial_partisans_colorado_adams,
                             "Partisans, Binomial, Rasch -- Colorado" = fit_rasch_binomial_partisans_colorado,
                             "Partisans, Categorical, Rasch -- Adams County, Colorado" = fit_rasch_categorical_colorado, 
                             "Partisans, Categorical, 2PL -- Adams County, Colorado" = fit_2pl_categorical_colorado)))
  # tar_quarto(ideals_paper, "ideals_paper.qmd", quiet = FALSE)
)
