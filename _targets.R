library(targets)
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "brms", "arrow"))
list(
  tar_target(cvr_path, "~/Dropbox (MIT)/Research/hidden-partisanship/data/cvr_qa_district/"),
  tar_target(data_cvr, get_cvrs(cvr_path)),
  tar_target(data_diagnostics, get_data_diagnostics(data_cvr))
  tar_target(simple_model, fit_simple_model(data_cvr)),
  tar_target(plot_diagnostics, plot_diagnostics(model))
)