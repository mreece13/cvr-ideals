#### TESTING FILE ONLY

library(tidyverse)
library(arrow)
library(brms)

rm(list=ls())
gc()

# define only the variables I need to run the analysis, to limit the size of the loaded data
partial_schema <- schema(
  field("state", string()),
  field("cvr_id", string()),
  field("county_name", string()),
  field("candidate", string()),
  field("district", string()),
  field("magnitude", string()),
  field("office", string())
)

unique_races <- open_dataset("~/Dropbox (MIT)/Research/hidden-partisanship/data/cvr_qa_main/",
                             partitioning = "state",
                             schema = partial_schema,
                             format = "parquet") |> 
  filter(state == "COLORADO", magnitude == "1", county_name == "TELLER") |> 
  distinct(office, district) |> 
  mutate(choice = 1) |> 
  collect()

data_raw <- open_dataset("~/Dropbox (MIT)/Research/hidden-partisanship/data/cvr_qa_main/",
                  partitioning = "state",
                  schema = partial_schema,
                  format = "parquet") |> 
  filter(state == "COLORADO", magnitude == "1", !is.na(office), !is.na(district)) |> 
  select(-magnitude) |> 
  filter(as.numeric(cvr_id) %% 25000 == 0) |>
  collect()

data_simplified <- data_raw |> 
  complete(cvr_id, nesting(office, district)) |> 
  mutate(possible_race = if_else(is.na(state), 0, 1)) |> 
  mutate(district = case_when(
    office %in% c("US HOUSE", "STATE HOUSE", "PROPOSITION") ~ str_remove(district, str_c(", ", county_name)),
    .default = district
  )) |> 
  mutate(race = str_c(office, district, sep = " -- ")) |>
  select(county_name, cvr_id, office, district, candidate, possible_race, race) |> 
  drop_na(office)

fit <- brm(
  formula = candidate ~ 1 + office/district + (1 | cvr_id),
  family = categorical(),
  data = d,
  backend = "cmdstanr",
  chains = 4,
  # threads = threading(threads = 12),
  cores = 4,
  seed = 02139,
  file = "fits/categorical_1pl_dummies",
  file_refit = "on_change",
  silent = 0
)

# plot trace plots
p_trace <- bayesplot::mcmc_trace(fit, regex_pars = "^r_officeA*")
ggsave("figs/categorical_1pl_trace.jpeg", plot = p_trace, width = 16, height = 16, units = "in")

# plot figures from the model
p1 <- ranef(fit, summary = FALSE)$office |> as_tibble() |>
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "estimate") |>
  mutate(parameter = str_remove(parameter, fixed(".Intercept"))) |>
  ggplot(aes(x = estimate, y = parameter)) +
  ggridges::geom_density_ridges() +
  theme_bw()

ggsave("figs/categorical_1pl_office.jpeg", plot = p1, width = 8, height = 12, units = "in")

p_1pl_logit_voters <- ranef(fit, summary = FALSE)$cvr_id |> as_tibble() |>
  select(1:15) |>
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "estimate") |>
  mutate(parameter = str_remove(parameter, fixed(".Intercept"))) |>
  ggplot(aes(x = estimate, y = parameter)) +
  ggridges::geom_density_ridges() +
  theme_bw()

ggsave("figs/categorical_1pl_voters.jpeg", plot = p_1pl_logit_voters, width = 8, height = 12, units = "in")