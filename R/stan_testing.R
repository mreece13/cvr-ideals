rm(list=ls())
gc()

library(tidyverse)
library(tidybayes)
library(bayesplot)
library(cmdstanr)
library(posterior)

source("../medsl_theme.R")

data <- targets::tar_read(data) |> 
  mutate(
    pres_choice = first(candidate[office == "us president"]),
    .by = c(state, county_name, cvr_id)
  )

stan_data = targets::tar_read(stan_data)
fit <- read_rds("fits/cat_2pl_numV118109_var.rds")

## graphing

vars <- fit |>
  as_draws_df() |>
  summarise_draws() |>
  filter(mean > 0.1 | mean < -0.1, !str_detect(variable, "raw")) |>
  slice_sample(n = 24) |>
  pull(variable)
  
trace_3 <- fit |> 
  as_draws_df() |> 
  mcmc_trace(pars = vars)

joined <- as_draws_df(fit) |> 
  # slice_head(n=1) |> 
  select(starts_with("alpha")) |> 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "alpha") |> 
  mutate(id = str_extract(variable, "\\d+") |> as.integer()) |> 
  left_join(
    distinct(data, county_name, precinct, cvr_id, pres_choice) |> 
      mutate(id = row_number()) |> select(county_name, id, pres_choice),
    by = "id"
  ) |> 
  select(county_name, alpha, pres_choice)

p_agg_main <- joined |> 
  # slice_head(n=100000) |> 
  filter(pres_choice %in% c("joseph r biden", "donald j trump")) |> 
  ggplot(aes(x = alpha, y = pres_choice, fill = pres_choice)) +
  ggdist::stat_slab() +
  scale_fill_discrete(type = c("#F6573E", "#3791FF"), guide = "none") +
  labs(x = expression(alpha), y = "", fill = "Candidate") +
  theme_minimal()

ggsave("figs/var_preschoices.jpg", plot = p_agg_main, width = 8, height = 6, units = "in")

p_agg_others <- joined |> 
  filter(!(candidate %in% c("JOSEPH R BIDEN", "DONALD J TRUMP"))) |> 
  filter(n() > 5, .by = candidate) |> 
  # mutate(alpha = alpha*-1) |> 
  ggplot(aes(x = alpha, y = candidate)) +
  ggridges::geom_density_ridges(fill = "#C0BA79", scale = 1, panel_scaling = FALSE) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = expression(alpha), y = "") +
  theme_medsl()

ggsave("figs/cat_aggregated_var.jpeg", plot = p_agg_main, width = 10, height = 6, units = "in")
ggsave("figs/cat_aggregated_others_var.jpg", plot = p_agg_others, width = 10, height = 6, units = "in")

sds <- fit |> 
  as_draws_df() |> 
  slice_head(n=1) |> 
  select(starts_with("alpha")) |> 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "alpha") |> 
  pull(alpha) |> 
  sd()

params <- fit |> 
  posterior::as_draws_df() |> 
  slice_head(n=1) |> 
  select(starts_with("diff["), starts_with("disc[")) |> 
  # sweep(1, sds, "*") |> 
  pivot_longer(cols = everything()) |> 
  bind_cols(bind_rows(ids, ids)) |> 
  mutate(name = str_remove(name, "\\[.*?$")) |> 
  select(name, race, candidate, value)

p <- params |> 
  filter(str_detect(race, "us president")) |> 
  ggplot(aes(x = value, y = candidate)) +
  geom_point() +
  facet_wrap(~ name) +
  theme_bw(base_size=5)

ggsave("figs/params_var.jpeg", plot = p, width = 8, height = 8, units = "in")

## #

mod <- cmdstan_model("R/irt_gpu.stan", compile = TRUE)
mod$compile(cpp_options = list(stan_opencl = FALSE), force_recompile = TRUE)

fit <- mod$sample(
  data = tar_read(stan_data_adams),
  chains = 2,
  iter_warmup = 100,
  iter_sampling = 100,
  seed = 02139,
  parallel_chains = 2
)
