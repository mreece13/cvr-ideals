rm(list=ls())
gc()

library(tidyverse)
library(tidybayes)
library(bayesplot)
library(cmdstanr)
library(posterior)

source("../medsl_theme.R")

data <- targets::tar_read(data_colorado) |> 
  filter(candidate != "undervote")

bad_races <- df |> 
  count(county_name, cvr_id, race_id) |> 
  filter(n > 1) |> 
  distinct(race_id) |> 
  pull(race_id)

ids |> ungroup() |>  filter(race_id %in% bad_races) |> distinct(race)

df |> 
  filter(race == "STATE HOUSE - 058") |> 
  filter(n() > 1, .by = cvr_id)

# Assign unique IDs to races and candidates
ids <- data |> 
  distinct(race, candidate) |> 
  arrange(race, candidate) |> 
  group_by(race) |> 
  mutate(candidate_id = 1:n(),
         race_id = cur_group_id())

# Join back to the original data
df <- left_join(data, ids, join_by(race, candidate))

# Create the votes matrix
votes_matrix <- df |> 
  select(cvr_id, race_id, candidate_id) |> 
  arrange(race_id, candidate_id) |>
  pivot_wider(names_from = race_id, values_from = candidate_id, values_fill = 0) |> 
  select(-cvr_id) |> 
  as.matrix()

num_cands <- df |> 
  distinct(race, race_id, candidate) |> 
  count(race_id) |> 
  pull(n)

# Prepare data for Stan
stan_data <- list(
  threaded = 0,
  J = n_distinct(df$cvr_id),
  K = n_distinct(ids$race),
  C = length(ids$candidate),
  votes = votes_matrix,
  sizes = num_cands
)

m <- cmdstan_model("R/cat_2pl_streamlined.stan", compile = FALSE)
m$compile(cpp_options = list(stan_threads = TRUE), force_recompile = FALSE)

fit <- m$sample(
  data = stan_data,
  chains = 1,
  iter_sampling = 200
)

fit <- m$pathfinder(
  data = stan_data,
  seed = 02139,
  num_threads = 5
)

fit$save_object("fits/cat_2pl_streamlined2.rds")

fit <- read_rds("fits/cat_2pl_streamlined.rds")

## graphing

pres_choices <- df |>
  filter(office == "US PRESIDENT") |>
  distinct(county_name, cvr_id, candidate) |> 
  mutate(candidate = str_squish(candidate)) |> 
  mutate(candidate = case_match(
    candidate,
    "JOSEPH KISHORE NORISSA SANTA CRUZ" ~ "JOSEPH KISHORE",
    "JORDAN CANCER SCOTT JENNIFER TEPOOL" ~ "JORDAN SCOTT",
    "BILL HAMMONS ERIC BODENSTAB" ~ "BILL HAMMONS",
    "MARK CHARLES ADRIAN WALLACE" ~ "MARK CHARLES",
    "PRINCESS KHADIJAH MARYAM JACOB FAMBRO KHADIJAH MARYAM JACOB SR" ~ "PRINCESS KHADIJAH JACOB-FAMBRO",
    "KYLE KENLEY KOPITKE NATHAN RE VO SORENSON" ~ "KYLE KENLEY",
    "JOE MC HUGH ELIZABETH STORM" ~ "JOE MCHUGH",
    "BLAKE HUBER FRANK ATWOOD" ~ "BLAKE HUBER",
    "PHIL COLLINS BILLY JOE PARKER" ~ "PHIL COLLINS",
    .default = candidate
  ))

voters <- df |> 
  select(county_name, cvr_id, race_id, candidate_id) |> 
  arrange(race_id, candidate_id) |> 
  distinct(county_name, cvr_id) |> 
  mutate(id = 1:n()) |> 
  left_join(pres_choices)

joined <- fit |> 
  as_draws_df() |> 
  slice_head(n=1) |> 
  select(starts_with("alpha")) |> 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "alpha") |> 
  mutate(id = str_extract(variable, "\\d+") |> as.numeric()) |>
  mutate(alpha = alpha/sd(alpha)) |> 
  left_join(voters, join_by(id)) |> 
  drop_na(candidate)

# joined <- fit |> 
#   spread_draws(alpha[id], ndraws = 1) |> 
#   ungroup() |> 
#   mutate(alpha = alpha/sd(alpha)) |> 
#   left_join(voters, join_by(id)) |> 
#   drop_na(candidate) 

p_agg_main <- joined |> 
  filter(candidate %in% c("JOSEPH R BIDEN", "DONALD J TRUMP")) |> 
  # mutate(alpha = alpha*-1) |> 
  ggplot(aes(x = alpha, fill = candidate, y = candidate)) +
  ggridges::geom_density_ridges(scale = 1, panel_scaling = FALSE) +
  scale_fill_discrete(type = c("#F6573E", "#3791FF"), guide = "none") +
  labs(x = expression(alpha), y = "", fill = "Candidate") +
  theme_medsl()

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
  select(starts_with("beta["), starts_with("gamma[")) |> 
  sweep(1, sds, "*") |> 
  pivot_longer(cols = everything()) |> 
  bind_cols(bind_rows(ids, ids)) |> 
  mutate(name = str_remove(name, "\\[.*?$")) |> 
  select(name, race, candidate, value)

targets::tar_read(stan_data)

p <- params |> 
  filter(str_detect(race, "US PRESIDENT")) |> 
  ggplot(aes(x = value, y = candidate)) +
  geom_point() +
  facet_wrap(~ name) +
  theme_bw()

ggsave("figs/params_var.jpeg", plot = p, width = 8, height = 8, units = "in")
