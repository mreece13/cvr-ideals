#### TESTING FILE ONLY
rm(list=ls())
gc()

library(patchwork)
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(targets)
library(posterior)

source("../medsl_theme.R")

###################################

cat_2pl <- readRDS("fits/cat_2pl_streamlined_full.rds") |> 
  as_draws_matrix() |> 
  subset_draws(variable = "gamma")

stan_data <- tar_read(stan_data_adams)

var <- str_replace(colnames(cat_2pl), ".+\\[([0-9]+)\\]$", "\\1")
var <- as.integer(var)
dim <- rep(1, length(var))
# dim <- rep(1:stan_data$K, stan_data$sizes)

colnames(cat_2pl) <- str_c("LambdaV", var, "_", dim)

sds <- readRDS("fits/cat_2pl_streamlined_full.rds") |> 
  spread_draws(alpha[cvr_id]) |> 
  ungroup() |> 
  summarise(sd = sd(alpha), .by = ".draw") |> 
  pull(sd)

library(factor.switching)

out <- rsp_exact(
  lambda_mcmc = cat_2pl,
  rotate = TRUE,
  maxIter = 200,
  threshold = 1e-6,
  # sa_loops = 10,
  verbose = TRUE
)

draws <- as_draws_df(out$lambda_reordered_mcmc) |>
  sweep(1, sds, "*") |> 
  # mutate(across(everything(), ~ na_if(.x, 0))) |> 
  select(-starts_with(".")) |> 
  pivot_longer(cols = everything(), values_drop_na = TRUE) |> 
  mutate(name = str_remove_all(name, "LambdaV|\\_1") |> as.numeric()) |> 
  left_join(mutate(ungroup(ids), name = 1:n()), by = "name") |> 
  select(-candidate_id, -race_id, -name) |> 
  # separate_wider_delim(cols = name, delim = "V", names = c("parameter", "race_id")) |> 
  # separate_wider_delim(cols = name, delim = "_", names = c("race_id", "candidate_id")) |> 
  # mutate(candidate_id = str_remove(candidate_id, "]")) |> 
  # mutate(race_id = as.numeric(race_id),
  #        candidate_id = as.numeric(candidate_id)) |> 
  # left_join(races) |> 
  # left_join(candidates) |> 
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
    "GLORIA LA RIVA SUNIL FREEMAN" ~ "GLORIA LA RIVA",
    .default = candidate
  ))

pres_options <- distinct(pres_choices, candidate) |> pull()

p1 <- draws |> 
  select(value, race, candidate) |> 
  filter(str_detect(race, "US PRESIDENT"), candidate %in% c("JOSEPH R BIDEN", "GLORIA LA RIVA", "DONALD J TRUMP", "BILL HAMMONS")) |> 
  mutate(value = ifelse(candidate == "BILL HAMMONS", 0, value)) |> 
  ggplot(aes(x = value, y = candidate)) +
  stat_halfeye(normalize = "xy") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  # facet_wrap(~ parameter, scales = "free_x") +
  labs(x = "", y = "", title = "Discrimination Parameter after Full-SA RSP Algorithm") +
  theme_bw()

p1 + p2

ggsave("figs/compare.jpg", width = 16, height = 12, units = "in")
