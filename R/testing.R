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

m <- cmdstan_model("R/cat_2pl_gpu.stan", compile = TRUE)
# m$compile(cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)

fit <- m$sample(
  data = stan_data,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 02139,
  parallel_chains = 4,
  threads_per_chain = 20
)

###################################

cat_2pl <- readRDS("fits/cat_2pl.rds") |> 
  as_draws_matrix() |> 
  subset_draws(variable = "gamma")

# glimpse(cat_2pl)

var <- str_replace(colnames(cat_2pl), ".+\\[([0-9]+),([0-9]+)\\]$", "\\1")
var <- as.integer(var)
dim <- rep(1, length(var))
dim <- str_replace(colnames(cat_2pl), ".+\\[([0-9]+),([0-9]+)\\]$", "\\2")
dim <- as.integer(dim)

colord <- order(var, dim)
colnames(cat_2pl) <- str_replace(
  colnames(cat_2pl),
  "^gamma\\[([0-9]+),([0-9]+)\\]$",
  "LambdaV\\1_\\2"
)
# glimpse(colnames(cat_2pl))

library(factor.switching)

out <- rsp_full_sa(
  lambda_mcmc = cat_2pl[, colord],
  rotate = TRUE,
  maxIter = 200,
  threshold = 1e-6,
  sa_loops = 10,
  verbose = TRUE
)

mpcm_lambda_mat_id <- as_draws_df(out$lambda_reordered_mcmc)
summary(summarise_draws(mpcm_lambda_mat_id))

summary(summarise_draws(cat_2pl))

mpcm_lambda_mat_id[, 1:10] |> 
  mcmc_areas()

draws <- as_draws_df(out$lambda_reordered_mcmc) |>
  sweep(1, sds, "*") |> 
  mutate(across(everything(), ~ na_if(.x, 0))) |> 
  select(-starts_with(".")) |> 
  pivot_longer(cols = everything(), values_drop_na = TRUE) |> 
  mutate(name = str_remove(name, "LambdaV")) |> 
  # separate_wider_delim(cols = name, delim = "V", names = c("parameter", "race_id")) |> 
  separate_wider_delim(cols = name, delim = "_", names = c("race_id", "candidate_id")) |> 
  # mutate(candidate_id = str_remove(candidate_id, "]")) |> 
  mutate(race_id = as.numeric(race_id),
         candidate_id = as.numeric(candidate_id)) |> 
  left_join(races) |> 
  left_join(candidates) |> 
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

pres_options <- distinct(pres_choices, candidate) |> pull()

p1 <- draws |> 
  select(value, race, candidate) |> 
  filter(str_detect(race, "US PRESIDENT"), candidate %in% c("JOSEPH R BIDEN", "GLORIA LA RIVA", "DONALD J TRUMP", "BILL HAMMONS")) |> 
  # mutate(race = str_remove(race, " - STATEWIDE")) |> 
  # mutate(parameter = factor(parameter, labels = c("Difficulty", "Discrimination"))) |>
  mutate(value = ifelse(candidate == "BILL HAMMONS", 0, value)) |> 
  ggplot(aes(x = value, y = candidate)) +
  stat_halfeye(normalize = "xy") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  # facet_wrap(~ parameter, scales = "free_x") +
  labs(x = "", y = "", title = "Discrimination Parameter after Full-SA RSP Algorithm") +
  theme_bw()

p1 + p2

ggsave("figs/compare.jpg", width = 16, height = 12, units = "in")
