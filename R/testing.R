#### TESTING FILE ONLY
rm(list=ls())
gc()

library(tidyverse)
library(cmdstanr)
library(tidybayes)
library(bayesplot)
library(arrow)
library(brms)
library(targets)

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

data <- tar_read(data_base_adams) |> 
  # propositions are not quite right
  mutate(candidate = case_when(
    str_detect(race, "PROPOSITION") ~ str_c(race, candidate, sep = " - "),
    TRUE ~ candidate
  ),
  race = str_remove(race, ", "))

# Assign unique IDs to races and candidates
races <- data |> 
  distinct(race) |> 
  arrange(race) |> 
  mutate(race_id = row_number())

candidates <- data |> 
  distinct(race, candidate) |> 
  arrange(race, candidate) |>
  select(candidate) |>
  mutate(candidate_id = row_number())

# Join back to the original data
df <- data |> 
  left_join(races, by = "race") |> 
  left_join(candidates, by = "candidate")

# some races are not classified perfectly in districts rn so they would show up as list-columns (bad)
bad_races <- df |> 
  count(cvr_id, race_id) |> 
  filter(n > 1) |> 
  distinct(race_id) |> 
  pull(race_id)

df <- df |> 
  filter(!(race_id %in% bad_races)) |>
  drop_na(race_id, candidate_id)

merge_choices <- function(race, candidate){
  
  choices <- df |> 
    filter(race == {{ race }}) |> 
    distinct(cvr_id, candidate)
  
  df |> 
    select(cvr_id, race_id, candidate_id) |> 
    arrange(race_id, candidate_id) |> 
    distinct(cvr_id) |> 
    mutate(id = row_number()) |> 
    left_join(choices) |> 
    drop_na(candidate) |> 
    filter(candidate == {{ candidate }}) |> 
    select(id)
}

dime <- read_csv("data/dime_matches.csv") |> 
  mutate(recipient.cfscore = (recipient.cfscore - mean(recipient.cfscore, na.rm = TRUE)/sd(recipient.cfscore, na.rm = TRUE))) |> 
  drop_na(recipient.cfscore) |> 
  distinct(office, district, candidate, .keep_all = TRUE) |> 
  mutate(race = str_c(office, district, sep = " - ")) |> 
  select(race, candidate, dime_score = recipient.cfscore) |> 
  filter(race %in% unique(df$race)) |> 
  filter(n()>1, .by = race)

draws <- cat_2pl |> 
  spread_draws(alpha[id]) |> 
  ungroup() |>  
  mutate(alpha = alpha/sd(alpha)) |> 
  select(id, alpha)

merged <- dime |> 
  mutate(voters = map2(race, candidate, merge_choices)) |> 
  unnest(cols = voters) |> 
  left_join(draws)

p2 <- merged |> 
  ggplot(aes(x = alpha, y = candidate)) +
  stat_halfeye() +
  geom_point(aes(x = dime_score, y = candidate), color = "blue", size = 4) +
  facet_wrap(~ race, ncol = 1, scales = "free") +
  theme_medsl() +
  labs(x = expression(alpha), y = "")

ggsave("figs/dime_comparison.jpeg", p2, width = 8, height = 12, units = "in")
