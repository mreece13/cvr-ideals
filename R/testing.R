#### TESTING FILE ONLY
rm(list=ls())
gc()

library(tidyverse)
library(cmdstanr)
library(tidybayes)
library(bayesplot)
library(arrow)
library(brms)

stan_data <- targets::tar_read(stan_data)
data <- targets::tar_read(data_base_adams) |> 
  # propositions are not quite right
  mutate(candidate = case_when(
    str_detect(race, "PROPOSITION") ~ str_c(race, candidate, sep = " - "),
    TRUE ~ candidate
  ),
  race = str_remove(race, ", "),
  candidate_order = case_match(
    candidate,
    "JOSEPH R BIDEN" ~ 1,
    "DONALD J TRUMP" ~ 2,
    .default = 3
  ),
  race_order = case_match(
    office,
    "US PRESIDENT" ~ 1,
    .default = 2
  ))

# Assign unique IDs to races and candidates
races <- df |> 
  distinct(race, race_order) |> 
  arrange(race_order, race) |> 
  select(race) |> 
  mutate(race_id = row_number())

candidates <- df |> 
  distinct(race_order, race, candidate_order, candidate) |> 
  arrange(race_order, race, candidate_order, candidate) |>
  select(candidate) |>
  mutate(candidate_id = row_number())

bad_races <- df |> 
  left_join(races) |> 
  left_join(candidates) |> 
  count(cvr_id, race_id) |> 
  filter(n > 1) |> 
  distinct(race_id) |> 
  pull(race_id)

votes_matrix <- df |> 
  left_join(races) |> 
  left_join(candidates) |> 
  filter(!(race_id %in% bad_races)) |> 
  mutate(trump_voter = ifelse(race == "US PRESIDENT - STATEWIDE" & candidate == "DONALD J TRUMP", 1, 0), .by = cvr_id) |> 
  select(cvr_id, race_id, candidate_id, trump_voter) |> 
  arrange(race_id, candidate_id) |>
  pivot_wider(names_from = race_id, values_from = candidate_id, values_fill = 0) |> 
  select(cvr_id, trump_voter) |> 
  mutate(id = row_number())

joiner <- stan_data$candidates |>
  as_tibble(rownames = "race_id") |>
  pivot_longer(cols = -race_id, names_to = "candidate_id") |>
  filter(value == 1) |>
  select(-value) |>
  mutate(across(everything(), as.numeric)) |> 
  filter(!(race_id %in% bad_races))

fit <- readRDS("fits/cat_2pl_unrestricted.rds")

gamma_signs <- as_draws_df(fit) |>
  select(matches("^gamma\\[")) |>
  rowMeans() |>
  sign()

fit_flipped <- as_draws_df(fit) |>
  mutate(across(matches("(^alpha\\[)|(^gamma\\[)"), ~ gamma_signs * .))

gammas <- fit_flipped |>
  spread_draws(gamma[race_id, candidate_id]) |>
  inner_join(joiner) |>
  left_join(races) |>
  left_join(candidates) |>
  ungroup()

betas <- fit_flipped |>
  spread_draws(beta[race_id, candidate_id]) |>
  inner_join(joiner) |>
  left_join(races) |>
  left_join(candidates) |>
  ungroup()

betas |>
  filter(str_detect(race, "US PRESIDENT|US SENATE|US HOUSE")) |>
  ggplot(aes(x = beta, y = candidate)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  facet_wrap(~ race, scales = "free") +
  # scale_x_continuous(limits = c(0, 15)) +
  theme_bw()

gammas |>
  filter(str_detect(race, "US PRESIDENT|US SENATE|US HOUSE")) |>
  ggplot(aes(x = gamma, y = candidate)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  facet_wrap(~ race, scales = "free") +
  # scale_x_continuous(limits = c(0, 15)) +
  theme_bw()

alphas <- fit_flipped |> 
  spread_draws(alpha[cvr_id])

alphas |> 
  ungroup() |> 
  mutate(alpha = (alpha - mean(alpha))/sd(alpha)) |> 
  # left_join(votes_matrix, by = c("cvr_id" = "id")) |> 
  filter(cvr_id < 15) |>
  ggplot(aes(x = alpha, y = as.character(cvr_id))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  theme_bw()

## Rhat Plots

# cat_1pl <- readRDS("fits/cat_1pl_new.rds")
cat_2pl <- readRDS("fits/cat_2pl_unrestricted.rds")
ber_1pl <- readRDS("fits/bernoulli_rasch.rds")
ber_2pl <- readRDS("fits/bernoulli_2pl.rds")

signs <- as_draws_df(cat_2pl) |>
  select(matches("^gamma\\[")) |>
  rowMeans() |>
  sign()

cat_2pl_flipped <- as_draws_df(cat_2pl) |>
  mutate(across(matches("(^alpha\\[)|(^gamma\\[)"), ~ gamma_signs * .))

fits <- list("Categorical, 2PL" = cat_2pl_flipped,
             "Bernoulli, 1PL" = ber_1pl,
             "Bernoulli, 2PL" = ber_2pl)

rhats <- tibble(fit = fits, fit_name = names(fits)) |> 
  mutate(rhat = map(fit, ~ select(summarise_draws(.x), rhat))) |> 
  select(-fit) |> 
  unnest(cols = rhat)

p_rhats <- rhats |> 
  ggplot(aes(x = rhat)) +
  geom_dots() +
  # geom_vline(xintercept = 1, color = "blue", linetype = "dashed") +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~ fit_name, scales = "free_x", ncol=4) +
  labs(x = expression(hat(R)), y = "", color = "Fit", title = "Distribution of Gelman-Rubin Diagnostic") +
  theme_bw()

ggsave("figs/rhat_comparison.jpg", plot = p_rhats, width = 8, height = 4, units = "in")


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

open_dataset("../cvrs/data/cvr_qa_main/", format = "parquet") |> 
  filter(state == "COLORADO") |> 
  distinct(county_name, cvr_id) |> 
  tally() |> 
  collect()


#############################################

base <- open_dataset("../cvrs/data/cvr_qa_main/", format = "parquet") |> 
  filter(state == "COLORADO", (magnitude == 1 | is.na(magnitude)), !is.na(office), !is.na(district))

contested_races <- base |> 
  distinct(county_name, office, district, candidate) |> 
  arrange(county_name, office, district) |> 
  collect() |> 
  filter(n() > 1, .by = c(county_name, office, district)) |> 
  distinct(county_name, office, district)

randoms <- base |>
  distinct(county_name, cvr_id) |>
  collect() |> 
  slice_sample(n=1e5)

data <- base |> 
  inner_join(randoms, by = c("county_name", "cvr_id")) |>
  inner_join(contested_races, by = c("county_name", "office", "district")) |> 
  mutate(choice_rep = as.numeric(party_detailed == "REPUBLICAN")) |> 
  collect() |> 
  mutate(district = if_else(office %in% c("US HOUSE", "STATE SENATE", "STATE HOUSE"), 
                            str_remove(district, county_name),
                            district),
         district = str_remove(district, fixed(", ")),
         race = str_c(office, district, sep = " - "),
         candidate = case_when(
           str_detect(race, "PROPOSITION") ~ str_c(race, candidate, sep = " - "),
           .default = candidate
         ))

uniques <- data |> 
  arrange(cvr_id, race, candidate) |> 
  mutate(choice = str_c(race, candidate, choice_rep, sep = "|")) |> 
  select(state, county_name, cvr_id, choice) |> 
  nest(data = choice) |> 
  mutate(pattern = map_chr(data, ~ str_flatten(pull(.x, choice), collapse = "||")))

grouped <- uniques |> 
  left_join(count(uniques, pattern) |> mutate(group_id = row_number())) |> 
  select(state, county_name, data, n, group_id) |> 
  unnest(cols = data) |> 
  separate_wider_delim(choice, delim = "|", names = c("race", "candidate", "choice_rep"))

form <- bf(
  choice_rep | weights(n) ~ 1 + (1 | race) + (1 | group_id),
  family = brmsfamily("bernoulli", link = "logit")
)

priors <-
  prior("normal(0, 2)", class = "Intercept") +
  prior("normal(0, 3)", class = "sd", group = "group_id") +
  prior("normal(0, 3)", class = "sd", group = "race")

fit <- brm(
  formula = form,
  prior = priors,
  data = grouped,
  chains = 4,
  iter = 2000,
  seed = 02139,
  silent = 0,
  file = str_c("fits/bernoulli_rasch_grouped"),
  file_refit = "on_change"
)


d <- targets::tar_read(data_colorado)

d <- open_dataset("../cvrs/data/cvr_qa_main/", format = "parquet") |> 
  filter(state == "COLORADO") |> 
  distinct(office, district) |> 
  collect()
