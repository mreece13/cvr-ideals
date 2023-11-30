#### TESTING FILE ONLY
rm(list=ls())
gc()

library(tidyverse)
library(cmdstanr)
library(tidybayes)
library(bayesplot)
library(arrow)

### NEW VERSION ACCOUNTING FOR VARYING CHOICES

partial_schema <- schema(
  field("state", string()),
  field("cvr_id", string()),
  field("county_name", string()),
  field("candidate", string()),
  field("district", string()),
  field("magnitude", string()),
  field("office", string()),
  field("party_detailed", string())
)

base <- open_dataset("../cvrs/data/cvr_qa_main",
                          partitioning = "state",
                          schema = partial_schema,
                          format = "parquet") |> 
  filter(state == "COLORADO", county_name == "ADAMS", !is.na(office), !is.na(district)) |> 
  select(-magnitude) |> 
  mutate(race = str_c(office, district, sep = " - "),
         race = str_remove(race, fixed(", "))) |> 
  # propositions are not quite right
  mutate(candidate = case_when(
    str_detect(race, "PROPOSITION") ~ str_c(race, candidate, sep = " - "),
    TRUE ~ candidate
  )) |> 
  collect()
  
## Filter to top 2 candidates in each race
# top_cands <- base |>
#   count(race, candidate) |>
#   arrange(race, desc(n)) |>
#   slice_head(n=2, by = race) |>
#   select(-n)
# 
# base <- inner_join(base, top_cands)

races <- base |> 
  distinct(race) |> 
  arrange(race) |> 
  mutate(race_id = row_number())

candidates <- base |> 
  distinct(race, candidate) |> 
  arrange(race, candidate) |>
  select(-race) |>
  mutate(candidate_id = row_number())

# Create the candidate availability matrix
candidate_availability <- base |> 
  summarize(available = n() > 0, .by = c(race, candidate)) |> 
  left_join(races, by = "race") |> 
  left_join(candidates, by = "candidate") |> 
  select(-race, -candidate) |> 
  drop_na(race_id, candidate_id) |> 
  complete(race_id, candidate_id, fill = list(available = FALSE)) |> 
  pivot_wider(names_from = candidate_id, values_from = available) |> 
  select(-race_id) |> 
  mutate(across(everything(), as.numeric)) |> 
  as.matrix()

df <- base |> 
  left_join(races, by = "race") |> 
  left_join(candidates, by = "candidate")

# some races are not classified perfectly in districts rn so they would show up as list-columns (bad)
bad_races <- df |> 
  count(cvr_id, race_id) |> 
  filter(n > 1) |> 
  distinct(race_id) |> 
  pull(race_id)

randos <- df |> 
  distinct(cvr_id) |> 
  slice_sample(n=100)

df <- df |> 
  inner_join(randos) |> 
  filter(!(race_id %in% bad_races)) |>
  drop_na(race_id, candidate_id)

# Create the votes matrix
votes_matrix <- df |> 
  select(cvr_id, race_id, candidate_id) |> 
  arrange(race_id, candidate_id) |>
  pivot_wider(names_from = race_id, values_from = candidate_id, values_fill = 0) |> 
  select(-cvr_id)

missing <- setdiff(races$race_id, colnames(votes_matrix)) |> as.character()

if (length(missing) > 0){
  votes_matrix[, missing] <- 0
  votes_matrix <- relocate(votes_matrix, all_of(as.character(races$race_id))) |> 
    as.matrix()
}

eligibility_matrix <- ifelse(votes_matrix > 0, 1, 0)

# Prepare data for Stan
stan_data <- list(
  J = n_distinct(df$cvr_id),
  K = max(races$race_id),
  C = max(candidates$candidate_id),
  candidates = candidate_availability,
  eligibility = eligibility_matrix,
  votes = votes_matrix,
  parallelize = 1
)

rm(base, candidates, candidate_availability, df, eligibility_matrix, races, 
   randos, votes_matrix, bad_races, missing, partial_schema)
gc()

## Optimized 2PL

# model <- cmdstan_model("R/mnm_varying_2pl_optimized.stan", compile = FALSE)
# model$compile(
#   cpp_options = list(stan_threads = TRUE)
# )
# 
# fit <- model$sample(
#   data = stan_data,
#   # chains = 2,
#   # iter_warmup = 100,
#   # iter_sampling = 400,
#   seed = 02139,
#   parallel_chains = 4,
#   threads_per_chain = 16
# )
# 
# fit$save_object("fits/cat_2pl.rds")

## Fit Diagnostics
# 
# fit <- readRDS("fits/cat_2pl.rds")
# 
# rhat(fit)
# 
# draws <- fit$draws() |>
#   as_tibble() |>
#   select(contains("alpha"), contains("beta"), contains("gamma")) |>
#   mutate(across(everything(), ~ if_else(.x < 0.01, NA, .x)))
# 
# fit |>
#   spread_draws(alpha[cvr_id], gamma[race_id]) |>
#   ungroup() |>
#   filter(cvr_id < 10) |>
#   # filter(gamma > 0.05) |>
#   # left_join(races, join_by(race_id)) |>
#   # filter(race != "COUNTY JUDGE - ADAMS") |>
#   ggplot(aes(x = alpha, y = as.character(cvr_id))) +
#   stat_halfeye() +
#   geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
#   # scale_x_continuous(limits = c(0, 15)) +
#   theme_bw()
# 
# mcmc_pairs(fit$draws(), pars = vars("alpha[1]", "alpha[2]", "alpha[3]",
#                                     "gamma[40]", "gamma[2]", "gamma[3]",
#                                   "beta[40]", "beta[2]", "beta[3]"))


## 2PL with parameters that vary by candidate

model <- cmdstan_model("R/cat_2pl.stan", compile = FALSE)
model$compile(
  cpp_options = list(stan_threads = TRUE)
)

fit <- model$sample(
  data = stan_data,
  chains = 4,
  seed = 02139,
  parallel_chains = 4,
  threads_per_chain = 20
)

fit$save_object("fits/cat_2pl_2.rds")

fit <- readRDS("fits/cat_2pl.rds")

fit |> summarise_draws(.cores = 4) |> filter(!str_detect(variable, "alpha")) |> summary()

stan_data <- targets::tar_read(stan_data)
data <- targets::tar_read(data_base_adams)

targets::tar_read(plot_rhat_comparison)

df <- data |> 
  # propositions are not quite right
  mutate(candidate = case_when(
    str_detect(race, "PROPOSITION") ~ str_c(race, candidate, sep = " - "),
    TRUE ~ candidate
  ))

# Assign unique IDs to races and candidates
races <- df |> 
  distinct(race) |> 
  arrange(race) |> 
  mutate(race_id = row_number())

candidates <- df |> 
  distinct(race, candidate) |> 
  arrange(race, candidate) |>
  # select(-race) |>
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

# t <- fit |>
#   spread_draws(gamma[race_id, candidate_id], ndraws = 1)
# 
# t |>
#   select(race_id, candidate_id, gamma) |>
#   pivot_wider(names_from = candidate_id, values_from = gamma)

gammas <- fit |>
  spread_draws(gamma[race_id, candidate_id]) |>
  inner_join(joiner) |>
  left_join(races) |>
  left_join(candidates) |>
  ungroup()

betas <- fit |>
  spread_draws(beta[race_id]) |>
  # inner_join(joiner) |>
  left_join(races) |>
  # left_join(candidates) |>
  ungroup()

gammas |>
  filter(!str_detect(candidate, "PROPOSITION")) |>
  ggplot(aes(x = gamma, y = candidate)) +
  stat_pointinterval() +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  facet_wrap(~ race, scales = "free") +
  # scale_x_continuous(limits = c(0, 15)) +
  theme_bw()

alphas <- fit |> 
  spread_draws(alpha[cvr_id])

alphas |> 
  # mutate(alpha = (alpha - mean(alpha))/sd(alpha)) |> 
  left_join(votes_matrix, by = c("cvr_id" = "id")) |> 
  # filter(cvr_id < 15) |> 
  ggplot(aes(x = alpha, fill = as.character(trump_voter), group = as.character(trump_voter), color = as.character(trump_voter))) +
  geom_density() +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  theme_bw()


fit <- readRDS("fits/mnm_varying_1pl.rds")

alphas |> 
  filter(cvr_id < 10) |> 
  ggplot(aes(x = alpha, y = as.character(cvr_id))) +
  stat_halfeye() +
  theme_bw()


betas |>
  filter(!str_detect(race, "JUDGE")) |>
  ggplot(aes(x = beta, y = race)) +
  stat_pointinterval() +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  # facet_wrap(~ race, scales = "free") +
  # scale_x_continuous(limits = c(0, 15)) +
  theme_bw()


########################

model <- cmdstan_model("R/cat_1pl.stan", compile = FALSE)
model$compile(
  cpp_options = list(stan_threads = TRUE)
)

fit <- model$sample(
  data = stan_data,
  # chains = 2,
  # iter_warmup = 100,
  # iter_sampling = 400,
  seed = 02139,
  parallel_chains = 4,
  threads_per_chain = 2
)

fit$save_object("fits/cat_2pl.rds")


fit <- targets::tar_read(fit_2pl_binomial_partisans_colorado_adams)

data <- targets::tar_read(data_colorado_adams)
data_all <- targets::tar_read(data_colorado)

form_2pl <- bf(
  choice_rep ~ beta + exp(loggamma) * alpha,
  nl = TRUE,
  alpha ~ 0 + (1 | cvr_id),
  beta ~ 1 + (1 |i| race),
  loggamma ~ 1 + (1 |i| race),
  family = brmsfamily("bernoulli", link = "logit")
)

prior_2pl <- 
  prior("normal(0, 2)", class = "b", nlpar = "beta") +
  prior("normal(0, 1)", class = "b", nlpar = "loggamma") +
  prior("normal(0, 1)", class = "sd", group = "cvr_id", nlpar = "alpha") + 
  prior("normal(0, 3)", class = "sd", group = "race", nlpar = "beta") +
  prior("normal(0, 1)", class = "sd", group = "race", nlpar = "loggamma")

fit_2pl <- brm(
  formula = form_2pl,
  prior = prior_2pl,
  data = data,
  chains = 4,
  iter = 2000,
  file = "fits/bin_2pl",
  file_refit = "on_change",
  sample_prior = TRUE,
  seed = 02139,
  silent = 0,
  control = list(adapt_delta = 0.95)
)


## Person Locations

person_pars_2pl <- ranef(fit_2pl, summary = FALSE)$cvr_id[, , "alpha_Intercept"] 

person_sds_2pl <- apply(person_pars_2pl, 1, sd)

person_pars_2pl <- person_pars_2pl |>
  sweep(1, person_sds_2pl, "/") |>
  posterior_summary() |>
  as_tibble() |>
  rownames_to_column(var = "cvr_id")

person_pars_2pl |> 
  ggplot(aes(x = Estimate)) +
  geom_histogram() +
  theme_bw()

person_pars_2pl |>
  slice_sample(n=15) |> 
  arrange(Estimate) |>
  mutate(id2 = seq_len(n())) |>
  ggplot(aes(cvr_id, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (sorted after Estimate)") +
  theme_bw()

## Race Locations

item_pars_2pl <- coef(fit_2pl, summary = FALSE)$race

# locations
beta <- item_pars_2pl[, , "beta_Intercept"] |>
  posterior_summary() |>
  as_tibble() |>
  rownames_to_column()

# slopes
gamma <- item_pars_2pl[, , "loggamma_Intercept"] |>
  exp() |>
  sweep(1, person_sds_2pl, "*") |>
  posterior_summary() |>
  as_tibble() |>
  rownames_to_column()

item_pars_2pl <- bind_rows(beta, gamma, .id = "nlpar") |>
  rename(item = "rowname") |>
  mutate(item = as.numeric(item)) |>
  mutate(
    nlpar = factor(nlpar, labels = c("Easiness", "Discrimination"))
  )

item_pars_2pl |> 
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  facet_wrap("nlpar", scales = "free_x") +
  coord_flip() +
  labs(x = "Item Number") +
  theme_bw()

#### Rasch Model too

form_1pl <- bf(
  choice_rep ~ 1 + (1 | race) + (1 | cvr_id),
  family = brmsfamily("bernoulli", link = "logit")
)

prior_1pl <- 
  prior("normal(0, 2)", class = "Intercept") +
  prior("normal(0, 3)", class = "sd", group = "cvr_id") + 
  prior("normal(0, 3)", class = "sd", group = "race")

fit_1pl <- brm(
  formula = form_1pl,
  prior = prior_1pl,
  data = data_colorado,
  chains = 4,
  iter = 2000,
  file = "fits/bin_1pl_colorado",
  file_refit = "on_change",
  sample_prior = TRUE,
  seed = 02139,
  silent = 0
)

## Rhat Plots

cat_1pl <- readRDS("fits/cat_1pl_new.rds")
cat_2pl <- readRDS("fits/cat_2pl_new.rds")
bin_1pl <- readRDS("fits/bin_1pl_colorado.rds")
bin_2pl <- readRDS("fits/bin_2pl_colorado.rds")

fits <- list("Categorical, 1PL" = cat_1pl,
             "Categorical, 2PL" = cat_2pl,
             "Binomial, 1PL" = bin_1pl,
             "Binomial, 2PL" = bin_2pl)

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
