#### TESTING FILE ONLY

library(targets)
library(tidyverse)
library(cmdstanr)
library(tidybayes)
library(bayesplot)
library(brms)
library(arrow)

source("../medsl_theme.R")

# cpp_options <- list(
#   STAN_OPENCL = TRUE
# )
# cmdstan_make_local(cpp_options = cpp_options)
# rebuild_cmdstan()

df <- tar_read(data_colorado_adams) |> 
  arrange(cvr_id) |> 
  slice_head(n=1001) |>  # specifically selected to include all races for last voter
  mutate(race = str_c(office, district, sep = " - "),
         voter_idx = as.numeric(factor(cvr_id)),
         race_idx = as.numeric(factor(race)),
         candidate_idx = as.numeric(factor(candidate, levels = unique(candidate))))

data_stan <- list(
  J = max(df$voter_idx), # number of voters
  K = max(df$race_idx), # number of races
  N = nrow(df), # number of observations
  C = max(df$candidate_idx), # number of candidates
  jj = df$voter_idx, # voter for observation n
  kk = df$race_idx, # race for observation n
  y = df$candidate_idx,  # candidate choice for observation n
  slices_n = seq_len(nrow(df))
)

model <- cmdstan_model("R/mnm_base.stan")

fit <- model$sample(
  data = data_stan,
  chains = 4,
  iter_warmup = 500,
  iter_sampling = 1000,
  seed = 02139,
  parallel_chains = 4
)

fit$save_object(file = "fits/mnm_base.rds")

fit <- readRDS("fits/mnm_base.rds")

fit |> 
  spread_draws(alpha[voter_idx], beta[race_idx], gamma[race_idx]) |> 
  summarise_draws()

races <- df |> 
  distinct(race, race_idx)

p <- fit |> 
  spread_draws(gamma[race_idx]) |> 
  left_join(races, join_by(race_idx)) |> 
  ggplot(aes(x = gamma, y = race)) +
  stat_halfeye() +
  scale_x_continuous(limits = c(0, 30)) +
  theme_bw()

ggsave("figs/mnm_irt_gamma.jpeg", plot = p, width = 8, height = 12, units = "in")


## brms
make_stancode(
  formula = bf(choice_rep ~ exp(logalpha) * eta,
               eta ~ 1 + (1 | office/district) + (1 | cvr_id),
               logalpha ~ 1 + (1 | office/district),
               nl = TRUE),
  family = bernoulli(),
  data = df,
  seed = 02139,
  silent = 0,
  threads = threading(threads = 4)
)

make_stancode(
  formula = bf(candidate_idx ~ exp(logalpha) * eta,
               eta ~ 1 + (1 | race_idx) + (1 | voter_idx),
               logalpha ~ 1 + (1 | race_idx),
               nl = TRUE),
  data = df,
  family = categorical(),
  threads = threading(threads=4)
)

## parallel testing

model_parallel <- cmdstan_model("R/mnm_parallel.stan", compile = FALSE)
model_parallel$format(canonicalize = list("deprecations"))
model_parallel$compile(pedantic = TRUE,
                       # force_recompile = TRUE,
                       cpp_options = list(stan_threads = TRUE))

fit_parallel <- model_parallel$sample(
  data = data_stan,
  chains = 4,
  iter_warmup = 500,
  iter_sampling = 1000,
  seed = 02139,
  parallel_chains = 4,
  threads_per_chain = 2
)

fit_parallel$save_object(file = "fits/mnm_parallel.rds")

fit_parallel <- readRDS("fits/mnm_parallel.rds")


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

base_data <- open_dataset("~/Dropbox (MIT)/Research/hidden-partisanship/data/cvr_qa_main/",
                          partitioning = "state",
                          schema = partial_schema,
                          format = "parquet") |> 
  filter(state == "COLORADO", magnitude == "1", county_name == "ADAMS", !is.na(office), !is.na(district)) |> 
  select(-magnitude) |> 
  mutate(race = str_c(office, district, sep = " - ")) |> 
  # drop some judges that are mis-classified right now
  filter(!(race %in% c("COUNTY JUDGE - ADAMS", "COURT OF APPEALS JUDGE - STATEWIDE", 
                       "DISTRICT COURT JUDGE - 17, ADAMS", "SUPREME COURT JUSTICE - STATEWIDE"))) |> 
  # propositions are not quite right
  mutate(candidate = case_when(
    str_detect(race, "PROPOSITION") ~ str_c(race, candidate, sep = " - "),
    TRUE ~ candidate
  ))

# What are the contested races?
contested_races <- base_data |> 
  distinct(county_name, office, district, candidate) |> 
  arrange(county_name, office, district) |> 
  collect() |> 
  filter(n() > 1, .by = c(county_name, office, district)) |> 
  distinct(county_name, office, district)

# pick some random people
randoms <- base_data |>
  distinct(county_name, cvr_id) |>
  collect() |> 
  slice_sample(n=150)

df <- base_data |> 
  inner_join(randoms, by = c("county_name", "cvr_id")) |>
  inner_join(contested_races, by = c("county_name", "office", "district")) |> 
  collect() |> 
  mutate(district = if_else(office %in% c("US HOUSE", "STATE SENATE", "STATE HOUSE"), 
                            str_remove(district, str_c(", ", county_name)),
                            district)) |> 
  select(-office, -district, -state, -county_name)

# Assign unique IDs to races and candidates
races <- base_data |> 
  inner_join(contested_races, by = c("county_name", "office", "district")) |> 
  distinct(race) |> 
  arrange(race) |> 
  collect() |> 
  mutate(race_id = row_number())

candidates <- base_data |> 
  inner_join(contested_races, by = c("county_name", "office", "district")) |> 
  distinct(candidate) |> 
  arrange(candidate) |> 
  collect() |> 
  mutate(candidate_id = row_number())

# Join back to the original data
data <- df |> 
  left_join(races, by = "race") |> 
  left_join(candidates, by = "candidate")

# Create the candidate availability matrix
candidate_availability <- base_data |> 
  summarize(available = n() > 0, .by = c(race, candidate)) |> 
  left_join(races, by = "race") |> 
  left_join(candidates, by = "candidate") |> 
  select(-race, -candidate) |> 
  collect() |> 
  drop_na(race_id, candidate_id) |> 
  complete(race_id, candidate_id, fill = list(available = FALSE)) |> 
  pivot_wider(names_from = candidate_id, values_from = available) |> 
  select(-race_id) |> 
  mutate(across(everything(), as.numeric)) |> 
  as.matrix()

# Create the votes matrix
votes_matrix <- data |> 
  select(cvr_id, race_id, candidate_id) |> 
  arrange(race_id, candidate_id) |>
  pivot_wider(names_from = race_id, values_from = candidate_id, values_fill = 0) |> 
  select(-cvr_id)

missing <- setdiff(races$race_id, colnames(votes_matrix)) |> as.character()
votes_matrix[, missing] <- 0
votes_matrix <- relocate(votes_matrix, all_of(as.character(races$race_id))) |> 
  as.matrix()

eligibility_matrix <- ifelse(votes_matrix > 0, 1, 0)

# Prepare data for Stan
data_varying <- list(
  J = n_distinct(data$cvr_id),
  K = max(races$race_id),
  C = max(candidates$candidate_id),
  candidates = candidate_availability,
  eligibility = eligibility_matrix,
  votes = votes_matrix
)

model_varying <- cmdstan_model("R/mnm_varying.stan")

fit_varying <- model_varying$sample(
  data = data_varying,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 02139,
  parallel_chains = 4,
  adapt_delta = 0.95,
  max_treedepth = 15
)

fit_varying$save_object(file = "fits/mnm_varying.rds")

fit_varying <- readRDS("fits/mnm_varying.rds")

fit_varying |> 
  spread_draws(gamma[race_id]) |> 
  mutate(gamma = log(gamma)) |> 
  left_join(races, join_by(race_id)) |> 
  ggplot(aes(x = gamma, y = race)) +
  stat_pointinterval() +
  # scale_x_continuous(limits = c(-5, 5)) +
  theme_bw()

fit_varying |> 
  spread_draws(adjusted_beta[race_id]) |> 
  left_join(races, join_by(race_id)) |> 
  ggplot(aes(x = adjusted_beta, y = race)) +
  stat_pointinterval() +
  # scale_x_continuous(limits = c(-5, 5)) +
  theme_bw()

fit_varying |> 
  spread_draws(alpha[cvr_id]) |> 
  filter(cvr_id < 10) |> 
  ggplot(aes(x = alpha, y = as.character(cvr_id), )) +
  stat_halfeye() +
  # scale_x_continuous(limits = c(-5, 5)) +
  theme_bw()

## 1PL attempt

model_varying_1pl <- cmdstan_model("R/mnm_varying_1pl.stan")

fit_varying_1pl <- model_varying_1pl$sample(
  data = data_varying,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 02139,
  parallel_chains = 4
)

fit_varying_1pl$save_object(file = "fits/mnm_varying_1pl.rds")

ids <- data |> 
  select(cvr_id, race_id, candidate_id) |> 
  arrange(race_id, candidate_id) |> 
  distinct(cvr_id) |> 
  mutate(voter_id = row_number())

fit_varying_1pl |> 
  spread_draws(alpha[voter_id]) |> 
  filter(voter_id < 10) |> 
  left_join(ids) |> 
  ggplot(aes(x = alpha, y = as.character(cvr_id))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  theme_bw()

reference_cats <- data |> 
  arrange(race_id, candidate_id) |> 
  distinct(race, candidate)

fit_varying_1pl |> 
  spread_draws(beta[race_id]) |> 
  ungroup() |> 
  left_join(races, join_by(race_id)) |> 
  ggplot(aes(x = beta, y = race)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  theme_bw()

## New 2PL attempt

model_varying_2pl <- cmdstan_model("R/mnm_varying_2pl.stan")

fit_varying_2pl <- model_varying_2pl$sample(
  data = data_varying,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 02139,
  parallel_chains = 4
)

fit_varying_2pl$save_object(file = "fits/mnm_varying_2pl.rds")

ids <- data |> 
  select(cvr_id, race_id, candidate_id, party_detailed) |> 
  arrange(race_id, candidate_id) |> 
  distinct(cvr_id) |> 
  mutate(voter_id = row_number())

pres_votes <- data |> 
  select(cvr_id, race, candidate, party_detailed) |> 
  filter(race == "US PRESIDENT - STATEWIDE") |> 
  select(cvr_id, candidate)

fit_varying_2pl |> 
  spread_draws(alpha[voter_id]) |> 
  filter(voter_id %% 10 == 0) |> 
  left_join(ids) |> 
  ggplot(aes(x = alpha, y = as.character(cvr_id))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  theme_bw()

fit_varying_2pl |> 
  spread_draws(alpha[voter_id]) |> 
  left_join(ids) |> 
  left_join(pres_votes, by = "cvr_id") |> 
  filter(candidate %in% c("DONALD J TRUMP", "JOSEPH R BIDEN")) |> 
  ggplot(aes(x = alpha, color = candidate, fill = candidate)) +
  stat_halfeye(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Latent Dimension 1", y = "") +
  scale_y_continuous(breaks = NULL) +
  theme_medsl() +
  scale_fill_discrete(type = c("#F6573E", "#3791FF")) +
  scale_color_discrete(type = c("#F6573E", "#3791FF")) +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
  )

reference_cats <- data |> 
  arrange(race_id, candidate_id) |> 
  distinct(race, candidate)

fit_varying_2pl |> 
  spread_draws(beta[race_id]) |> 
  ungroup() |> 
  left_join(races, join_by(race_id)) |> 
  ggplot(aes(x = beta, y = race)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  theme_bw()

fit_varying_2pl |> 
  spread_draws(gamma[race_id]) |> 
  ungroup() |> 
  left_join(races, join_by(race_id)) |> 
  ggplot(aes(x = gamma, y = race)) +
  stat_halfeye() +
  scale_x_continuous(limits = c(0, 10)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  theme_bw()

fits <- c("2PL" = fit_varying_2pl)

tibble(fit = fits, fit_name = names(fits)) |> 
  mutate(rhat = map(fit, ~ select(summarise_draws(.x), rhat))) |> 
  select(-fit) |> 
  unnest(cols = rhat) |> 
  ggplot(aes(x = rhat)) +
  geom_dots() + 
  geom_vline(xintercept = 1, color = "blue", linetype = "dashed") +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~ fit_name, scales = "free_x", ncol=2) +
  labs(x = expression(hat(R)), y = "") +
  theme_bw()

## Optimized 2PL

model_varying_2pl_optimized <- cmdstan_model("R/mnm_varying_2pl_optimized.stan", compile = FALSE)
model_varying_2pl_optimized$compile(
  cpp_options = list(stan_threads = TRUE)
)

data_varying_parallel <- data_varying
data_varying_parallel$parallelize <- 1

fit_optim <- model_varying_2pl_optimized$sample(
  data = data_varying_parallel,
  chains = 2,
  iter_warmup = 50,
  iter_sampling = 100,
  seed = 02139,
  parallel_chains = 2,
  threads_per_chain = 2
)
