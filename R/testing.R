#### TESTING FILE ONLY

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

base <- open_dataset("~/cvrs/data/cvr_qa_main",
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
  ))

## Filter to top 2 candidates in each race
top_cands <- base |> 
  count(race, candidate) |> 
  arrange(race, desc(n)) |> 
  collect() |> 
  slice_head(n=2, by = race) |> 
  select(-n)

base <- inner_join(base, top_cands)

races <- base |> 
  distinct(race) |> 
  arrange(race) |> 
  collect() |> 
  mutate(race_id = row_number())

candidates <- base |> 
  distinct(candidate) |> 
  arrange(candidate) |> 
  collect() |> 
  mutate(candidate_id = row_number())

# Create the candidate availability matrix
candidate_availability <- base |> 
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

# some races are not classified perfectly in districts rn so they would show up as list-columns (bad)
bad_races <- base |> 
  left_join(races, by = "race") |> 
  left_join(candidates, by = "candidate") |> 
  count(cvr_id, race_id) |> 
  filter(n > 1) |> 
  distinct(race_id) |> 
  collect() |> 
  pull(race_id)

randos <- base |> 
  distinct(cvr_id) |> 
  collect() |> 
  slice_sample(n=100)

df <- base |> 
  inner_join(randos) |> 
  left_join(races, by = "race") |> 
  left_join(candidates, by = "candidate") |> 
  filter(!(race_id %in% bad_races)) |>
  collect() |>
  drop_na(race_id, candidate_id)

# Create the votes matrix
votes_matrix <- df |> 
  select(cvr_id, race_id, candidate_id) |> 
  arrange(race_id, candidate_id) |>
  collect() |> 
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
   randos, top_cands, votes_matrix, bad_races, missing, partial_schema)
gc()

## Optimized 2PL

model <- cmdstan_model("R/mnm_varying_2pl_optimized.stan", compile = FALSE)
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
  threads_per_chain = 16
)

fit$save_object("fits/cat_2pl.rds")

## Fit Diagnostics

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
