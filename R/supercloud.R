library(tidyverse)
library(targets)
library(cmdstanr)
library(arrow)

data = tar_read(data_adams)

# Assign unique IDs to races and candidates
ids <- data |> 
  filter(candidate != "UNDERVOTE") |> 
  count(race, candidate) |> 
  arrange(race, desc(n)) |> 
  mutate(
    candidate_id = 1:n(),
    race_id = cur_group_id(),
    .by = race
  )

df <- data |> 
  filter(candidate != "UNDERVOTE") |> 
  arrange(race, candidate) |> 
  mutate(
    combo = paste(candidate, race, sep = "##")
  ) |> 
  summarize(
    group = str_flatten(combo, collapse = "||"),
    .by = c(state, county_name, cvr_id)
  ) |> 
  count(state, county_name, group, name = "n_group") |> 
  mutate(
    group_id = 1:n()
  ) |> 
  separate_longer_delim(cols = group, delim = "||") |> 
  separate_wider_delim(cols = group, delim="##", names = c("candidate", "race")) |> 
  left_join(ids, join_by(race, candidate))

# Create the votes matrix
votes_matrix <- df |> 
  select(county_name, group_id, race_id, candidate_id, n) |> 
  arrange(race_id, desc(n)) |>
  select(-n) |> 
  pivot_wider(names_from = race_id, values_from = candidate_id, values_fill = 0) |> 
  select(-county_name, -group_id) |> 
  as.matrix()

num_cands <- df |> 
  distinct(race, race_id, candidate) |> 
  count(race_id) |> 
  pull(n)

group_sizes <- df |> 
  distinct(group_id, n_group) |> 
  pull(n_group)

# Prepare data for Stan
stan_data <- list(
  N_groups = df |> distinct(county_name, group_id) |> tally() |> pull(),
  N_contests = n_distinct(ids$race),
  N_cands = length(ids$candidate),
  votes = votes_matrix,
  sizes = num_cands,
  group_sizes = group_sizes
)

m <- cmdstan_model("R/cat_2pl_grps.stan", compile = TRUE)

fit <- m$sample(
  data = stan_data,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 02139,
  parallel_chains = 4,
  refresh = 100,
)

path <- str_c("fits/cat_2pl_grps_numV", as.character(stan_data$N_groups), ".rds")

fit$save_object(path)