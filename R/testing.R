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
library(factor.switching)

source("../medsl_theme.R")

###################################

cat_2pl <- readRDS("fits/cat_2pl_streamlinednumV7448_full.rds") |> 
  spread_draws(gamma[id], beta[id])

var <- str_replace(colnames(cat_2pl), ".+\\[([0-9]+)\\]$", "\\1")
var <- as.integer(var)
dim <- rep(1, length(var))

colnames(cat_2pl) <- str_c("LambdaV", var, "_", dim)

sds <- readRDS("fits/cat_2pl_streamlinednumV7448_full.rds") |> 
  spread_draws(alpha[cvr_id]) |> 
  ungroup() |> 
  summarise(sd = sd(alpha), .by = ".draw") |> 
  pull(sd)

out <- rsp_exact(
  lambda_mcmc = cat_2pl,
  rotate = TRUE,
  maxIter = 500,
  # threshold = 1e-6,
  # sa_loops = 10,
  verbose = TRUE
)

data <- tar_read(data_adams)

ids <- data |> 
  filter(candidate != "undervote") |> 
  distinct(race, candidate) |> 
  arrange(race, candidate) |> 
  group_by(race) |> 
  mutate(candidate_id = 1:n(),
         race_id = cur_group_id()) |> 
  ungroup() |> 
  mutate(id = 1:n())

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

o <- draws |> 
  filter(str_detect(race, "US PRESIDENT")) |> 
  mutate(value = ifelse(candidate == "BILL HAMMONS", 0, value)) |> 
  summarize(mean = abs(mean(value)), .by = candidate) |> 
  arrange(mean) |> 
  pull(candidate)

p1 <- draws |> 
  filter(str_detect(race, "US PRESIDENT")) |> 
  mutate(value = ifelse(candidate == "BILL HAMMONS", 0, value)) |> 
  ggplot(aes(x = value, y = order(candidate, o))) +
  stat_halfeye(normalize = "xy") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = "", y = "", title = "Discrimination Parameter after RSP-Exact Algorithm") +
  theme_bw()

rsp <- tibble(
  signs = out$sign_vectors[1],
  permute = out$permute_vectors[1],
  .draw = 1:length(out$permute_vectors)
)

draws_signed <- cat_2pl |> 
  left_join(rsp, join_by(.draw)) |> 
  mutate(gamma = gamma*signs*permute) |> 
  left_join(ids, join_by(id)) |> 
  ungroup() |> 
  select(race, candidate, beta, gamma)

draws_signed |> 
  filter(str_detect(race, "US PRESIDENT")) |> 
  pivot_longer()

sds2 <- sds |> 
  as_tibble_col() |> 
  mutate(.draw = 1:n())

draws_old <- cat_2pl |>
  left_join(sds2, join_by(.draw)) |> 
  mutate(gamma = gamma*value) |> 
  select(-starts_with(".")) |> 
  left_join(ids, join_by(id)) |> 
  ungroup() |> 
  select(race, candidate, gamma, beta) |> 
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

draws_old |> 
  filter(str_detect(race, "US PRESIDENT")) |> 
  mutate(value = ifelse(candidate == "BILL HAMMONS", 0, gamma),
         type = "Before RSP") |>
  bind_rows(
    filter(draws, str_detect(race, "US PRESIDENT")) |> 
    mutate(value = ifelse(candidate == "BILL HAMMONS", 0, value), 
           type = "After RSP")
  ) |> 
  mutate(type = fct_relevel(type, "Before RSP")) |> 
  ggplot(aes(x = value, y = candidate)) +
  stat_halfeye(normalize = "xy") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  facet_wrap(~ type) +
  labs(x = "", y = "", title = "Discrimination Parameter") +
  theme_medsl() +
  theme(
    panel.grid.major.x = element_blank()
  )

ggsave("figs/rsp_compare.jpg", width = 12, height = 8, units = "in")

####################

contests <- read_csv("../cvrs/code/util/contests.csv") |> 
  filter(state == "COLORADO")

base_data <- open_dataset("../cvrs/data/pass1/",
                          partitioning = c("state", "county_name"),
                          schema = partial_schema,
                          format = "parquet") |> 
  filter(state == "COLORADO", magnitude == "1", !is.na(office), !is.na(district), candidate != "undervote") |> 
  select(-magnitude) |> 
  mutate(race = str_c(office, district, sep = " - "))

small_candidates <- base_data |> 
  count(race, candidate) |> 
  filter(n <= 20) |> 
  distinct(race, candidate)

contested_races <- base_data |> 
  anti_join(small_candidates) |> 
  distinct(race, candidate) |> 
  count(race) |> 
  filter(n > 1) |> 
  select(race)

partisan_races <- base_data |> 
  distinct(race, party_detailed) |>
  filter(party_detailed != "NONPARTISAN") |> 
  distinct(race)

contested_races <- inner_join(contested_races, partisan_races)

randoms <- base_data |>
  distinct(county_name, cvr_id) |>
  collect() |> 
  slice_sample(n=1000)

base <- base_data |> 
  inner_join(randoms, by = c("county_name", "cvr_id")) |>
  inner_join(contested_races, by = c("race")) |> 
  filter(!is.na(party_detailed)) |> 
  mutate(choice_rep = as.numeric(party_detailed == "REPUBLICAN")) |> 
  collect()

form <- bf(
  choice_rep ~ exp(loggamma) * alpha - beta,
  nl = TRUE,
  alpha ~ 0 + (1 | cvr_id),
  beta ~ 1 + (1 | race),
  loggamma ~ 0 + (1 | race),
  family = brmsfamily("bernoulli", link = "logit")
)

priors <-
  prior("normal(0, 2)", class = "b", nlpar = "beta") +
  prior("normal(0, 1)", class = "b", nlpar = "loggamma") +
  prior("normal(0, 1)", class = "sd", group = "cvr_id", nlpar = "alpha") +
  prior("normal(0, 3)", class = "sd", group = "race", nlpar = "beta") +
  prior("normal(0, 1)", class = "sd", group = "race", nlpar = "loggamma")

fit <- brm(
  formula = form,
  # prior = priors,
  data = base,
  chains = 4,
  cores = 2,
  iter = 2000,
  seed = 02139,
  silent = 0,
  # opencl = c(0, 0),
  file = "fits/bernoulli_2pl_TEST",
  file_refit = "on_change"
)


