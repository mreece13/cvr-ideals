rm(list=ls())
gc()

library(patchwork)
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(targets)

source("../medsl_theme.R")

ber_1pl <- readRDS("fits/bernoulli_rasch.rds")
ber_2pl <- readRDS("fits/bernoulli_2pl.rds")

signs <- readRDS("fits/cat_2pl.rds") |> 
  as_draws_df() |>
  select(matches("^gamma\\[")) |>
  rowMeans() |>
  sign()

cat_2pl <- readRDS("fits/cat_2pl.rds") |> 
  as_draws_df() |>
  mutate(across(matches("(^alpha\\[)|(^gamma\\[)"), ~ signs * .))

## binary results - ideals

p1 <- ber_1pl |>
  spread_draws(r_cvr_id[cvr_id, var]) |>
  mutate(alpha = r_cvr_id/sd(r_cvr_id)) |>
  filter(cvr_id < 20) |> 
  ggplot(aes(x = alpha, y = as.character(cvr_id))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = expression(alpha), y = "Voter", title = "Rasch Model")

p2 <- ber_2pl |> 
  spread_draws(r_cvr_id__alpha[cvr_id,]) |> 
  mutate(alpha = r_cvr_id__alpha/sd(r_cvr_id__alpha)) |> 
  filter(cvr_id < 20) |> 
  ggplot(aes(x = alpha, y = as.character(cvr_id))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = expression(alpha), y = "", title = "2PL Model")

ber_ideal <- p1 + p2 & theme_medsl()

ggsave("figs/ber_ideals.jpg", plot = ber_ideal, width = 10, height = 6, units = "in")

## bernoulli - other params

person_pars_2pl <- ranef(ber_2pl, summary = FALSE)$cvr_id[, , "alpha_Intercept"] 
person_sds_2pl <- apply(person_pars_2pl, 1, sd)
item_pars_2pl <- coef(ber_2pl, summary = FALSE)$race

# locations
beta <- item_pars_2pl[, , "beta_Intercept"] |>
  as_tibble() |> 
  pivot_longer(cols = everything(), names_to = "race") |> 
  mutate(race = str_remove(race, fixed(", ")) |> str_squish()) |> 
  mutate(nlpar = "beta")

random_races <- beta |> 
  distinct(race) |> 
  slice_sample(n=12) |> 
  bind_rows(tibble(race = c("US PRESIDENT - STATEWIDE", "US SENATE - STATEWIDE"))) |> 
  distinct(race)

# slopes
gamma <- item_pars_2pl[, , "loggamma_Intercept"] |>
  exp() |> 
  sweep(1, person_sds_2pl, "*") |>
  as_tibble() |> 
  pivot_longer(cols = everything(), names_to = "race") |> 
  mutate(nlpar = "gamma") |> 
  mutate(race = str_remove(race, fixed(", ")) |> str_squish()) 

ber_params <- bind_rows(beta, gamma) |>
  inner_join(random_races) |> 
  mutate(nlpar = factor(nlpar, labels = c("Difficulty", "Discrimination"))) |>
  ggplot(aes(x = value, y = race)) +
  stat_pointinterval() +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  facet_wrap(~ nlpar, scales = "free_x") +
  labs(y = "", x = "") +
  theme_medsl()

ggsave("figs/ber_params.jpg", plot = ber_params, width = 10, height = 6, units = "in")

## categorical - ideals

cat_ideals <- cat_2pl |> 
  spread_draws(alpha[cvr_id]) |> 
  ungroup() |> 
  mutate(alpha = (alpha - mean(alpha))/sd(alpha)) |> 
  filter(cvr_id < 15) |> 
  ggplot(aes(x = alpha, y = as.character(cvr_id))) +
  stat_halfeye() +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = expression(alpha), y = "Voter") +
  theme_medsl()

ggsave("figs/cat_ideals.jpg", plot = cat_ideals, width = 10, height = 6, units = "in")

## categorical - aggregated

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

pres_choices <- df |>
  filter(office == "US PRESIDENT") |>
  distinct(cvr_id, candidate) |> 
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
  select(cvr_id, race_id, candidate_id) |> 
  arrange(race_id, candidate_id) |> 
  distinct(cvr_id) |> 
  mutate(id = row_number()) |> 
  left_join(pres_choices)

joined <- cat_2pl |> 
  spread_draws(alpha[id]) |> 
  ungroup() |> 
  mutate(alpha = alpha/sd(alpha)) |> 
  left_join(voters, join_by(id)) |> 
  drop_na(candidate) 

cat_aggregated <- joined |> 
  filter(candidate %in% c("JOSEPH R BIDEN", "DONALD J TRUMP")) |> 
  ggplot(aes(x = alpha, fill = candidate, y = candidate)) +
  ggridges::geom_density_ridges(scale = 1) +
  scale_fill_discrete(type = c("#F6573E", "#3791FF"), guide = "none") +
  labs(x = expression(alpha), y = "", fill = "Candidate") +
  theme_medsl()

cat_aggregated_others <- joined |> 
  filter(!(candidate %in% c("JOSEPH R BIDEN", "DONALD J TRUMP"))) |> 
  ggplot(aes(x = alpha, y = candidate)) +
  ggridges::geom_density_ridges(fill = "#C0BA79", scale = 1) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = expression(alpha), y = "") +
  theme_medsl()

ggsave("figs/cat_aggregated.jpg", plot = cat_aggregated, width = 10, height = 6, units = "in")
ggsave("figs/cat_aggregated_others.jpg", plot = cat_aggregated_others, width = 10, height = 6, units = "in")

## categorical - params

sds <- cat_2pl |> 
  spread_draws(alpha[cvr_id]) |> 
  ungroup() |> 
  summarise(sd = sd(alpha), .by = ".draw") |> 
  pull(sd)

draws <- cat_2pl |> 
  as_draws_df() |>
  select(starts_with("beta["), starts_with("gamma[")) |> 
  sweep(1, sds, "*") |> 
  mutate(across(everything(), ~ na_if(.x, 0))) |> 
  pivot_longer(cols = everything(), values_drop_na = TRUE) |> 
  separate_wider_delim(cols = name, delim = "[", names = c("parameter", "race_id")) |> 
  separate_wider_delim(cols = race_id, delim = ",", names = c("race_id", "candidate_id")) |> 
  mutate(candidate_id = str_remove(candidate_id, "]")) |> 
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

p2 <- draws |> 
  filter(str_detect(race, "US PRESIDENT"), 
         candidate %in% c("JOSEPH R BIDEN", "GLORIA LA RIVA", "DONALD J TRUMP", "BILL HAMMONS"),
         parameter == "gamma") |> 
  # mutate(parameter = factor(parameter, labels = c("Difficulty", "Discrimination"))) |>
  bind_rows(tibble(value = 0, race = "US PRESIDENT", candidate = "BILL HAMMONS")) |> 
  ggplot(aes(x = value, y = candidate)) +
  stat_halfeye(normalize = "xy") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  # facet_wrap(~ parameter, scales = "free_x") +
  labs(x = "", y = "", title = "Discrimination Parameter after Sign-Flipping") +
  theme_bw()

ggsave("figs/cat_params.jpg", plot = cat_params, width = 10, height = 6, units = "in")


## dime comparison

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

## Trace plots

trace_1 <- mcmc_trace(ber_1pl, pars = sample(get_variables(ber_1pl), size = 24))
trace_2 <- mcmc_trace(ber_2pl, pars = sample(get_variables(ber_2pl), size = 24))

vars <- summarise_draws(cat_2pl) |> 
  filter(mean > 0.1 | mean < -0.1, !str_detect(variable, "raw")) |> 
  slice_sample(n=24) |> 
  pull(variable)

trace_3 <- mcmc_trace(cat_2pl, pars = vars)

ggsave("figs/ber_1pl_trace.jpeg", trace_1, width = 10, height = 12, units = "in")
ggsave("figs/ber_2pl_trace.jpeg", trace_2, width = 12, height = 12, units = "in")
ggsave("figs/cat_2pl_trace.jpeg", trace_3, width = 10, height = 12, units = "in")

### rhat plots
p_r1 <- tibble(r = rhat(ber_1pl)) |> 
  ggplot(aes(x = r)) +
  geom_dots() +
  scale_y_continuous(labels = NULL) +
  labs(title = "Bernoulli Rasch", x = expression(hat(R)), y = "")

p_r2 <- tibble(r = rhat(ber_2pl)) |> 
  ggplot(aes(x = r)) +
  geom_dots() +
  scale_y_continuous(labels = NULL) +
  labs(title = "Bernoulli 2PL", x = expression(hat(R)), y = "")

p_r3 <- tibble(r = summarise_draws(cat_2pl)$rhat) |> 
  ggplot(aes(x = r)) +
  geom_dots() +
  scale_y_continuous(labels = NULL) +
  labs(title = "Categorical 2PL", x = expression(hat(R)), y = "")

plot <- p_r1 + p_r2 + p_r3 & theme_medsl()

ggsave("figs/rhat_comparison.jpg", plot = plot, width = 10, height = 6, units = "in")



