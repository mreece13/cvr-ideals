#### TESTING FILE ONLY

rm(list=ls())
gc()

library(tidyverse)
library(arrow)
library(brms)
library(tidybayes)
library(bayesplot)

# define only the variables I need to run the analysis, to limit the size of the loaded data
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

# What are the partisan races in the data?
partisan_races <- open_dataset("~/Dropbox (MIT)/Research/hidden-partisanship/data/cvr_qa_main/",
                               partitioning = "state",
                               schema = partial_schema,
                               format = "parquet") |> 
  filter(state == "COLORADO", magnitude == "1", !is.na(office), !is.na(district)) |> 
  filter(party_detailed != "NONPARTISAN") |> 
  distinct(county_name, office, district) |> 
  collect()

contested_races <- open_dataset("~/Dropbox (MIT)/Research/hidden-partisanship/data/cvr_qa_main/",
                               partitioning = "state",
                               schema = partial_schema,
                               format = "parquet") |> 
  filter(state == "COLORADO", magnitude == "1", !is.na(office), !is.na(district)) |> 
  inner_join(partisan_races, by = c("county_name", "office", "district")) |> 
  distinct(county_name, office, district, candidate) |> 
  arrange(county_name, office, district) |> 
  collect() |> 
  filter(n() > 1, .by = c(county_name, office, district)) |> 
  distinct(county_name, office, district)

## SIMPLE STEP 1
# Subset to only partisan races, then outcome is a binary for whether the voter picked the Democratic candidate
# Also randomly select a set of voters
data_simple1 <- open_dataset("~/Dropbox (MIT)/Research/hidden-partisanship/data/cvr_qa_main/",
                  partitioning = "state",
                  schema = partial_schema,
                  format = "parquet") |> 
  filter(state == "COLORADO", magnitude == "1", !is.na(office), !is.na(district)) |> 
  select(-magnitude) |> 
  inner_join(partisan_races,  by = c("county_name", "office", "district")) |> 
  inner_join(contested_races, by = c("county_name", "office", "district")) |> 
  mutate(choice_dem = as.numeric(party_detailed == "DEMOCRAT"),
         choice_rep = as.numeric(party_detailed == "REPUBLICAN")) |> 
  filter(as.numeric(cvr_id) %% 100 == 0) |> 
  collect() |> 
  mutate(district = if_else(office %in% c("US HOUSE", "STATE SENATE", "STATE HOUSE"), 
                            str_remove(district, str_c(", ", county_name)),
                            district)) 

## START WITH JUST ONE COUNTY
data_simple1_adams <- filter(data_simple1, county_name == "ADAMS",
                             as.numeric(cvr_id) %% 350 == 0)

random_10_adams <- slice_sample(distinct(data_simple1_adams, cvr_id), n=10)

# Here, 1PL Rasch Model with intercept and random effects for nested districts within offices
# also include random effect for individuals
form_simple1 <- bf(choice_rep ~ (1 | office/district) + (1 | cvr_id))

fit_simple1 <- brm(
  formula = form_simple1,
  family = bernoulli(),
  data = data_simple1_adams,
  backend = "cmdstanr",
  chains = 4,
  # threads = threading(threads = 12),
  cores = 4,
  seed = 02139,
  file = "fits/partisanOnly_choiceRep_rasch_colorado_adams",
  file_refit = "on_change",
  silent = 0,
  control = list(adapt_delta = 0.95)
)

summary(fit_simple1)

# plot trace plots
mcmc_trace(fit_simple1, regex_pars = "^b|^sd_")
ggsave("figs/partisanOnly_choiceRep_rasch_colorado_adams_trace.jpeg", width = 16, height = 16, units = "in")

fit_simple1 |> 
  spread_draws(r_cvr_id[cvr_id, dimension]) |> 
  mutate(cvr_id = as.character(cvr_id)) |> 
  inner_join(random_10_adams) |> # only show 10 ppl
  ggplot(aes(y = cvr_id, x = r_cvr_id)) +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  scale_x_continuous(limits = c(-20, 20)) +
  stat_halfeye() +
  theme_bw()

ggsave("figs/partisanOnly_choiceRep_rasch_colorado_adams_voters.jpeg", width = 8, height = 12, units = "in")

# Here, 2PL Model with intercept and random effects for nested districts within offices
# also include random effect for individuals.
# most important formulation is the addition of a discrimination parameter

form_simple2 <- bf(choice_rep ~ exp(logalpha) * eta,
                   eta ~ 1 + (1 | office/district) + (1 | cvr_id),
                   logalpha ~ 1 + (1 | office/district),
                   nl = TRUE)

fit_simple2 <- brm(
  formula = form_simple2,
  family = bernoulli(),
  data = data_simple1_adams,
  backend = "cmdstanr",
  chains = 4,
  # threads = threading(threads = 12),
  cores = 4,
  seed = 02139,
  file = "fits/partisanOnly_choiceRep_2pl_colorado_adams",
  file_refit = "on_change",
  silent = 0,
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

(draw_summaries <- summarise_draws(fit_simple2))

# plot rhats for both Rasch and 2PL
draw_summaries |> 
  ggplot(aes(x = rhat, y = "2PL")) +
  geom_dots() + 
  geom_dots(data = summarise_draws(fit_simple1), aes(y = "Rasch")) +
  geom_vline(xintercept = 1, color = "blue", linetype = "dashed") +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.001)) +
  labs(x = expression(hat(R)), y = "") +
  theme_medsl()

ggsave("figs/partisanOnly_choiceRep_2pl_colorado_adams_rhat.jpeg", width = 12, height = 8, units = "in")

# plot trace plots
mcmc_trace(fit_simple2, regex_pars = "^b_|^sd_")
ggsave("figs/partisanOnly_choiceRep_2pl_colorado_adams_trace.jpeg", width = 16, height = 16, units = "in")

fit_simple2 |> 
  spread_draws(r_cvr_id__eta[cvr_id, dimension]) |> 
  mutate(cvr_id = as.character(cvr_id)) |> 
  inner_join(random_10_adams) |> # only show 10 ppl
  ggplot(aes(y = cvr_id, x = r_cvr_id__eta)) +
  stat_halfeye() +
  scale_x_continuous(limits = c(-10, 10)) +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  labs(x = "Ideal Point Estimate", y = "Voter ID") +
  theme_medsl()

ggsave("figs/partisanOnly_choiceRep_2pl_colorado_adams_voters.jpeg", width = 8, height = 12, units = "in")

fit_simple2 |> 
  spread_draws(r_office__logalpha[office, dimension], r_office__eta[office, dimension]) |> 
  pivot_longer(cols = starts_with("r_")) |> 
  mutate(value = if_else(value > quantile(value, 0.99, na.rm = TRUE), NA, value)) |> 
  mutate(value = if_else(value < quantile(value, 0.01, na.rm = TRUE), NA, value)) |> 
  ungroup() |> 
  mutate(name = case_match(
    name,
    "r_office__logalpha" ~ "Discrimination",
    "r_office__eta" ~ "Easiness",
  )) |> 
  ggplot(aes(x = value, y = office)) +
  stat_halfeye() + 
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  facet_wrap(~ name, scales = "free_x") +
  theme_medsl()

ggsave("figs/partisanOnly_choiceRep_2pl_colorado_adams_office.jpeg", width = 12, height = 8, units = "in")


## Extension to have more than one county

data_simple1_smaller <- filter(data_simple1, as.numeric(cvr_id) %% 575 == 0)
random_10 <- slice_sample(distinct(data_simple1_smaller, cvr_id), n=10)

# Here, 1PL Rasch Model with intercept and random effects for nested districts within offices
# also include random effect for individuals

fit_rasch_colorado <- brm(
  formula = bf(choice_rep ~ county_name + (1 | office/district) + (1 | cvr_id)),
  family = bernoulli(),
  data = data_simple1_smaller,
  backend = "cmdstanr",
  chains = 4,
  # threads = threading(threads = 12),
  cores = 4,
  seed = 02139,
  file = "fits/partisanOnly_choiceRep_rasch_colorado",
  file_refit = "on_change",
  silent = 0,
  control = list(adapt_delta = 0.95)
)

summary(fit_rasch_colorado)

# plot trace plots
mcmc_trace(fit_rasch_colorado, regex_pars = "^b|^sd_")
ggsave("figs/partisanOnly_choiceRep_rasch_colorado_trace.jpeg", width = 16, height = 16, units = "in")

fit_rasch_colorado |> 
  spread_draws(r_cvr_id[cvr_id, dimension]) |> 
  mutate(cvr_id = as.character(cvr_id)) |> 
  inner_join(random_10) |> # only show 10 ppl
  ggplot(aes(y = cvr_id, x = r_cvr_id)) +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  stat_halfeye() +
  theme_bw()

ggsave("figs/partisanOnly_choiceRep_rasch_colorado_voters.jpeg", width = 8, height = 12, units = "in")

fit_rasch_colorado |> 
  spread_draws(r_cvr_id[cvr_id, dimension]) |> 
  ggplot(aes(x = r_cvr_id)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  theme_medsl()

ggsave("figs/partisanOnly_choiceRep_rasch_colorado_voters_all.jpeg", width = 8, height = 8, units = "in")
