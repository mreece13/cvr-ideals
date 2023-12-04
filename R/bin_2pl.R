library(tidyverse)
library(brms)
library(arrow)

options("brms.threads" = 20)
options("mc.cores" = 4)
options("brms.backend" = "cmdstanr")
options("future" = FALSE)

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

base_data <- open_dataset("../cvrs/data/cvr_qa_main",
                          partitioning = "state",
                          schema = partial_schema,
                          format = "parquet") |> 
  filter(state == "COLORADO", magnitude == "1", !is.na(office), !is.na(district)) |> 
  select(-magnitude)

# What are the contested races?
contested_races <- base_data |> 
  distinct(county_name, office, district, candidate) |> 
  arrange(county_name, office, district) |> 
  collect() |> 
  filter(n() > 1, .by = c(county_name, office, district)) |> 
  distinct(county_name, office, district)

# What are the partisan races in the data?
partisan_races <- base_data |> 
  filter(party_detailed != "NONPARTISAN") |> 
  distinct(county_name, office, district) |> 
  collect()

contested_races <- inner_join(contested_races, partisan_races)

# pick some random people
randoms <- base_data |>
  distinct(county_name, cvr_id) |>
  collect() |> 
  slice_sample(n=250)

data_colorado <- base_data |> 
  inner_join(randoms, by = c("county_name", "cvr_id")) |>
  inner_join(contested_races, by = c("county_name", "office", "district")) |> 
  mutate(choice_dem = as.numeric(party_detailed == "DEMOCRAT"),
         choice_rep = as.numeric(party_detailed == "REPUBLICAN")) |> 
  collect() |> 
  mutate(district = if_else(office %in% c("COUNTY CLERK", "COUNTY COMMISSIONER",
                                          "COUNTY SHERIFF", "COUNTY TREASURER"), 
                            str_c(county_name, district, sep = " ") |> str_squish(), 
                            district)) |> 
  mutate(district = str_remove(district, fixed(","))) |> 
  mutate(district = str_squish(district)) |> 
  mutate(race = str_c(office, district, sep = " - ")) |> 
  filter(!(office == "US HOUSE" & district == "STATEWIDE"), 
         !(office == "US HOUSE" & district == "57"),
         !(office == "US SENATE" & district == "8"))

form_2pl <- bf(
  choice_rep ~ gamma * alpha - beta,
  nl = TRUE,
  alpha ~ 0 + (1 | cvr_id),
  beta ~ 1 + (1 |i| race),
  gamma ~ 1 + (1 |i| race),
  family = brmsfamily("categorical", link = "logit")
)

get_prior(form_2pl, data = data_colorado)
make_stancode(form_2pl, data = data_colorado, prior = prior_2pl)


prior_2pl <-
  prior("normal(0, 2)", class = "b", nlpar = "beta") +
  prior("normal(0, 1)", class = "b", nlpar = "gamma") +
  prior("normal(0, 1)", class = "sd", group = "cvr_id", nlpar = "alpha") +
  prior("normal(0, 3)", class = "sd", group = "race", nlpar = "beta") +
  prior("normal(0, 1)", class = "sd", group = "race", nlpar = "gamma")

# Add More Counties to 2PL
brm(
  formula = form_2pl,
  # prior = prior_2pl,
  data = data_colorado,
  chains = 4,
  iter = 0,
  file = "fits/bin_2pl_colorado",
  file_refit = "on_change",
  sample_prior = TRUE,
  seed = 02139,
  silent = 0
)

make_stancode(form_2pl, data_colorado)

## Load DIME scores

# dime <- read_csv("data/dime_recipients_1979_2020.csv") |> 
#   filter(str_detect(election, "2020"), (state == "CO" | seat == "federal:president")) |> 
#   select(name, lname, ffname, fname, mname, party, seat, district, recipient.cfscore) |> 
#   mutate(office = case_match(
#     seat,
#     "federal:house" ~ "US HOUSE",
#     "federal:senate" ~ "US SENATE",
#     "federal:president" ~ "US PRESIDENT",
#     "state:upper" ~ "STATE SENATE",
#     "state:lower" ~ "STATE HOUSE",
#     "local:other" ~ NA,
#     "state:treasurer" ~ NA,
#     "state:governor" ~ NA,
#     "local:district-attorney" ~ NA,
#     "state:attorneyg" ~ NA,
#     "state:education" ~ "BOARD OF EDUCATION",
#     "state:office" ~ NA,
#     "federal:committee" ~ NA,
#     "federal:527" ~ NA,
#     "state:ballot" ~ NA
#   )) |> 
#   drop_na(office) |> 
#   mutate(district = case_when(
#     office %in% c("US PRESIDENT", "US SENATE") ~ "STATEWIDE",
#     office == "US HOUSE" ~ str_remove(district, "CO"),
#     office %in% c("STATE HOUSE", "STATE SENATE", "BOARD OF EDUCATION") ~ str_remove(district, "CO-"),
#     .default = NA
#   )) |> 
#   mutate(party_detailed = case_match(
#     party,
#     "100" ~ "DEMOCRAT",
#     "200" ~ "REPUBLICAN",
#     .default = NA
#   ))

# dime |> 
#   write_csv("data/dime_colorado.csv")
# 
# data_colorado |> 
#   distinct(office, district, candidate, party_detailed) |> 
#   left_join(dime, join_by(office, district, party_detailed), "one-to-one") |> 
#   write_csv("data/dime_raw_matches.csv")

candidates_dime <- read_csv("~/cvrs/data/dime_matches.csv") |> 
  distinct(office, district, candidate, .keep_all = TRUE) |> 
  select(-party_detailed)

data_colorado_dime <- data_colorado |> left_join(candidates_dime, join_by(office, district, candidate))

form_1pl <- bf(
  choice_rep ~ mi(recipient.cfscore) + (1 | race) + (1 | cvr_id),
  family = brmsfamily("bernoulli", link = "logit")
) +
  bf(
    recipient.cfscore | mi() ~ 1,
    family = brmsfamily("gaussian")
  ) +
  set_rescor(FALSE)

prior_1pl <-
  prior("normal(0, 2)", class = "Intercept") +
  prior("normal(0, 3)", class = "sd", group = "cvr_id") +
  prior("normal(0, 3)", class = "sd", group = "race")

brm(
  formula = form_1pl,
  # prior = prior_1pl,
  data = data_colorado_dime,
  chains = 4,
  iter = 2000,
  file = "fits/bin_1pl_colorado_dime",
  file_refit = "on_change",
  seed = 02139,
  silent = 0
)
