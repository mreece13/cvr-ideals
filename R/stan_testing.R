rm(list=ls())
gc()

library(tidyverse)
library(tidybayes)
library(bayesplot)
library(cmdstanr)

data <- targets::tar_read(data_base_adams) |> 
  # propositions are not quite right
  mutate(candidate = case_when(
    str_detect(race, "PROPOSITION") ~ str_c(race, candidate, sep = " - "),
    TRUE ~ candidate
  ),
  race = str_remove(race, ", ADAMS")) |> 
  filter(!(race %in% c("COUNTY JUDGE - ADAMS", "COURT OF APPEALS JUDGE - STATEWIDE", 
                       "DISTRICT COURT JUDGE - 17", "SUPREME COURT JUSTICE - STATEWIDE")))

# Assign unique IDs to races and candidates
ids <- data |> 
  distinct(race, candidate) |> 
  arrange(race, candidate) |> 
  group_by(race) |> 
  mutate(candidate_id = 1:n(),
         race_id = cur_group_id())

# Join back to the original data
df <- left_join(data, ids, join_by(race, candidate))

# Create the votes matrix
votes_matrix <- df |> 
  select(cvr_id, race_id, candidate_id) |> 
  arrange(race_id, candidate_id) |>
  pivot_wider(names_from = race_id, values_from = candidate_id, values_fill = 0) |> 
  select(-cvr_id) |> 
  as.matrix()

sizes <- df |> 
  distinct(race, race_id, candidate) |> 
  count(race_id) |> 
  pull(n)

# Prepare data for Stan
stan_data <- list(
  threaded = 1,
  J = n_distinct(df$cvr_id),
  K = max(ids$race_id),
  C = n_distinct(ids$candidate),
  votes = votes_matrix,
  sizes = sizes
)

m <- cmdstan_model("R/cat_2pl_streamlined.stan", compile = FALSE)
m$compile(cpp_options = list(stan_threads = TRUE), force_recompile = FALSE)

fit <- m$sample(
  data = stan_data,
  chains = 1,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 02139,
  parallel_chains = 4,
  threads_per_chain = 4
)

fit <- m$pathfinder(
  data = stan_data,
  seed = 02139
)

fit$save_object("fits/cat_2pl_streamlined2.rds")

fit <- read_rds("fits/cat_2pl_streamlined.rds")

## graphing

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
  mutate(id = 1:n()) |> 
  left_join(pres_choices)

joined <- fit |> 
  spread_draws(alpha[id], ndraws = 1) |> 
  ungroup() |> 
  mutate(alpha = alpha/sd(alpha)) |> 
  left_join(voters, join_by(id)) |> 
  drop_na(candidate) 

joined |> 
  filter(candidate %in% c("JOSEPH R BIDEN", "DONALD J TRUMP")) |> 
  # mutate(alpha = alpha*-1) |> 
  ggplot(aes(x = alpha, fill = candidate, y = candidate)) +
  ggridges::geom_density_ridges(scale = 1, panel_scaling = FALSE) +
  scale_fill_discrete(type = c("#F6573E", "#3791FF"), guide = "none") +
  labs(x = expression(alpha), y = "", fill = "Candidate") +
  theme_medsl()

joined |> 
  filter(!(candidate %in% c("JOSEPH R BIDEN", "DONALD J TRUMP"))) |> 
  filter(n() > 5, .by = candidate) |> 
  mutate(alpha = alpha*-1) |> 
  ggplot(aes(x = alpha, y = candidate)) +
  ggridges::geom_density_ridges(fill = "#C0BA79", scale = 1, panel_scaling = FALSE) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = expression(alpha), y = "") +
  theme_medsl()

sds <- fit |> 
  spread_draws(alpha[cvr_id], ndraws = 1) |> 
  ungroup() |> 
  summarise(sd = sd(alpha), .by = ".draw") |> 
  pull(sd)

params <- fit |> 
  posterior::as_draws_df() |> 
  slice_head(n=1) |> 
  select(starts_with("beta["), starts_with("gamma[")) |> 
  sweep(1, sds, "*") |> 
  pivot_longer(cols = everything()) |> 
  bind_cols(bind_rows(ids, ids)) |> 
  mutate(name = str_remove(name, "\\[.*?$")) |> 
  select(name, race, candidate, value)

p <- params |> 
  filter(str_detect(race, "US PRESIDENT")) |> 
  ggplot(aes(x = value, y = candidate)) +
  geom_point() +
  facet_wrap(~ name) +
  theme_bw()

ggsave("figs/params.jpeg", plot = p, width = 8, height = 8, units = "in")
