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

ber_1pl <- readRDS("fits/bernoulli_1pl_TEST.rds")
ber_2pl <- readRDS("fits/bernoulli_2pl_TEST.rds")

cat_2pl <- readRDS("fits/cat_2pl_streamlinednumV7535_full.rds") |> 
  as_draws_matrix() |> 
  subset_draws(variable = "gamma")

d <- tibble(r = brms::rhat(ber_1pl), type = "Bernoulli 1-Parameter") |> 
  bind_rows(
    tibble(r = brms::rhat(ber_2pl), type = "Bernoulli 2-Parameter")
  ) |> 
  bind_rows(
    tibble(r = summarise_draws(cat_2pl)$rhat, type = "Categorical 2-Parameter")
  )

datasummary(r * type ~ Mean + Median + Max, data = d)

######################

var <- str_replace(colnames(cat_2pl), ".+\\[([0-9]+)\\]$", "\\1")
var <- as.integer(var)
dim <- rep(1, length(var))

colnames(cat_2pl) <- str_c("LambdaV", var, "_", dim)

sds <- readRDS("fits/cat_2pl_streamlinednumV7535_full.rds") |> 
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
  distinct(race, candidate, party_detailed) |> 
  arrange(race, candidate) |> 
  group_by(race) |> 
  mutate(candidate_id = 1:n(),
         race_id = cur_group_id()) |> 
  ungroup() |> 
  mutate(id = 1:n())

# draws <- as_draws_df(out$lambda_reordered_mcmc) |>
#   sweep(1, sds, "*") |> 
#   select(-starts_with(".")) |> 
#   pivot_longer(cols = everything(), values_drop_na = TRUE) |> 
#   mutate(name = str_remove_all(name, "LambdaV|\\_1") |> as.numeric()) |> 
#   left_join(mutate(ungroup(ids), name = 1:n()), by = "name") |> 
#   select(-candidate_id, -race_id, -name) |>  
#   mutate(candidate = str_squish(candidate)) |> 
#   mutate(candidate = case_match(
#     candidate,
#     "JOSEPH KISHORE NORISSA SANTA CRUZ" ~ "JOSEPH KISHORE",
#     "JORDAN CANCER SCOTT JENNIFER TEPOOL" ~ "JORDAN SCOTT",
#     "BILL HAMMONS ERIC BODENSTAB" ~ "BILL HAMMONS",
#     "MARK CHARLES ADRIAN WALLACE" ~ "MARK CHARLES",
#     "PRINCESS KHADIJAH MARYAM JACOB FAMBRO KHADIJAH MARYAM JACOB SR" ~ "PRINCESS KHADIJAH JACOB-FAMBRO",
#     "KYLE KENLEY KOPITKE NATHAN RE VO SORENSON" ~ "KYLE KENLEY",
#     "JOE MC HUGH ELIZABETH STORM" ~ "JOE MCHUGH",
#     "BLAKE HUBER FRANK ATWOOD" ~ "BLAKE HUBER",
#     "PHIL COLLINS BILLY JOE PARKER" ~ "PHIL COLLINS",
#     "GLORIA LA RIVA SUNIL FREEMAN" ~ "GLORIA LA RIVA",
#     .default = candidate
#   ))

rsp <- tibble(
  signs = out$sign_vectors[1],
  permute = out$permute_vectors[1],
  .draw = 1:length(out$permute_vectors)
)

sds2 <- sds |> 
  as_tibble_col() |> 
  mutate(.draw = 1:n())

draws_old <- readRDS("fits/cat_2pl_streamlinednumV7535_full.rds") |> 
  spread_draws(gamma[id], beta[id]) |> 
  left_join(sds2, join_by(.draw)) |> 
  mutate(gamma = gamma/value) |> 
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

signs <- readRDS("fits/cat_2pl_streamlinednumV7535_full.rds") |> 
  spread_draws(gamma[id]) |> 
  summarize(sign = sign(mean(gamma)))

draws_signed <- readRDS("fits/cat_2pl_streamlinednumV7535_full.rds") |> 
  spread_draws(gamma[id]) |> 
  left_join(signs, join_by(id)) |>
  left_join(sds2, join_by(".draw")) |> 
  mutate(gamma = abs(gamma)*sign/value) |>
  left_join(ids, join_by(id)) |> 
  ungroup() |> 
  select(race, candidate, party_detailed, gamma)

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

ggsave("figs/rsp_compare.jpg", width = 12, height = 12, units = "in")

base <- draws_signed |> 
  filter(!(mean(gamma) == 0), .by = c(race, candidate)) |>
  mutate(race_type = str_remove_all(race, " -.*?$|"),
         race = str_remove(race, "- STATEWIDE - "),
         race = str_remove(race, race_type),
         race = str_remove_all(race, "^- |^ - "),
         candidate = case_match(
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
         )
         )

base |> 
  filter(party_detailed == "NONPARTISAN") |> 
  # filter(race_type != "PROPOSITION") |> 
  ggplot(aes(x = gamma, y = interaction(race, candidate, sep = " - "))) +
  stat_halfeye(geom = "pointinterval") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  facet_grid(~ race_type, scales = "free_x", space = "free", labeller = labeller(race_type = label_wrap_gen(5))) +
  # ggforce::facet_row(facets = vars(race_type), scales = "free_x", space = "free", labeller = labeller(groupwrap = label_wrap_gen(5))) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_medsl() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.spacing.x = unit(15, units = "points"),
    strip.text.x = element_text(margin = margin(b = "10")),
    strip.clip = "off"
  ) +
  coord_flip() +
  xlim(c(-2, 5))

ggsave("figs/nonpartisan_disc_slides.jpg", width = 12, height = 8, units = "in")

draws_signed |> 
  filter(party_detailed != "NONPARTISAN") |> 
  mutate(race_type = str_remove_all(race, " -.*?$|"),
         race = str_remove(race, race_type),
         race = str_remove_all(race, "- STATEWIDE - |FEDERAL|COLORADO"),
         race = str_remove_all(race, "^- |^ - "),
         y = str_c(race, candidate, sep = " - ") |> str_remove("^ -") |> as.factor()
  ) |> 
  ggplot(aes(x = gamma, y = y)) +
  stat_halfeye(geom = "pointinterval") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  ggforce::facet_col(facets = vars(race_type), scales = "free", space = "free") +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_medsl() +
  theme(
    panel.grid.major.x = element_blank()
  )

ggsave("figs/partisan_disc.jpg", width = 10, height = 16, units = "in")

## 

base |> 
  mutate(partisan = ifelse(party_detailed == "NONPARTISAN", "NONPARTISAN", "PARTISAN")) |> 
  # filter(party_detailed == "NONPARTISAN") |> 
  ggplot(aes(x = abs(gamma), y = race_type)) +
  stat_halfeye(geom = "pointinterval") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  ggforce::facet_row(~ partisan, scales = "free_x", space = "free") +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_medsl() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    strip.text.x = element_text(margin = margin(b = "10"))
  ) +
  coord_flip()

ggsave("figs/disc_comparison.jpg", width = 12, height = 8, units = "in")
