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

draws_signed <- cat_2pl |> 
  spread_draws(gamma[id]) |> 
  ungroup() |> 
  left_join(mutate(ids, id = 1:n()), join_by(id)) |> 
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
  mutate(race_type = str_remove_all(race, " -.*?$|"),
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
           "DARIO HUNTER DAWN NEPTUNE ADAMS" ~ "DARIO HUNTER",
           .default = candidate
         )
         )

base |> 
  filter(!(mean(gamma) == 0), .by = c(race, candidate)) |>
  filter(party_detailed == "NONPARTISAN") |> 
  ggplot(aes(x = gamma, y = reorder(interaction(race, candidate, sep = " - "), as.numeric(as.factor(race_type))))) +
  stat_halfeye(geom = "pointinterval") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  ggforce::facet_col(~ race_type, scales = "free_y", space = "free") +
  labs(x = NULL, y = NULL, title = NULL, color = "Contest") +
  theme_medsl() +
  # scale_color_medsl() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(fill = NA, color = "black"),
    # axis.text.x = element_text(angle = 90, hjust = 1),
    panel.spacing.x = unit(15, units = "points"),
    strip.text.x = element_text(margin = margin(b = "10")),
    strip.clip = "off",
    legend.position = "right",
    legend.direction = "vertical",
    legend.box.background = element_rect(color = "black", linewidth = 0.2)
  )

ggsave("figs/nonpartisan_disc.jpg", width = 8, height = 12, units = "in")

base |> 
  # filter(!(mean(gamma) == 0), .by = c(race, candidate)) |>
  filter(party_detailed != "NONPARTISAN") |> 
  ggplot(aes(x = gamma, y = reorder(interaction(race, candidate, sep = " - "), as.numeric(as.factor(race))))) +
  stat_halfeye(geom = "pointinterval") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  ggforce::facet_col(~ race_type, scales = "free_y", space = "free") +
  labs(x = NULL, y = NULL, title = NULL, color = "Contest") +
  theme_medsl() +
  # scale_color_medsl() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(fill = NA, color = "black"),
    # axis.text.x = element_text(angle = 90, hjust = 1),
    panel.spacing.x = unit(15, units = "points"),
    strip.text.x = element_text(margin = margin(b = "10")),
    strip.clip = "off",
    legend.position = "right",
    legend.direction = "vertical",
    legend.box.background = element_rect(color = "black", linewidth = 0.2)
  )

ggsave("figs/partisan_disc2.jpg", width = 8, height = 14, units = "in")

base |> 
  filter(party_detailed != "NONPARTISAN", race_type == "US PRESIDENT") |> 
  bind_rows(
    tibble(candidate = "JOSEPH R BIDEN", gamma = 0)
  ) |> 
  mutate(candidate = as.factor(candidate) |> fct_relevel("JOSEPH R BIDEN")) |> 
  ggplot(aes(x = gamma, y = candidate)) +
  stat_halfeye(geom = "pointinterval") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  # ggforce::facet_col(facets = vars(race_type), scales = "free", space = "free") +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_medsl() +
  theme(
    panel.grid.major.x = element_blank()
  )

ggsave("figs/partisan_disc_pres.jpg", width = 10, height = 8, units = "in")

## 

base |> 
  mutate(partisan = ifelse(party_detailed == "NONPARTISAN", "NONPARTISAN", "PARTISAN")) |> 
  ggplot(aes(x = abs(gamma), y = reorder(race_type, partisan == "NONPARTISAN"), color = partisan)) +
  stat_halfeye(geom = "pointinterval") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  ggforce::facet_col(~ partisan, scales = "free_y", space = "free") +
  labs(x = NULL, y = NULL, title = NULL, color = "Election Type") +
  theme_medsl() +
  scale_color_manual(values = c("#C0BA79", "#3791FF")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(fill = NA, color = "black"),
    # axis.text.x = element_text(angle = 90, hjust = 1),
    strip.text.x = element_text(margin = margin(b = "10")),
    legend.position = "right",
    legend.direction = "vertical",
    legend.box.background = element_rect(color = "black", linewidth = 0.2)
  )

ggsave("figs/disc_slides.jpg", width = 12, height = 8, units = "in")

####

ideals <- readRDS("fits/cat_2pl_streamlinednumV7535_full.rds") |> 
  as_draws_matrix() |> 
  subset_draws("alpha")

ideals <- ideals * out$sign_vectors[,1]

choices <- data |> 
  filter(candidate != "undervote") |> 
  left_join(ids, join_by(race, candidate)) |> 
  arrange(race_id, candidate_id) |> 
  mutate(cvr_id = 1:n(), .by = cvr_id) |> 
  distinct(cvr_id, office, district, candidate, party_detailed.x)

results <- ideals |> 
  spread_draws(alpha[cvr_id]) |> 
  ungroup() |> 
  left_join(choices, join_by(cvr_id)) |> 
  drop_na(district) |> 
  mutate(district = as.numeric(district))

house_options <- house_results |> distinct(district) |> pull()

house_summaries <- house_results |> 
  summarise(ideal_mean = mean(alpha), .by = district)

boundaries <- read_sf("data/co_sldl_2011_to_2021/co_sldl_2011_to_2021.shp") |> 
  select(District_1) |> 
  filter(District_1 %in% house_options) |> 
  left_join(house_summaries, join_by("District_1" == "district")) 

ggplot(boundaries, aes(fill = ideal_mean)) +
  geom_sf() +
  ggrepel::geom_label_repel(
    data = head(boundaries),
    aes(label = round(ideal_mean, 2), geometry = geometry),
    stat = "sf_coordinates"
  ) +
  theme_medsl_map() +
  scale_fill_gradient2(low = "#3791FF", mid = "white", high = "#F6573E", midpoint = 0, limits = c(-1, 1)) +
  labs(fill = "Average Ideal Point", title = "Colorado State House Districts")

ggsave("figs/map_state_house.jpg", width = 12, height = 8, units = "in")

house_results |> 
  ggplot(aes(x = alpha, y = district)) +
  stat_slab() +
  theme_bw()
  scale_fill_gradient2(low = "#3791FF", mid = "white", high = "#F6573E", midpoint = 0, limits = c(-1, 1))

###

cat_2pl_full <- cat_2pl |> 
  colMeans() |> 
  as_tibble(rownames = "parameter") |> 
  filter(!str_detect(parameter, "^lp|raw|\\."))

cat_2pl_var <- readRDS("fits/cat_2pl_streamlined_numV7535_var.rds") |> 
  as_draws_df() |> 
  slice_head(n=1) |> 
  select(-starts_with("lp_"), -contains("raw"), -contains(".")) |> 
  pivot_longer(cols = everything(), names_to = "parameter")

comparison <- left_join(cat_2pl_full, cat_2pl_var, join_by(parameter))

cor(comparison$value.x, comparison$value.y)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

comparison |> 
  filter(parameter != "mu_beta") |> 
  mutate(
    type = case_when(
      str_detect(parameter, "alpha") ~ "alpha",
      str_detect(parameter, "beta") ~ "beta",
      str_detect(parameter, "gamma") ~ "gamma"
      ),
    type = factor(type, labels = c("alpha", "beta", "gamma"))
  ) |> 
  mutate(
    value.x = range01(value.x),
    value.y = range01(value.y),
    .by = type
  ) |> 
  ggplot(aes(x = value.x, y = value.y)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "#F6573E", linetype = "dashed") +
  geom_smooth(color = "#3791FF") +
  facet_wrap(~ type, scales = "fixed", labeller = label_parsed) +
  labs(x = "Full Parameters", y = "Pathfinder Parameters") +
  theme_medsl() +
  theme(
    strip.text = element_text(family = "sans"),
    panel.border = element_rect(fill = NA, color = "black"),
    panel.grid.major = element_blank()
  ) +
  ylim(c(0, 1)) +
  xlim(c(0, 1)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1), breaks = c(0, 0.5, 1)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1), breaks = c(0, 0.5, 1))

ggsave("figs/var_compare.jpg", width = 10, height = 4, units = "in")


################################################
# Creating Stan Data for Grouping
################################################

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

path <- str_c("fits/", file_name, "_numV", as.character(stan_data$N_groups), "_grouped.rds")

fit$save_object(path)