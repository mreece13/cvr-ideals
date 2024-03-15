plot_ber_ideals <- function(ber_1pl, ber_2pl){
  
  rand_1pl <- str_c("r_cvr_id[", 1:20, ",Intercept]")
  rand_2pl <- str_c("r_cvr_id__alpha[", 1:20, ",Intercept]")
  
  p1 <- ber_1pl |>
    as_draws_df() |> 
    select(any_of(rand_1pl), starts_with(".")) |> 
    spread_draws(r_cvr_id[cvr_id,]) |>
    ggplot(aes(x = r_cvr_id, y = as.factor(cvr_id))) +
    stat_halfeye(normalize = "xy") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
    labs(x = expression(alpha), y = "Voter", title = "1-Parameter")
  
  p2 <- ber_2pl |> 
    as_draws_df() |> 
    select(any_of(rand_2pl), starts_with(".")) |> 
    spread_draws(r_cvr_id__alpha[cvr_id,]) |>
    ggplot(aes(x = r_cvr_id__alpha, y = as.factor(cvr_id))) +
    stat_halfeye(normalize = "xy") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
    labs(x = expression(alpha), y = "", title = "2-Parameter")
  
  ber_ideal <- p1 + p2 & 
    theme_medsl() & 
    theme(
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(fill = NA, color = "black")
    )
  
  ggsave("figs/ber_ideals.jpg", plot = ber_ideal, width = 10, height = 6, units = "in")
  
  return(ber_ideal)
  
}

plot_ber_params <- function(ber_2pl){
  
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
    slice_sample(n=25) |> 
    bind_rows(tibble(race = c("US PRESIDENT - FEDERAL", "US SENATE - COLORADO"))) |> 
    distinct(race)
  
  # slopes
  gamma <- item_pars_2pl[, , "loggamma_Intercept"] |>
    exp() |> 
    sweep(1, person_sds_2pl, "/") |>
    as_tibble() |> 
    pivot_longer(cols = everything(), names_to = "race") |> 
    mutate(nlpar = "gamma") |> 
    mutate(race = str_remove(race, fixed(", ")) |> str_squish()) 
  
  ber_params <- bind_rows(beta, gamma) |>
    inner_join(random_races) |> 
    mutate(nlpar = factor(nlpar, labels = c("Difficulty", "Discrimination"))) |>
    ggplot(aes(x = value, y = race)) +
    stat_halfeye(geom = "pointinterval") +
    geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
    facet_wrap(~ nlpar, scales = "free_x") +
    labs(y = "", x = "") +
    theme_medsl() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(fill = NA, color = "black"),
      panel.spacing.x = unit(15, units = "points"),
      strip.text.x = element_text(margin = margin(b = "10")),
      strip.clip = "off"
    )
  
  ggsave("figs/ber_params.jpg", plot = ber_params, width = 10, height = 12, units = "in")
  
  return(ber_params)
  
}

plot_traces <- function(ber_1pl, ber_2pl, cat_2pl_path){
  
  cat_2pl <- readRDS(cat_2pl_path) |> as_draws_df()
  
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
  
  return("COMPLETED")
  
}

plot_rhats <- function(ber_1pl, ber_2pl, cat_2pl_path){
  
  cat_2pl <- readRDS(cat_2pl_path)
  
  d <- tibble(r = brms::rhat(ber_1pl), type = "Bernoulli 1-Parameter") |> 
    bind_rows(
      tibble(r = brms::rhat(ber_2pl), type = "Bernoulli 2-Parameter")
    ) |> 
    bind_rows(
      tibble(r = summarise_draws(cat_2pl)$rhat, type = "Categorical 2-Parameter")
    )
  
  datasummary(r * type ~ Mean + Median + Max + SD, data = d)
  
  d |> 
    ggplot(aes(x = r, y = type)) +
    geom_boxplot() +
    theme_bw()
  
  p_r1 <- tibble(r = brms::rhat(ber_1pl)) |> 
    ggplot(aes(x = r)) +
    geom_dots() +
    scale_y_continuous(labels = NULL) +
    labs(title = "Bernoulli 1PL", x = expression(hat(R)), y = "")
  
  p_r2 <- tibble(r = brms::rhat(ber_2pl)) |> 
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
  
  return(plot)
}