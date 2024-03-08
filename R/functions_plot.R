plot_ber_ideals <- function(ber_1pl, ber_2pl){
  
  p1 <- ber_1pl |>
    spread_draws(r_cvr_id[cvr_id, var]) |>
    mutate(alpha = r_cvr_id/sd(r_cvr_id)) |>
    filter(cvr_id < 20) |> 
    ggplot(aes(x = alpha, y = as.character(cvr_id))) +
    stat_halfeye() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
    labs(x = expression(alpha), y = "Voter", title = "1PL Model")
  
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
  
  return(ber_params)
  
}

plot_traces <- function(ber_1pl, ber_2pl, cat_2pl_path){
  
  cat_2pl <- readRDS(cat_2pl_path)
  
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
  
  return(list(trace_1, trace_2, trace_3))
  
}

plot_rhats <- function(ber_1pl, ber_2pl, cat_2pl_path){
  
  cat_2pl <- readRDS(cat_2pl_path)
  
  p_r1 <- tibble(r = rhat(ber_1pl)) |> 
    ggplot(aes(x = r)) +
    geom_dots() +
    scale_y_continuous(labels = NULL) +
    labs(title = "Bernoulli 1PL", x = expression(hat(R)), y = "")
  
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
  
  return(plot)
}