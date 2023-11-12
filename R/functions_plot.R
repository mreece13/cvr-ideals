#R/functions_plot.R

plot_trace <- function(fit){
  mcmc_trace(fit, regex_pars = "^b|^sd_")
}

plot_rhats <- function(fits){
  
  rhats <- tibble(fit = fits) |> 
    mutate(rhat = map(fit, ~ select(summarise_draws(.x), rhat))) |> 
    unnest(cols = rhat)
  
  rhats |> 
    ggplot(aes(x = rhat, y = fit)) +
    geom_dots() + 
    geom_vline(xintercept = 1, color = "blue", linetype = "dashed") +
    scale_x_continuous(labels = scales::label_number(accuracy = 0.001)) +
    labs(x = expression(hat(R)), y = "") +
    theme_bw()
  
}

plot_voters <- function(fit, randos){
  
  fit |> 
    spread_draws(r_cvr_id[cvr_id, dimension]) |> 
    mutate(cvr_id = as.character(cvr_id)) |> 
    inner_join(randos) |>
    mutate(r_cvr_id = if_else(r_cvr_id > quantile(r_cvr_id, 0.99, na.rm = TRUE), NA, r_cvr_id)) |> 
    mutate(r_cvr_id = if_else(r_cvr_id < quantile(r_cvr_id, 0.01, na.rm = TRUE), NA, r_cvr_id)) |> 
    ggplot(aes(y = cvr_id, x = r_cvr_id)) +
    geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
    stat_halfeye() +
    theme_bw()
  
}

plot_offices <- function(fit){
  
  fit |> 
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
    theme_bw()
  
}
