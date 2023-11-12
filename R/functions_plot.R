#R/functions_plot.R

plot_trace <- function(fit){
  mcmc_trace(fit, regex_pars = "^b|^sd_")
}

plot_rhats <- function(fits){
  
  tibble(fit = fits, fit_name = names(fits)) |> 
    mutate(rhat = map(fit, ~ select(summarise_draws(.x), rhat))) |> 
    select(-fit) |> 
    unnest(cols = rhat) |> 
    ggplot(aes(x = rhat)) +
    geom_dots() + 
    geom_vline(xintercept = 1, color = "blue", linetype = "dashed") +
    scale_y_continuous(labels = NULL) +
    facet_wrap(~ fit_name, scales = "free_x", ncol=2) +
    labs(x = expression(hat(R)), y = "") +
    theme_bw()
  
}

plot_voters <- function(fit, randos, twopl){
  
  if (twopl){
    plot <- fit |> 
      spread_draws(r_cvr_id__eta[cvr_id, dimension]) |> 
      mutate(cvr_id = as.character(cvr_id)) |> 
      inner_join(randos) |>
      mutate(r_cvr_id__eta = if_else(r_cvr_id__eta > quantile(r_cvr_id__eta, 0.99, na.rm = TRUE), NA, r_cvr_id__eta)) |> 
      mutate(r_cvr_id__eta = if_else(r_cvr_id__eta < quantile(r_cvr_id__eta, 0.01, na.rm = TRUE), NA, r_cvr_id__eta)) |> 
      ggplot(aes(y = cvr_id, x = r_cvr_id__eta)) +
      geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
      stat_halfeye() +
      labs(x = "Ideal Point Estimate", y = "Voter ID") +
      theme_bw()
    
  } else {
    plot <- fit |> 
      spread_draws(r_cvr_id[cvr_id, dimension]) |> 
      mutate(cvr_id = as.character(cvr_id)) |> 
      inner_join(randos) |>
      mutate(r_cvr_id = if_else(r_cvr_id > quantile(r_cvr_id, 0.99, na.rm = TRUE), NA, r_cvr_id)) |> 
      mutate(r_cvr_id = if_else(r_cvr_id < quantile(r_cvr_id, 0.01, na.rm = TRUE), NA, r_cvr_id)) |> 
      ggplot(aes(y = cvr_id, x = r_cvr_id)) +
      geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
      stat_halfeye() +
      labs(x = "Ideal Point Estimate", y = "Voter ID") +
      theme_bw()
  }
  
  return(plot)
  
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
