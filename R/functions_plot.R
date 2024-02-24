#R/functions_plot.R

plot_trace <- function(fit){
  mcmc_trace(fit, regex_pars = "^b_")
}

plot_rhats <- function(){
  
  ber_1pl <- readRDS("fits/bernoulli_rasch.rds")
  ber_2pl <- readRDS("fits/bernoulli_2pl.rds")
  cat_2pl <- readRDS("fits/cat_2pl_unrestricted.rds")
  
  p_r1 <- tibble(r = rhat(ber_1pl)) |> 
    ggplot(aes(x = r)) +
    geom_dots() +
    scale_y_continuous(labels = NULL) +
    labs(title = "Binary 1PL", x = expression(hat(R)), y = "")
  
  p_r2 <- tibble(r = rhat(ber_2pl)) |> 
    ggplot(aes(x = r)) +
    geom_dots() +
    scale_y_continuous(labels = NULL) +
    labs(title = "Binary 2PL", x = expression(hat(R)), y = "")
  
  p_r3 <- tibble(r = summarise_draws(cat_2pl)$rhat) |> 
    ggplot(aes(x = r)) +
    geom_dots() +
    scale_y_continuous(labels = NULL) +
    labs(title = "Categorical 2PL", x = expression(hat(R)), y = "")
  
  plot <- p_r1 + p_r2 + p_r3 & theme_medsl()
  
  ggsave("figs/rhat_comparison.jpg", plot = plot, width = 10, height = 6, units = "in")
  
  return("RHAT PLOT")
  
}

plot_bin <- function(){
  
  ber_1pl <- readRDS("fits/bernoulli_rasch.rds")
  ber_2pl <- readRDS("fits/bernoulli_2pl.rds")
  
  p1 <- ber_1pl |>
    spread_draws(r_cvr_id[cvr_id, var]) |>
    mutate(alpha = r_cvr_id/sd(r_cvr_id)) |>
    filter(cvr_id < 50) |> 
    ggplot(aes(x = alpha, y = as.character(cvr_id))) +
    stat_halfeye() +
    theme_bw() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
    labs(x = expression(alpha), y = "Voter", title = "Bernoulli 1PL Model -- Latent Trait")
  
  p2 <- ber_2pl |> 
    spread_draws(r_cvr_id__alpha[cvr_id,]) |> 
    mutate(alpha = r_cvr_id__alpha/sd(r_cvr_id__alpha)) |> 
    filter(cvr_id < 50) |> 
    ggplot(aes(x = alpha, y = as.character(cvr_id))) +
    stat_halfeye() +
    theme_bw() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
    labs(x = expression(alpha), y = "", title = "Bernoulli 2PL Model -- Latent Trait")
  
  plot <- p1 + p2 & scale_x_continuous(limits = c(-6, 6))
  
  ggsave("figs/rhat_comparison.jpg", plot = plot, width = 6, height = 6, units = "in")
  
  return("Bernoulli PLOT - ALPHA")
  
  
}
