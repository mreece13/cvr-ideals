# R/functions_fit.R

# 1PL Rasch Model with intercept and random effects for nested districts within offices
# also include random effect for individuals
fit_rasch <- function(data, binomial = TRUE){
  
  if (n_distinct(data$county_name) > 1){
    form <- bf(choice_rep ~ county_name + (1 | office/district) + (1 | cvr_id))
  } else {
    form <- bf(choice_rep ~ (1 | office/district) + (1 | cvr_id))
  }
  
  if (binomial){
    fit <- brm(
      formula = form,
      family = bernoulli(),
      data = data,
      chains = 4,
      iter = 2000,
      seed = 02139,
      silent = 0,
      control = list(adapt_delta = 0.95)
    )
  } else {
    stop("MULTINOMIAL NOT YET IMPLEMENTED")
  }
  
  return(fit)
  
}

# Here, 2PL Model with intercept and random effects for nested districts within offices
# also include random effect for individuals.
# most important formulation is the addition of a discrimination parameter
fit_2pl <- function(data, binomial = TRUE){
  
  if (n_distinct(data$county_name) > 1){
    form <- bf(choice_rep ~ exp(logalpha) * eta,
               eta ~ 1 + county_name + (1 | office/district) + (1 | cvr_id),
               logalpha ~ 1 + county_name + (1 | office/district),
               nl = TRUE)
  } else {
    form <- bf(choice_rep ~ exp(logalpha) * eta,
               eta ~ 1 + (1 | office/district) + (1 | cvr_id),
               logalpha ~ 1 + (1 | office/district),
               nl = TRUE)
  }
  
  if (binomial){
    fit <- brm(
      formula = form,
      family = bernoulli(),
      data = data,
      seed = 02139,
      silent = 0,
      control = list(adapt_delta = 0.95, max_treedepth = 15)
    )
  } else {
    stop("MULTINOMIAL NOT YET IMPLEMENTED")
  }
  
  return(fit)
}