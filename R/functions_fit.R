# R/functions_fit.R

# 1PL Rasch Model with intercept and random effects for nested districts within offices
# also include random effect for individuals

fit_binomial <- function(data, type){
  
  if (type == "rasch"){
    if (n_distinct(data$county_name) > 1){
      form <- bf(choice_rep ~ county_name + (1 | office/district) + (1 | cvr_id))
    } else {
      form <- bf(choice_rep ~ (1 | office/district) + (1 | cvr_id))
    }
  }
  
  if (type == "2pl"){
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
  }
  
  brm(
    formula = form,
    family = bernoulli(),
    data = data,
    chains = 4,
    iter = 2000,
    seed = 02139,
    silent = 0,
    control = list(adapt_delta = 0.95)
  )
  
}

fit_stan <- function(model, stan_data, file_name){
  
  m <- cmdstan_model(str_c("R/", file_name, ".stan"), compile = FALSE)
  m$compile(cpp_options = list(stan_threads = TRUE))
  
  fit <- m$sample(
    data = stan_data,
    chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    seed = 02139,
    parallel_chains = 4,
    threads_per_chain = 16
  )
  
  path <- str_c("fits/", file_name, ".rds")
  
  fit$save_object(path)
  
  path
}