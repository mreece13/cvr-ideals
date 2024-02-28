rm(list=ls())
gc()

library(tidyverse)
library(brms)
library(tidybayes)
library(posterior)
library(factor.switching)

cat_2pl <- readRDS("fits/cat_2pl.rds") |> 
  as_draws_matrix() |> 
  subset_draws(variable = "gamma")

var <- str_replace(colnames(cat_2pl), ".+\\[([0-9]+),([0-9]+)\\]$", "\\1")
var <- as.integer(var)
dim <- rep(1, length(var))
dim <- str_replace(colnames(cat_2pl), ".+\\[([0-9]+),([0-9]+)\\]$", "\\2")
dim <- as.integer(dim)

colord <- order(var, dim)
colnames(cat_2pl) <- str_replace(
  colnames(cat_2pl),
  "^gamma\\[([0-9]+),([0-9]+)\\]$",
  "LambdaV\\1_\\2"
)

out <- rsp_partial_sa(
  lambda_mcmc = cat_2pl[, colord],
  rotate = TRUE,
  maxIter = 100,
  threshold = 1e-6,
  sa_loops = 5,
  verbose = TRUE
)

write_rds(out, "fits/cat_2pl_rsp.rds")