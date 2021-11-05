rm(list = ls())

require(cmdstanr)
require(matrixStats)
require(tidyverse)
require(posterior)
require(rethinking)
require(cowplot)
source("load time series data.R")


##Prepare the salmon data for the model
salmon.df = list()
for(i in 1:dim(salmon_catch)[1]){
  df= data.frame(
    year = i,
    day = seq(1, dim(salmon_catch)[2], 1),
    catch = salmon_catch[i,],
    effort = salmon_effort[i,]
  )
  salmon.df[[i]] = filter(df, !is.na(catch))
}
salmon.df = bind_rows(salmon.df)
salmon.df

# get the data for the model
mod_data <- list(
  D = dim(mat_sightings)[[2]],
  M = dim(mat_sightings)[[3]],
  Y = dim(mat_sightings)[[1]],
  S = mat_sightings,
  S_extant = mat_extant,
  salmon_N = nrow(salmon.df),
  salmon_years = salmon.df$year,
  salmon_days = salmon.df$day,
  salmon_catch = salmon.df$catch,
  salmon_effort = salmon.df$effort
)

# set_cmdstan_path("C:/Users/mw607/cmdstan-2.28.1")
set_cmdstan_path("D:/cmdstan/cmdstan-2.28.1")

mod <- cmdstan_model(
  stan_file = "StatesandSalmon SIMPLE.stan"
)

# fit <- mod$sample(data = mod_data, iter_warmup = 250, iter_sampling = 250, parallel_chains = 4)

## THIS WORKS. 500 model outputs log_omega and y_sim
fit <- mod$variational(data = mod_data, output_samples = 500)

A <- fit$draws() %>%
  as_draws_df


# stanfit <- rstan::read_stan_csv(fit$output_files())

