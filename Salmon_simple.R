rm(list = ls())

require(cmdstanr)
require(matrixStats)
require(tidyverse)
require(posterior)
require(rethinking)
require(cowplot)
source("load time series data.R")

dim(salmon_catch)
dim(salmon_effort)

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
salmon_data <- list(
  D = dim(mat_sightings)[[2]],
  M = dim(mat_sightings)[[3]],
  Y = dim(mat_sightings)[[1]],
  salmon_N = nrow(salmon.df),
  salmon_years = salmon.df$year,
  salmon_days = salmon.df$day,
  salmon_days_std = salmon.df$day/max(salmon.df$day),
  salmon_catch = salmon.df$catch,
  salmon_effort = salmon.df$effort
)

mod = cmdstan_model(
  stan_file = "Salmon_simple.stan"
)

fit = mod$variational(data = salmon_data, output_samples = 250)

salmon_draws <- fit$draws() %>%
  as_draws_df

salmon_matrix <- as.matrix(salmon_draws)

salmon_array <- array(data = salmon_matrix[,substr(colnames(salmon_matrix),1,7) == "log_lam"], dim = c(500, dim(mat_sightings)[[1]],dim(mat_sightings)[[2]]))
