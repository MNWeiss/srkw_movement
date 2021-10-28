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

###for testing
salmon.df = filter(salmon.df, year <=2, day <=153)

# get the data for the model
mod_data <- list(
  D = 153,
  M = 5,
  Y = 2,
  S = mat_sightings[1:2,1:153,1:5],

  salmon_N = nrow(salmon.df),
  salmon_years = salmon.df$year,
  salmon_days = salmon.df$day,
  salmon_catch = salmon.df$catch,
  salmon_effort = salmon.df$effort
)


mod = cmdstan_model(
  stan_file = "StatesandSalmon.stan"
)

fit = mod$sample(data = mod_data, parallel_chains = 4)

saveRDS(fit, "statesalmonmod.RDS")
fit = readRDS("statesalmonmod.RDS")

stanfit <- rstan::read_stan_csv(fit$output_files())
activepars = c("pd", "parrive", "pleave", "mu[3]")
traceplot(stanfit, pars = activepars)
precis(stanfit, depth = 3)

hidden_probs_df = fit$draws() %>%
  as_draws_df %>%
  select(starts_with("hidden_probs")) %>%
  pivot_longer(everything(),
               names_to = c("year", "mat","state", "d"),
               names_transform = list(year = as.integer, mat = as.integer, state = as.factor, d = as.integer),
               names_pattern = "hidden_probs\\[([0-9]*),([0-9]*),([0-9]*),([0-9]*)\\]",
               values_to = "hidden_probs")


hidden_probs_df %>%
  group_by(year, mat, state, d) %>%
  summarize(qh = quantile(hidden_probs, 0.8),
            m = median(hidden_probs),
            ql = quantile(hidden_probs, 0.2)) %>%
  group_by(d) %>%
  # filter(m == max(m)) %>%
  arrange(d) %>%
  ggplot(aes(x = d, y = m, fill = state)) +
  geom_bar(stat = "identity")+
  theme_bw() +
  facet_wrap(year~mat)