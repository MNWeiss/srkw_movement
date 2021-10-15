rm(list = ls())

##https://mc-stan.org/users/documentation/case-studies/hmm-example.html
# https://github.com/stan-dev/example-models/blob/master/knitr/hmm-example/hmm-example.Rmd

require(cmdstanr)
require(tidyverse)
require(posterior)
source("load time series data.R")

jags_data_simple <- list(
  N = 153,
  y = mat_sightings[1,1:153,1]
)
jags_data_simple

mod = cmdstan_model(
  stan_file = "HiddenMarkovTest.stan"
)
fit = mod$sample(data = jags_data_simple, parallel_chains = 4)

fit$summary("mu")
fit$summary("t1")
fit$summary("t2")
fit$summary("t3")
fit$summary()

hidden_probs_df = fit$draws() %>%
  as_draws_df %>%
  select(starts_with("hidden_probs")) %>%
  pivot_longer(everything(),
               names_to = c("state", "n"),
               names_transform = list(k = as.integer, n = as.integer),
               names_pattern = "hidden_probs\\[([0-9]*),([0-9]*)\\]",
               values_to = "hidden_probs")

hidden_probs_df %>%
  group_by(state, n) %>%
  summarize(qh = quantile(hidden_probs, 0.8),
            m = median(hidden_probs),
            ql = quantile(hidden_probs, 0.2)) %>%
  ungroup() %>%
  ggplot() +
  geom_errorbar(aes(n, ymin = ql, ymax = qh, width = 0.0, colour = state), alpha = 0.5) +
  geom_point(aes(n, m, colour = state)) +
  facet_grid(state ~ ., labeller = "label_both") +
  ggtitle("Ribbon is 60% posterior interval, point is median") +
  ylab("Probability of being in state") +
  xlab("Time (n)")



require(rethinking)
stanfit <- rstan::read_stan_csv(fit$output_files())
traceplot(stanfit)




