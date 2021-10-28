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

###for testing
salmon.df = filter(salmon.df, year <3)


# get the data for the model
salmon_data <- list(
  Y = 2,
  D = 153,
  D_seq = (1:153 /153),
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

fit = mod$sample(data = salmon_data, parallel_chains = 4)


stanfit <- rstan::read_stan_csv(fit$output_files())
traceplot(stanfit)
precis(stanfit, depth = 3)

samples = extract(stanfit)

plot.df = list()
for(i in 1:salmon_data$Y){
  post.samples  = exp(samples$log_lam[,i,])
  plot.df[[i]] = 
    data.frame(
      year = i, 
      d = salmon_data$D_seq*153, 
      post.mean = colMeans(post.samples),
      lCI = colQuantiles(post.samples, probs = 0.05 ),
      uCI = colQuantiles(post.samples, probs = 0.95 )
      
      )
}
plot.df = bind_rows(plot.df)


plot.list = list()
for(i in 1:salmon_data$Y){
  plot.list[[i]] = 
    ggplot()+
    # geom_line(filter(plot.df, year == i), mapping = aes(d, post.mean))+
    geom_ribbon(filter(plot.df, year == i), mapping = aes(x = d, y = post.mean,ymin = lCI, ymax = uCI), alpha = 0.5)+
    geom_point(filter(salmon.df, year == i), mapping = aes(x = day, y = catch/effort))
}

plot_grid(plotlist = plot.list)


