source("load time series data.R")

require(rjags)
require(runjags)

salmon_model <- "model{

  for(t in 1:Yrs){
    for(i in 1:Nday){
      catch[t,i] ~ dpois(lambda[t,i]*effort[t,i]) # catch is a Poisson process
      log(lambda[t,i]) <- b0[t] + b1[t]*i + b2[t]*(i^2) + b3[t]*(i^3) + b4[t]*(i^4) + err[t,i] # predicted salmon on log scale
      err[t,i] ~ dnorm(0,tau.obs) # observation-level random effect for overdispersion
    }
  }
  
  tau.obs ~ dgamma(1,0.01) # observation-level variance
  
  # random coefficients for each year
  for(t in 1:Yrs){
    b0[t] ~ dnorm(mu.b0,tau.b0)
    b1[t] ~ dnorm(mu.b1,tau.b1)
    b2[t] ~ dnorm(mu.b2,tau.b2)
    b3[t] ~ dnorm(mu.b3,tau.b3)
    b4[t] ~ dnorm(mu.b4,tau.b4)
  }
  
  # priors for coefficient means
  mu.b0 ~ dnorm(0,1)
  mu.b1 ~ dnorm(0,1)
  mu.b2 ~ dnorm(0,1)
  mu.b3 ~ dnorm(0,1)
  mu.b4 ~ dnorm(0,1)
  
  # priors for coefficient variances
  tau.b0 ~ dgamma(1,0.01)
  tau.b1 ~ dgamma(1,0.01)
  tau.b2 ~ dgamma(1,0.01)
  tau.b3 ~ dgamma(1,0.01)
  tau.b4 ~ dgamma(1,0.01)

}"

# get the data for the model
model_data <- list(
  Yrs = length(years),
  Nday = length(unique_days),
  catch = salmon_catch,
  effort = salmon_effort
)

# run the model
salmon_sample <- run.jags(salmon_model, data = model_data, monitor = "lambda")

# get the MCMC iterations
salmon_mcmc <- do.call(rbind,salmon_sample$mcmc)

# get it as an array
salmon_lambda <- array(dim = c(nrow(salmon_mcmc),nrow(salmon_catch),ncol(salmon_catch)))
for(i in 1:nrow(salmon_mcmc)){
  salmon_lambda[i,,] <- matrix(salmon_mcmc[i,],nrow=nrow(salmon_catch),ncol=ncol(salmon_catch))
}
