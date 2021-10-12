source("load time series data.R")

require(rjags)
require(runjags)

salmon_model_string <- "model{

  # Salmon model

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

  # Model for initial presence and detection

  for(t in 1:Yrs){
      for(j in 1:Nmat){
        detection[t,1,j] ~ dbern(detect_prob[state[t,1,j]])
        state[t,1,j] ~ dcat(init_p[t,1:2]) #draw state from categorical distribution
      }
      init_p[t,1] <- parrive[t,1]/(pleave[t,1]+parrive[t,1]) # assign initial state probabilities as the steady state probabilities from the transition matrix
      init_p[t,2] <- pleave[t,1]/(pleave[t,1]+parrive[t,1])
  }
  
  # Transition model
  
  for(t in 1:Yrs){
    for(j in 1:Nmat){
      for(i in 2:Nday){
        
        detection[t,i,j] ~ dbern(detect_prob[state[t,i,j]]) # detect with given probability
        state[t,i,j] ~ dcat(ptran[t,i,state[t,(i-1),j],1:2]) # draw state from categorical distribution given previous state
        
      }
    }
  }
  
  # Detection probability prior
  
  detect_prob[1] <- pd
  detect_prob[2] <- 0
  pd ~ dbeta(2,2)
  
  # set up year and matriline specific transistion matrices
  
  mu.lam <- mean(lambda)
  
  for(t in 1:Yrs){
    for(i in 1:Nday){
      ptran[t,i,1,1] <- 1 - pleave[t,i] # probability of staying is 1 - the probability of leaving
      ptran[t,i,1,2] <- pleave[t,i] # probability of transitioning from present to absent is just the probability of leaving
      ptran[t,i,2,1] <- parrive[t,i] # probability of arriving
      ptran[t,i,2,2] <- 1 - parrive[t,i] # probability of staying gone is 1 - probability of arriving
  
      logit(pleave[t,i]) <- l0 + l1*lambda[t,i]
      logit(parrive[t,i]) <- a0 + a1*lambda[t,i]
    }
  }
  
  l0 ~ dnorm(0,1)
  l1 ~ dnorm(0,1)
  
  a0 ~ dnorm(0,1)
  a1 ~ dnorm(0,1)

}"

# Set up the data
# Currently subsetted for testing, but will eventually include all the data

jags_data <- list(
  Yrs = length(years),
  Nday = 50,
  Nmat = length(mats),
  detection = mat_sightings[,25:74,],
  catch = salmon_catch[,25:74],
  effort = salmon_effort[,25:74]
)

# Run the model, recording the population-level intercepts for (logit-transformed) leaving and arrival probabilities, and (non-transformed) detection probabilities

salmon_transition_run <- run.jags(salmon_model_string, data = jags_data, monitor = c("a0","a1","l0","l1","detect_prob[1]"), sample = 1000, burnin = 1000)

summary(salmon_transition_run)
plot(salmon_transition_run)
