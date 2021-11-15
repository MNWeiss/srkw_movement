source("load time series data.R")

require(rjags)
require(runjags)

simple_model_string <- "model{

  # Model for initial presence and detection

  for(t in 1:Yrs){
      for(j in 1:Nmat){
        detection[t,1,j] ~ dbern(detect_prob[state[t,1,j]])
        state[t,1,j] ~ dcat(init_p[t,j,1:2]) #draw state from categorical distribution
        init_p[t,j,1] <- parrive[t,j]/(pleave[t,j]+parrive[t,j]) # assign initial state probabilities as the steady state probabilities from the transition matrix
        init_p[t,j,2] <- pleave[t,j]/(pleave[t,j]+parrive[t,j])
      }
  }
  
  # Transition model
  
  for(t in 1:Yrs){
    for(j in 1:Nmat){
      for(i in 2:Nday){
        
        detection[t,i,j] ~ dbern(detect_prob[state[t,i,j]]) # detect with given probability
        state[t,i,j] ~ dcat(ptran[t,j,state[t,(i-1),j],1:2]) # draw state from categorical distribution given previous state
        
      }
    }
  }
  
  # Detection probability prior
  
  detect_prob[1] <- pd
  detect_prob[2] <- 0
  pd ~ dunif(0,1)
  
  # set up year and matriline specific transistion matrices
  
  for(t in 1:Yrs){
    for(j in 1:Nmat){
      ptran[t,j,1,1] <- 1 - pleave[t,j] # probability of staying is 1 - the probability of leaving
      ptran[t,j,1,2] <- pleave[t,j] # probability of transitioning from present to absent is just the probability of leaving
      ptran[t,j,2,1] <- parrive[t,j] # probability of arriving
      ptran[t,j,2,2] <- 1 - parrive[t,j] # probability of staying gone is 1 - probability of arriving
    }
  }
  
  # get the probabilities of arriving and leaving
  
  for(t in 1:Yrs){
    for(j in 1:Nmat){
      logit(pleave[t,j]) <- l0 + ly[t] + lm[j]
      logit(parrive[t,j]) <- a0 + ay[t] + am[j]
    }
  }
  
  # Priors for population-level intercepts
  
  l0 ~ dnorm(0,1)
  a0 ~ dnorm(0,1)
  
  # Year level random effects
  
  for(t in 1:Yrs){
    ly[t] ~ dnorm(0, tau.ly)
    ay[t] ~ dnorm(0, tau.ay)
  }
  
  # variance priors
  tau.ly ~ dgamma(1,0.01)
  tau.ay ~ dgamma(1,0.01)
  
  # matriline level random effects
  
  for(j in 1:Nmat){
    lm[j] ~ dnorm(0, tau.lm)
    am[j] ~ dnorm(0, tau.am)
  }
  
  #variance priors
  tau.lm ~ dgamma(1,0.01)
  tau.am ~ dgamma(1,0.01)

}"

# Set up the data
# Currently subsetted for testing, but will eventually include all the data

jags_data <- list(
  Yrs = 5,
  Nday = 50,
  Nmat = 10,
  detection = mat_sightings[1:5,1:50,1:10]
)

# Run the model, recording the population-level intercepts for (logit-transformed) leaving and arrival probabilities, and (non-transformed) detection probabilities

RE_run <- run.jags(simple_model_string, data = jags_data, monitor = c("a0","l0","detect_prob[1]"), sample = 10000, burnin = 1000)
summary(RE_run)
plot(RE_run)