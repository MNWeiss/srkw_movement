source("load time series data.R")

require(rjags)
require(runjags)

simple_model_string <- "model{

  # Model for initial presence and detection

  for(t in 1:Yrs){
      for(j in 1:Nmat){
        detection[t,1,j] ~ dbern(detect_prob[state[t,1,j]])
        state[t,1,j] ~ dcat(init_p[1:2]) #draw state from categorical distribution
      }
  }
  
  init_p[1] <- parrive/(pleave+parrive) # assign initial state probabilities as the steady state probabilities from the transition matrix
  init_p[2] <- pleave/(pleave+parrive)
  
  # Transition model
  
  for(t in 1:Yrs){
    for(j in 1:Nmat){
      for(i in 2:Nday){
        
        detection[t,i,j] ~ dbern(detect_prob[state[t,i,j]]) # detect with given probability
        state[t,i,j] ~ dcat(ptran[state[t,(i-1),j],1:2]) # draw state from categorical distribution given previous state
        
      }
    }
  }
  
  # Detection probability prior
  
  detect_prob[1] <- pd
  detect_prob[2] <- 0
  pd ~ dunif(0,1)
  
  # set up year and matriline specific transistion matrices
  
  ptran[1,1] <- 1 - pleave # probability of staying is 1 - the probability of leaving
  ptran[1,2] <- pleave # probability of transitioning from present to absent is just the probability of leaving
  ptran[2,1] <- parrive # probability of arriving
  ptran[2,2] <- 1 - parrive # probability of staying gone is 1 - probability of arriving
  
  pleave ~ dunif(0,1)
  parrive ~ dunif(0,1)

}"

# Set up the data
# Currently subsetted for testing, but will eventually include all the data

jags_data <- list(
  Yrs = length(years),
  Nday = 50,
  Nmat = length(mats),
  detection = mat_sightings[,1:50,]
)

# Run the model, recording the population-level intercepts for (logit-transformed) leaving and arrival probabilities, and (non-transformed) detection probabilities

simple_run <- run.jags(simple_model_string, data = jags_data, monitor = c("pleave","parrive","detect_prob[1]"), sample = 1000, burnin = 1000)

summary(simple_run)
plot(simple_run$mcmc)
