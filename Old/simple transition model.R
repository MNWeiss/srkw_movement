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
  
  init_p[1:2] ~ ddirch(a[1:2])
  a[1] <- pleave/(parrive+pleave)
  a[2] <- parrive/(parrive+pleave)
  
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
  
  detect_prob[1] <- 0
  detect_prob[2] <- pd

  pd ~ dunif(0,1)
  
  # set up year and matriline specific transistion matrices
  
  ptran[1,1] <- 1-parrive
  ptran[1,2] <- parrive
  ptran[2,1] <- pleave
  ptran[2,2] <- 1-pleave

  pleave ~ dunif(0,0.5)
  parrive ~ dunif(0,0.5)

}"


# Set up the data
# Currently subsetted for testing, but will eventually include all the data

jags_data <- list(
  Yrs = length(years),
  Nday = length(unique_days),
  Nmat = length(mats),
  detection = mat_sightings
)

# Run the model, recording the population-level intercepts for leaving and arrival probabilities, and detection probabilities

start <- Sys.time()
simple_run <- run.jags(simple_model_string, data = jags_data, monitor = c("pleave","parrive","detect_prob","state"), sample = 500000, burnin = 1000, n.chains = 4)
end <- Sys.time()

end - start

summary(simple_run)
plot(simple_run)
