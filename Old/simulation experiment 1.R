require(rjags)
require(runjags)

N <- 10
t <- 200

state <- rbinom(N,1,0.2) + 1

tprob <- matrix(c(1,0.1,0.2,1), nrow = 2)
tprob <- tprob/rowSums(tprob)

dprob <- c(0.2,0)

occurrence <- matrix(nrow = t, ncol = N)

burnin <- 200

for(i in 1:burnin){
  for(j in 1:N){
    state[j] <- sample(2,size=1,prob=tprob[state[j],])
  }
}

for(i in 1:t){
  for(j in 1:N){
    state[j] <- sample(2,size=1,prob=tprob[state[j],])
  }
  occurrence[i,] <- rbinom(n = N, size = 1, prob = dprob[state])
}


simple_model_string <- "model{

  # Model for initial presence and detection

  for(i in 1:Nind){
    detection[1,i] ~ dbern(detection_prob[state[1,i]])
    state[1,i] ~ dcat(pinit[1:2])
  }
  
  pinit[1:2] ~ ddirch(a[1:2])

  for(t in 2:Nsamp){
    for(i in 1:Nind){
      detection[t,i] ~ dbern(detection_prob[state[t,i]])
      state[t,i] ~ dcat(ptran[state[(t-1),i],1:2])
    }
  }
  
  detection_prob[1] <- pd
  detection_prob[2] <- 0
  
  pd ~ dunif(0,1)
  
  ptran[1,1] <- 1-pleave
  ptran[1,2] <- pleave
  ptran[2,2] <- 1-parrive
  ptran[2,1] <- parrive
  
  parrive ~ dunif(0,0.5)
  pleave ~ dunif(0,0.5)

}"


# Set up the data
# Currently subsetted for testing, but will eventually include all the data

jags_data <- list(
  Nsamp = t,
  Nind = N,
  a = rep(1,2),
  detection = occurrence
)

# Run the model, recording the population-level intercepts for leaving and arrival probabilities, and detection probabilities

simple_run <- run.jags(simple_model_string, data = jags_data, monitor = c("pleave","parrive","detection_prob","state"), sample = 20000, burnin = 1000, n.chains = 4)
