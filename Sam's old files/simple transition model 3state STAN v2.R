## Same as version 1 but with some changes that attempt the fix. BUT didn't work. Kept for interest. 

rm(list = ls())

source("load time series data.R")
require(rethinking)

simple_mode_stan <-
  "
      functions{
        // This function coverts a real value to an integer. A pain but necessarry
        // from https://discourse.mc-stan.org/t/real-to-integer-conversion/5622/8
        int bin_search(real x, int min_val, int max_val){
          int range = (max_val - min_val+1)/2;
          int mid_pt = min_val + range;
          int out;
          while(range > 0) {
            if(x == mid_pt){
              out = mid_pt;
              range = 0;
            } else {
              // figure out if range == 1
              range =  (range+1)/2;
              mid_pt = x > mid_pt ? mid_pt + range: mid_pt - range;
            }
          }
          return out;
        }
      }




data{
int Yrs;
int Nday;
int Nmat;
int detection[Yrs, Nday, Nmat];
int prestate[Yrs, Nmat];
}

parameters{

real pd;

real<lower = 0, upper = 1> parrive;
real<lower = 0, upper = 1> pleave;


matrix<lower = 0.51, upper = 3.5>[Nday, Nmat] state_real[Yrs];

}


model{

matrix[3,3] ptran;
real detect_prob[3];
vector[3] X;
int state_int[Yrs, Nday, Nmat];
//int Z2;

//Z ~ normal(2,0.1);
//Z2 = bin_search(round(Z), 0, 5);


//priors
pd ~ beta(2,2);
parrive ~ beta(2,2);
pleave ~ beta(2,2);

for(t in 1:Yrs){
  for(j in 1:Nmat){
     for(i in 1:Nday){
     state_real[t,i,j] ~ uniform(0.5,3.5);
     }
  }
}

for(t in 1:Yrs){
  for(j in 1:Nmat){
     for(i in 1:Nday){
     state_int[t,i,j] = bin_search(round(state_real[t,i,j]), 1, 3);
     }
  }
}


//state = rep_array(2,Yrs, Nday, Nmat);

  ptran[1,1] = 1- parrive; // #Gone to Gone
  ptran[1,2] = parrive; // #gone to newly arrived
  ptran[1,3] = 0; // #gone to staying
  ptran[2,1] = pleave; // #newly to gone
  ptran[2,2] = 0; // # newly to newly
  ptran[2,3] = 1 - pleave; // # newly to staying
  ptran[3,1] = pleave; // #stayed to gone
  ptran[3,2] = 0; // # stayed to newly
  ptran[3,3] = 1- pleave; // #stayed to stayed


detect_prob[1] = 0;
detect_prob[2] = 1;
detect_prob[3] = pd;



//initial
for(t in 1:Yrs){
for(j in 1:Nmat){
detection[t,1,j] ~ bernoulli(detect_prob[state_int[t,1,j]]);
state_int[t,1,j] = prestate[t,j];
}
}


//full
  for(t in 1:Yrs){
    for(j in 1:Nmat){
      for(i in 2:Nday){

        detection[t,i,j] ~ bernoulli(detect_prob[state_int[t,i,j]]) ;
        for(k in 1:3){
        X[k] = ptran[state_int[t,(i-1),j],k];
        }
        //print(X);
        //Z2 ~ categorical(X) ;
        //print(Z2);
        print(state_int[t,i,j]);
        print(state_real[t,i,j]);
        state_int[t,i,j] ~ categorical(X) ;
        print(state_int[t,i,j]);
        print(state_real[t,i,j]);

      }
    }
  }

}


"

pre_state = array(0, c(dim(pre_mat_sightings)[1],1,dim(pre_mat_sightings)[3]))
for(i in 1:dim(pre_mat_sightings)[1]){
  presighted = colSums(pre_mat_sightings[i,1:14,])
  day1 = pre_mat_sightings[i,15,]
  pre_state[i,,] = ifelse(presighted >0 , 3, # of the matriline has been sighted intial state is 3 (stayed)
                          ifelse(day1 > 0, 2, 1) # otherwise, if first started on day 1 = 2(newly sighted), otherwise 1 (away)
  )
}

jags_data <- list(
  Yrs = 10,
  Nday = 50,
  Nmat = 5,
  detection = mat_sightings[1:10,1:50,1:5],
  prestate = pre_state[1:10,1,1:5]
)


mod = stan(
  model_code = simple_mode_stan,
  data = jags_data,
  chains = 1,
  cores = 1,
  iter = 500,
  init = list(list(pd = 0.5, pleave = 1))
  )






require(rjags)
require(runjags)

simple_model_string <- "model{

  # Model for initial presence and detection
  
    for(t in 1:Yrs){
    for(j in 1:Nmat){
      for(i in 2:Nday){
      state[t,i,j] ~ dcat(p[1:3])
      }
    }
    }

  for(t in 1:Yrs){
      for(j in 1:Nmat){
        detection[t,1,j] ~ dbern(detect_prob[state[t,1,j]])
        #state[t,1,j] ~ dcat(p[1:3])
      }
  }
  
  p[1:3] ~ ddirch(a[1:3])
  
  
  # Transition model
  
  for(t in 1:Yrs){
    for(j in 1:Nmat){
      for(i in 2:Nday){
        
        detection[t,i,j] ~ dbern(detect_prob[state[t,i,j]]) # detect with given probability
        #state[t,i,j] ~ dcat(ptran[state[t,(i-1),j],1:3]) # draw state from categorical distribution given previous state
        
      }
    }
  }
  
  # Detection probability prior
  
  detect_prob[1] <- 0 # Its this that breaks it....
  detect_prob[2] <- pd2
  detect_prob[3] <- pd
  pd ~ dbeta(2,2)
  pd2 ~ dbeta(3,1)
  
  # set up year and matriline specific transistion matrices

  ptran[1,1] <- 1- parrive  #Gone to Gone
  ptran[1,2] <- parrive #gone to newly arrived
  ptran[1,3] <- 0 #gone to staying
  ptran[2,1] <- pleave #newly to gone
  ptran[2,2] <- 0 # newly to newly
  ptran[2,3] <- 1 - pleave # newly to staying
  ptran[3,1] <- pleave #stayed to gone
  ptran[3,2] <- 0 # stayed to newly
  ptran[3,3] <- 1- pleave #stayed to stayed

  
  pleave ~ dunif(0,0.5)
  parrive ~ dunif(0,0.5)

}"

# Set up the data

pre_state = array(0, c(dim(pre_mat_sightings)[1],1,dim(pre_mat_sightings)[3]))
for(i in 1:dim(pre_mat_sightings)[1]){
  presighted = colSums(pre_mat_sightings[i,1:14,])
  day1 = pre_mat_sightings[i,15,]
  pre_state[i,,] = ifelse(presighted >0 , 3, # of the matriline has been sighted intial state is 3 (stayed)
                          ifelse(day1 > 0, 2, 1) # otherwise, if first started on day 1 = 2(newly sighted), otherwise 1 (away)
                          ) 
}

# Currently subsetted for testing, but will eventually include all the data

jags_data <- list(
  Yrs = 10,
  Nday = 50,
  Nmat = 5,
  detection = mat_sightings[1:10,1:50,1:5],
  a = rep(1,3)
)

# Run the model, recording the population-level intercepts for leaving and arrival probabilities, and detection probabilities

start <- Sys.time()
simple_run <- run.jags(simple_model_string, data = jags_data, 
                       monitor = c("pleave","parrive","detect_prob[3]"), 
                       sample = 1000, burnin = 1000, n.chains = 4# ,
                       # keep.jags.files = TRUE, method = "interruptible"
                       )
end <- Sys.time()

end - start

summary(simple_run)
plot(simple_run$mcmc)
