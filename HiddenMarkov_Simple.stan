// #### TO DO
//  # add new years indexing level
//  # sort out rho so it varies by matriline per year

data {
  int D; // Number of observations
  int M;
  int Y;
  int S[Y,D, M];
}

parameters {
  // Parameters of measurement model
  real<lower = 0, upper = 0.5> pd;
  
  // Initial state
  simplex[3] rho[Y,M];
  
  real<lower = 0, upper = 0.5> parrive;
  real<lower = 0, upper = 0.5> pleave;
  
}

transformed parameters {
  real<lower = 0, upper = 1> mu[3];
  matrix[3, 3] Gamma = rep_matrix(0, 3, 3);
  matrix[3, D] log_omega[Y,M];
  // Rows of the transition matrix
  simplex[2] t1;
  simplex[2] t2;
  simplex[2] t3;
  
  t1[1] = 1-parrive;
  t1[2] = parrive;
  t2[1] = pleave;
  //t2[2] = 0.01;
  t2[2] = 1-pleave;
  t3[1] = pleave;
  //t3[2] = 0.01;
  t3[2] = 1 - pleave;
  
  mu[1] = 0;
  mu[2] = 1;
  mu[3] = pd;
  
  
  
  // Build the transition matrix
  Gamma[1, 1:2] = t1';
  Gamma[2, 1] = t2[1];
  Gamma[2, 3] = t2[2];
  Gamma[3, 1] = t3[1];
  Gamma[3, 3] = t3[2];

  // Compute the log likelihoods in each possible state
  for(y in 1:Y){
    for(m in 1:M){
      for(d in 1:D) {
      // The observation model could change with n, or vary in a number of
      //  different ways (which is why log_omega is passed in as an argument)
    log_omega[y, m, 1, d] = bernoulli_lpmf(S[y, d, m] | mu[1]);
    log_omega[y, m, 2, d] = bernoulli_lpmf(S[y, d, m] | mu[2]);
    log_omega[y, m, 3, d] = bernoulli_lpmf(S[y, d, m] | mu[3]);
    }
  }
  }

}


model {
  
  // pd ~ beta(2, 10);
  // parrive ~  beta(2, 10); //loosen this with more data
  // pleave ~ beta(2, 10); // loosen this with more data 
  pd ~ normal(0.25, 10); // Uniform priors seem to make it struggle to initialise. 
  parrive ~  normal(0.25, 10); 
  pleave ~ normal(0.25, 10); 
  
  for(y in 1:Y){
      rho[y,] ~ dirichlet([1, 1, 1]);
  }

  for(y in 1:Y){
    for(m in 1:M){
      target += hmm_marginal(log_omega[y, m, ,], Gamma, rho[y,m,]);
    } 
  }

}

generated quantities {
  matrix[3,D] hidden_probs[Y,M];
  int y_sim[Y, M, D];
  for(y in 1:Y){
    for(m in 1:M){
      hidden_probs[y,m,,] = hmm_hidden_state_prob(log_omega[y,m,,], Gamma, rho[y,m,]);
      y_sim[y,m,] = hmm_latent_rng(log_omega[y,m,,], Gamma, rho[y,m,]);
    }
  }


}
