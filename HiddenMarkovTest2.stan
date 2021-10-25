// #### TO DO
//  # add new years indexing level
//  # sort out rho so it varies by matriline per year
//  # play with priors

data {
  int D; // Number of observations
  int M;
  int y[D, M];
}

parameters {
  // Parameters of measurement model
  real<lower = 0, upper = 1> pd;
  
  // Initial state
  simplex[3] rho;
  
  real<lower = 0, upper = 1> parrive;
  real<lower = 0, upper = 1> pleave;
  
}

transformed parameters {
  real<lower = 0, upper = 1> mu[3];
  matrix[3, 3] Gamma = rep_matrix(0, 3, 3);
  matrix[3, D] log_omega[M];
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
  for(m in 1:M){
    for(d in 1:D) {
      // The observation model could change with n, or vary in a number of
      //  different ways (which is why log_omega is passed in as an argument)
    log_omega[m, 1, d] = bernoulli_lpmf(y[d, m] | mu[1]);
    log_omega[m, 2, d] = bernoulli_lpmf(y[d, m] | mu[2]);
    log_omega[m, 3, d] = bernoulli_lpmf(y[d, m] | mu[3]);
    }
  }
}


model {
  
  // pd ~ beta(2, 10);
  // parrive ~  beta(2, 10); //loosen this with more data
  // pleave ~ beta(2, 10); // loosen this with more data 
  pd ~ uniform(0.001,0.5);
  parrive ~  uniform(0.001,0.5); //loosen this with more data
  pleave ~ uniform(0.001,0.5); // loosen this with more data 
  

  rho ~ dirichlet([10, 1, 1]);

  t1 ~ dirichlet([1, 1]);
  t2 ~ dirichlet([1, 1]);
  t3 ~ dirichlet([1, 1]);
  
  for(m in 1:M){
  target += hmm_marginal(log_omega[m, ,], Gamma, rho);
  }
}

generated quantities {
  matrix[3,D] hidden_probs[M];
  int y_sim[M, D];
  for(m in 1:M){
      hidden_probs[m,,] = hmm_hidden_state_prob(log_omega[m,,], Gamma, rho);
      y_sim[m,] = hmm_latent_rng(log_omega[m,,], Gamma, rho);
  }

}
