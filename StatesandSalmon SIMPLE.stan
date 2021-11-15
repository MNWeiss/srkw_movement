data {
  
  //state model variavles
  int D; // Number of observations
  int M; //Number of matrilines
  int Y; //number of years
  int S[Y,D, M]; //sightings matrix
  int<lower = 0, upper = 1> S_extant[Y,M]; // whether each matriline is extant in each year
  
  // //salmon model parameters
  // int salmon_N;
  // int salmon_years[salmon_N];
  // int salmon_days[salmon_N];
  // int salmon_catch[salmon_N]; //catch matrx matrix
  // real salmon_effort[salmon_N]; //effort matrix
  
  
}

parameters {
  
  // Parameters of state model
  real<lower = 0.9, upper = 1> pd2;
  real<lower = 0, upper = 1> pd3;
  simplex[3] rho[Y,M]; // Initial state
  real<lower = 0, upper = 0.5> parrive;
  real<lower = 0, upper = 0.5> pleave;
  
  //parameters for the salmon model
  // vector[Y] b0;
  // vector[Y] b1;
  // vector[Y] b2;
  // vector[Y] b3;
  // vector[Y] b4;
  // vector[5] b_mu;
  // vector<lower =0>[5] b_tau;
  // matrix[Y, D] err;
  // real<lower = 0.01> tau_obs;
  
}

transformed parameters {
  
  //Transformed parameters for the STATE model
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
  mu[2] = pd2;
  mu[3] = pd3;
  
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
  
  // THE STATE MODEL
  
  //declare
  pd2 ~ uniform(0.9,1);
  pd3 ~ normal(0.25, 10); // Uniform priors seem to make it struggle to initialise. 
  parrive ~  normal(0.25, 10); 
  pleave ~ normal(0.25, 10); 
  
  //priors
  for(y in 1:Y){
      rho[y,] ~ dirichlet([1, 1, 1]);
  }
  
  //Model
  for(y in 1:Y){
    for(m in 1:M){
      if(S_extant[y,m]){
        target += hmm_marginal(log_omega[y, m, ,], Gamma, rho[y,m,]);
      }
    } 
  }
  
  
  // THE SALMON MODEL
  
  //declare
  // real lambda[salmon_N];
  // real X[salmon_N];
  // real salmon_days_std[salmon_N];
  // for(n in 1:salmon_N){  //days standardised to ease fitting
  //   salmon_days_std[n] = salmon_days[n]*1.0/max(salmon_days);
  // }
  // 
  // //priors
  // tau_obs ~ gamma(1, 0.01);
  // b0 ~ normal(b_mu[1], b_tau[1]);
  // b1 ~ normal(b_mu[2], b_tau[2]);
  // b2 ~ normal(b_mu[3], b_tau[3]);
  // b3 ~ normal(b_mu[4], b_tau[4]);
  // b4 ~ normal(b_mu[5], b_tau[5]);
  // b_mu ~ normal(0,1);
  // b_tau ~ gamma(1,0.01);
  // for(y in 1:Y){
  //   for(d in 1:D){
  //     err[y,d] ~normal (0, tau_obs);
  // 
  //   }
  // }
  // 
  // //model
  // for(n in 1:salmon_N){
  //   lambda[n] = 
  //           exp(
  //       b0[salmon_years[n]] +
  //       b1[salmon_years[n]]*salmon_days_std[n] +
  //       b2[salmon_years[n]]*(salmon_days_std[n]^2) +
  //       b3[salmon_years[n]]*(salmon_days_std[n]^3) +
  //       b4[salmon_years[n]]*(salmon_days_std[n]^4) +
  //       err[salmon_years[n],salmon_days[n]]
  //     );
  //     X[n] = lambda[n]*salmon_effort[n];// All works more smoothlty if multiply first
  // }
  // salmon_catch ~ poisson(X);

}

generated quantities {

  //The STATE MODEL
  // matrix[3,D] hidden_probs[Y,M];
  int y_sim[Y, M, D];
  // real<lower = 0, upper = 1> present[Y, M, D];

  for(y in 1:Y){
    for(m in 1:M){
      // hidden_probs[y,m,,] = hmm_hidden_state_prob(log_omega[y,m,,], Gamma, rho[y,m,]);
      y_sim[y,m,] = hmm_latent_rng(log_omega[y,m,,], Gamma, rho[y,m,]);
    }
  }

  // for(y in 1:Y){
  //   for(m in 1:M){
  //     for(d in 1:D){
  //       present[y,m,d] = y_sim[y,m,d] > 1;
  //     }
  //   }
  // }
  // 
  // // THE SALMON MODEL
  // real log_lam[Y,D];
  // real D_seq[D];
  // for(d in 1:D){
  //   D_seq[d] = d*1.0/D;
  // }
  // 
  // for(y in 1:Y){
  //     for(d in 1:D){
  //       log_lam[y,d] = b0[y] + b1[y]*D_seq[d] + b2[y]*D_seq[d]^2 + b3[y]*D_seq[d]^3 + b4[y]*D_seq[d]^4+err[y,d];
  //     }
  // }
  // 
  // // Combining models
  // 
  // real mat_salmon[Y,M];
  // real avg_salmon[Y];
  // real mat_lam[Y,M,D];
  // 
  // for(y in 1:Y){
  //   avg_salmon[y] = mean(exp(log_lam[y,]));
  // }
  // 
  // for(y in 1:Y){
  //   for(m in 1:M){
  //     for(d in 1:D){
  //       mat_lam[y,m,d] = exp(log_lam[y,d])*present[y,m,d];
  //     }
  //   }
  // }
  // 
  // for(y in 1:Y){
  //   for(m in 1:M){
  //     mat_salmon[y,m] = sum(mat_lam[y,m,])/sum(present[y,m,]);
  //   }
  // }

}
