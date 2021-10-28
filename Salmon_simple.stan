data {
  int D; // Number of observations
  int Y; //number of years
  
  int salmon_N;
  int salmon_years[salmon_N];
  int salmon_days[salmon_N];
  int salmon_catch[salmon_N]; //catch matrx matrix
  real salmon_effort[salmon_N]; //effort matrix
}

parameters{

  vector[Y] b0;
  vector[Y] b1;
  vector[Y] b2;
  vector[Y] b3;
  vector[Y] b4;

  vector[5] b_mu;
  vector<lower =0>[5] b_tau;
  
  matrix[Y, D] err;
  real<lower = 0.01> tau_obs;
}

model{
  
  //declare
  real lambda[salmon_N];
  real X[salmon_N];
  real salmon_days_std[salmon_N];
  
  //days standardised to ease fitting
  for(n in 1:salmon_N){
    salmon_days_std[n] = salmon_days[n]*1.0/max(salmon_days);
  }
  
  //priors
  tau_obs ~ gamma(1, 0.01);
  
  b0 ~ normal(b_mu[1], b_tau[1]);
  b1 ~ normal(b_mu[2], b_tau[2]);
  b2 ~ normal(b_mu[3], b_tau[3]);
  b3 ~ normal(b_mu[4], b_tau[4]);
  b4 ~ normal(b_mu[5], b_tau[5]);
  
  b_mu ~ normal(0,1);
  b_tau ~ gamma(1,0.01);


  
  for(y in 1:Y){
    for(d in 1:D){
      err[y,d] ~normal (0, tau_obs);

    }
  }
  
  //model
  for(n in 1:salmon_N){
    lambda[n] = 
            exp(
        b0[salmon_years[n]] +
        b1[salmon_years[n]]*salmon_days_std[n] +
        b2[salmon_years[n]]*(salmon_days_std[n]^2) +
        b3[salmon_years[n]]*(salmon_days_std[n]^3) +
        b4[salmon_years[n]]*(salmon_days_std[n]^4) +
        err[salmon_years[n],salmon_days[n]]
      );
      X[n] = lambda[n]*salmon_effort[n];// All works more smoothlty if multiply first
  }
  salmon_catch ~ poisson(X);
  
}

generated quantities{
  matrix[Y,D] log_lam;
  
  real D_seq[D];
  for(d in 1:D){
    D_seq[d] = d*1.0/D;
  }

for(y in 1:Y){
    for(d in 1:D){
    log_lam[y,d] = b0[y] + b1[y]*D_seq[d] + b2[y]*D_seq[d]^2 + b3[y]*D_seq[d]^3 + b4[y]*D_seq[d]^4+err[y,d];
  }
}

}