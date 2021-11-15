data{
  int N;
  real mat_salmon[N];
  real social_var[N];
  int pod[N];
  
  int output_n;
  real output_seq[output_n];
  
}
parameters{
  real B0;
  real B1;
  real<lower = 0> sigma;
  
  vector[3] B_pod;
  real pods_mu;
  real<lower = 0> pods_sigma;
}
model{
  real mu[N];
  
  //priors
  B0 ~ normal(0,1);
  B1 ~ normal(0,1);
  sigma ~ exponential(1);
  
  pods_mu~ normal(0,1);
  pods_sigma ~ exponential(1);
  B_pod ~ normal(pods_mu, pods_sigma);
  
  for(i in 1:N){
    real mu_i;
    mu_i = B_pod[pod[i]] + B1*social_var[i];
    mu[i] = mu_i;
  }
  mat_salmon ~ normal(mu, sigma);
}

generated quantities{
  real post_mu[output_n];
  
  for(i in 1:output_n){
    post_mu[i] = pods_mu + B0 + B1*output_seq[i];
  }
}
