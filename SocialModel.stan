data{
  int N;
  real mat_salmon[N];
  real social_var[N];
  
  int output_n;
  real output_seq[output_n];
  
}
parameters{
  real B0;
  real B1;
  real sigma;
}
model{
  real mu[N];
  
  for(i in 1:N){
    real mu_i;
    mu_i = B0 + B1*social_var[i];
    mu[i] = mu_i;
  }
  mat_salmon ~ normal(mu, sigma);
}

generated quantities{
  real post_mu[output_n];
  
  for(i in 1:output_n){
    post_mu[i] = B0 + B1*output_seq[i];
  }
}
