data {
  int N;
  int sex[N];
  int mat[N];
  int year[N];
  int age_cat[N];
  int died[N];
  
  int Niter;
  int Nyears;
  int Nmats;
  real mat_salmon[Niter, Nyears, Nmats];
  real mean_salmon[Nyears, Niter];
  int mat_extant[Nyears, Nmats];
  
  vector[100] mean_salmon_seq;
  vector[100] mat_salmon_seq;
  real mean_salmon_mean;
  real mean_mat_salmon;
  
}

parameters{
  
  // real<lower = 0.25, upper = 0.75> B0[2];
  real Bage[8];
  real Bmu_salmon[1];
  real Bmat_salmon[1];
  
}

model{
  vector[N] p;
  
  //priors
  // B0 ~ normal(0.5,5);
  Bage ~ normal(-3.38,0.05);
  Bmu_salmon ~ normal(0,5);
  Bmat_salmon ~ normal(0,5);
  
  for(i in 1:N){
    real p_i;
    if(mat_extant[year[i], mat[i]]){
          p_i = Bage[age_cat[i]] + (Bmu_salmon[1]*mean_salmon[year[i]-1,1])+(Bmat_salmon[1]*mat_salmon[1,year[i]-1,mat[i]]);
          p[i] = inv_logit(p_i);
    }

  }
  
  died ~ bernoulli(p);
}

generated quantities{

  vector[100] post_p_F_meansalmon;
  vector[100] post_p_M_meansalmon;
  
  vector[100] post_p_F_matsalmon;
  vector[100] post_p_M_matsalmon;

  for(i in 1:100){
    post_p_F_meansalmon[i] = inv_logit(Bage[4]  + (Bmu_salmon[1]*mean_salmon_seq[i])+(Bmat_salmon[1]*mean_mat_salmon));
    post_p_M_meansalmon[i] = inv_logit(Bage[8]  + (Bmu_salmon[1]*mean_salmon_seq[i])+(Bmat_salmon[1]*mean_mat_salmon));
    
    post_p_F_matsalmon[i] = inv_logit(Bage[4]  + (Bmu_salmon[1]*mean_salmon_mean)+(Bmat_salmon[1]*mat_salmon_seq[i]));
    post_p_M_matsalmon[i] = inv_logit(Bage[8]  + (Bmu_salmon[1]*mean_salmon_mean)+(Bmat_salmon[1]*mat_salmon_seq[i]));
  }



}