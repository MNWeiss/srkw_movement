rm(list = ls())
source("load time series data.R")

require(dplyr)



all.ids = unique(attributes[attributes[,2] == "S" & attributes$yob <=2019 & attributes[3] != 2,]$id )
all.ids

surv.df = list()
for(i in 1:length(all.ids)){
  yob = filter(attributes, id == all.ids[i])$yob
  yod = filter(attributes, id == all.ids[i])$yod
  
  yo.entry = ifelse(yob<1987, 1987, yob)
  yo.exit = ifelse(is.na(yod), 2019, yod)
  
  years = yo.entry:yo.exit
  
  surv.df[[i]] = 
    data.frame(
      id = all.ids[i],
      year = years,
      dead = ifelse(years != yod | is.na(yod), 0, 1 ),
      sex = ifelse(filter(attributes, id == all.ids[i])$sex == 0, 1, 2),
      age = years - yob
    )
  
  
}
surv.df = bind_rows(surv.df)
surv.df

require(rethinking)
stancode = 
 " 
  data{
    int N;
    int dead[N];
    vector[N] age;
    int sex[N];
  }

parameters{
  vector[2] B0;
  vector[2] B1;
  vector[2] B2;
}

model{
  real mu[N];
  
  B0 ~ normal(0,1);
  B1 ~ normal(0,1);
  B2 ~ normal(0,1);
  
  for(i in 1:N){
    mu[i] = inv_logit(B0[sex[i]] + B1[sex[i]]*age[i] + B2[sex[i]]*(age[i]^2));
  }
  
  dead ~ bernoulli(mu);
  
}
"


mod.dat = list(
  N = nrow(surv.df),
  age = surv.df$age/max(surv.df$age),
  sex = surv.df$sex,
  dead = surv.df$dead
)

mod = stan(
  model_code = stancode,
  data = mod.dat,
  chains = 4,
  cores = 4,
  iter = 500
)


precis(mod, depth = 2)
traceplot(mod)
