source("get_matriline_salmon_measures.R")


attributes


CALF= 0:1
JUVENILE = 2:14
REPRODUCTIVE = 15:44
PR.F = 45:150
SEX.MAT = 15:21
PHYS.MAT = 22:150

age.cat.key = 
  data.frame(
    age = seq(0,150, 1),
    F.age.cat = c(rep("calf", length(CALF)), 
                  rep("juve", length(JUVENILE)),
                  rep("repro", length(REPRODUCTIVE)),
                  rep("post.repo", length(PR.F))
    ),
    M.age.cat = c(rep("calf", length(CALF)), 
                  rep("juve", length(JUVENILE)),
                  rep("sex.mat", length(SEX.MAT)),
                  rep("phys.mat", length(PHYS.MAT))
    ),
    F.age.cat.n = c(rep(1, length(CALF)), 
                  rep(2, length(JUVENILE)),
                  rep(3, length(REPRODUCTIVE)),
                  rep(4, length(PR.F))
    ),
    M.age.cat.n = c(rep(5, length(CALF)), 
                  rep(6, length(JUVENILE)),
                  rep(7, length(SEX.MAT)),
                  rep(8, length(PHYS.MAT))
    )
  )
  
  
all.ids = names(matriline)
deaths = list()
for(i in 1:length(all.ids)){
  id.df = filter(attributes, id == all.ids[i])

  year.in = ifelse(id.df$yob >=1987, id.df$yob, 1987)
  year.out = ifelse(is.na(id.df$yod) | id.df$yod > 2019, 2019, id.df$yod)
  
  if(year.out < year.in){
    year = 1986
    died = NA
  } else {
    year = seq(year.in, year.out, 1)
    died = rep(0, length(year))
    if(!is.na(id.df$yod) & year.out == id.df$yod){
      died[length(year)] = 1
    }
  }
  
  
  ages = year - id.df$yob
  
  if(id.df$sex ==0 ){
    age.cat = filter(age.cat.key, age %in% ages)$F.age.cat.n
  } else {
    #treats unknowns as males but it doesn't matter becasue they'll all either be calfs or juves which are common to both
    age.cat = filter(age.cat.key, age %in% ages)$M.age.cat.n
  }
  
  
  year.n = year - 1986
  
  deaths[[i]] = data.frame(
    id = all.ids[i],
    sex = id.df$sex + 1,
    mat = matriline[names(matriline)== all.ids[i]],
    year = year.n,
    age = ages,
    age.cat,
    died
  )
}
deaths = as_tibble(bind_rows(deaths))
deaths = filter(deaths, !is.na(died)) # these are ids only alive pre the study

#Just for now
deaths = filter(deaths, sex != 3)

deaths


##############################
#
###################################

deaths.in = filter(deaths, year >1) # using year before salmon so can't use first year

mat_salmonII = ifelse(is.nan(mat_salmon), 0, mat_salmon)
mat_salmonII = colMeans(mat_salmonII, dims = 1) # for now done on the mean salmon. to be redone over the array in the furture
mean_salmonII = rowMeans(mean_salmon)

surv_mod_dat = list(
  
  N = nrow(deaths.in),
  sex = deaths.in$sex,
  mat = deaths.in$mat,
  year = deaths.in$year,
  age_cat = deaths.in$age.cat,
  died = deaths.in$died,
  
  Niter = 200,
  Nyears = 33,
  Nmats = 37,
  mat_salmon = mat_salmonII,
  mean_salmon = mean_salmonII,
  mat_extant = mat_extant,
  
  mean_salmon_seq = seq(quantile(mean_salmon, 0.1), quantile(mean_salmon, 0.9), length.out = 100),
  mat_salmon_seq = seq(quantile(mat_salmonII, 0.1), quantile(mat_salmonII, 0.9), length.out = 100),
  mean_salmon_mean = quantile(mean_salmon, 0.1),
  mean_mat_salmon = mean(mat_salmonII[mat_extant ==1])
)


surv.mod = 
  stan(file = "survival.stan", data = surv_mod_dat, iter = 500, chains = 4, cores = 4 )

traceplot(surv.mod)
out = precis(surv.mod, depth = 2)
out

post = extract(surv.mod)



meansalmon.plot = 
  data.frame(
    mean_salmon = rep(surv_mod_dat$mean_salmon_seq,2),
    sex = c(rep("F", 100), rep("M",100)),
    p.mean = c(colMeans(post$post_p_F_meansalmon), colMeans(post$post_p_M_meansalmon)),
    p.lCI  = c(colQuantiles(post$post_p_F_meansalmon, probs = 0.05), colQuantiles(post$post_p_M_meansalmon, probs = 0.05)),
    p.uCI  = c(colQuantiles(post$post_p_F_meansalmon, probs = 0.95), colQuantiles(post$post_p_M_meansalmon, probs = 0.95))
  )
ggplot(meansalmon.plot, aes(mean_salmon, p.mean, colour = sex, fill = sex))+
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = p.lCI, ymax = p.uCI), alpha  = 0.5)


matsalmon.plot = 
  data.frame(
   mat_salmon = rep(surv_mod_dat$mat_salmon_seq,2),
    sex = c(rep("F", 100), rep("M",100)),
    p.mean = c(colMeans(post$post_p_F_matsalmon), colMeans(post$post_p_M_matsalmon)),
    p.lCI  = c(colQuantiles(post$post_p_F_matsalmon, probs = 0.2), colQuantiles(post$post_p_M_matsalmon, probs = 0.2)),
    p.uCI  = c(colQuantiles(post$post_p_F_matsalmon, probs = 0.8), colQuantiles(post$post_p_M_matsalmon, probs = 0.8))
  )
ggplot(matsalmon.plot, aes(mat_salmon, p.mean, colour = sex, fill = sex))+
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = p.lCI, ymax = p.uCI), alpha  = 0.5)


#best plot
ggplot(filter(matsalmon.plot, sex == "F"), aes(mat_salmon, p.mean, colour = sex, fill = sex))+
  geom_line(size = 2)+
  geom_ribbon(aes(ymin = p.lCI, ymax = p.uCI), alpha  = 0.5, colour = NA) +
  xlab(" 'Salmon Achieved' ")+
  ylab("Probability of Dying") +
  scale_colour_manual(values = "firebrick4")+
  scale_fill_manual(values = "firebrick4")+
  theme_bw() +
  theme(
    axis.text = element_text(size = 30),
    axis.title = element_text(size = 40),
    legend.position = "none"
  )



