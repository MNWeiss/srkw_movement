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
    died = 0
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


#Just for now
deaths = filter(deaths, sex != 3)
deaths


