rm(list = ls())

require(tidyverse)

source("load time series data.R")

attributes


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
  
  
  age = year - id.df$yob

  
  
  deaths[[i]] = data.frame(
    id = all.ids[i],
    sex = id.df$sex,
    mat = matriline[names(matriline)== all.ids[i]],
    year,
    age,
    died
  )
}
deaths
