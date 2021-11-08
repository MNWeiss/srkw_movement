source("get_matriline_salmon_measures.R")

require(brms)

matriline_data <- list()

all_sex <- attributes$sex..0...females..1...males..2...uknown.[match(all_ids, attributes$id)]

mat_salmon_mean <- apply(mat_salmon, c(2,3), median, na.rm = T)

for(i in 1:length(years)){
  is.prf <- ifelse(all_sex == 0 & (years[i] - yob) >= 45, 1, 0)
  is.am <- ifelse(all_sex == 1 & (years[i] - yob) >= 21, 1, 0)
  is.calf <- ifelse((years[i]-yob) <= 2, 1, 0)
  is.alive <- ifelse(yob <= years[i] & (is.na(yod) | yod >= years[i]), 1, 0)
  mat_size <- sapply(mats, function(z){
    sum(is.alive[matriline == z])
  })
  has_prf <- sapply(mats, function(z){
    sum(is.prf == 1 & is.alive == 1 & matriline == z)
  })
  has_am <- sapply(mats, function(z){
    ifelse(any(is.am == 1 & is.alive == 1 & matriline == z), 1, 0)
  })
  has_calf <- sapply(mats, function(z){
    ifelse(any(is.calf == 1 & is.alive == 1 & matriline == z), 1, 0)
  })
  matriline_data[[i]] <- data.frame(year = years[i], mat_ID = mats, pod  = pod, mat_size, has_prf, has_am, has_calf, salmon = mat_salmon_mean[i,])
}

matriline_data <- do.call(rbind,matriline_data)
matriline_data <- matriline_data[matriline_data$mat_size > 0,]
matriline_data$pod <- as.factor(matriline_data$pod)

salmon_prf_model <- brm(salmon ~ has_prf + mat_size + (1|mat_ID) + (1|year), data = matriline_data)

plot(salmon_prf_model)
summary(salmon_prf_model)
