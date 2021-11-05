source("Salmon_simple.R")

matriline_draws <- as.matrix(readRDS("post.draws_var250.RDS"))

matriline_array <- array(matriline_draws[,substr(colnames(matriline_draws),1,5) == "y_sim"],dim = c(250, dim(mat_sightings)[[1]],dim(mat_sightings)[[3]],dim(mat_sightings)[[2]] ))

mat_salmon <- array(dim = c(250, dim(mat_sightings)[[1]], dim(mat_sightings)[[3]]))
mean_salmon <- apply(exp(salmon_array[1:250,,]), c(2,1), mean)

for(i in 1:250){
  for(j in 1:dim(mat_sightings)[[3]]){
    for(k in 1:dim(mat_sightings)[[1]]){
      mat_presence <- matriline_array[i,k,j,]
      mat_presence <- ifelse(mat_presence == 1, 0, 1)
      salmon <- exp(salmon_array[i,k,])
      mat_salmon[i,k,j] <- (sum(salmon*mat_presence)/sum(mat_presence)) - mean_salmon[k,i]
    }
  }
}
