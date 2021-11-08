source("Salmon_simple.R")

matriline_draws <- as.matrix(readRDS("post.draws_var200.RDS"))
#var250 has pd2 set to 1
# var200 uniform between 0.9 and 1

plot.df = data.frame(
  var = c(rep("pleave", dim(matriline_draws)[1]), rep("parrive", dim(matriline_draws)[1]), rep("pd2", dim(matriline_draws)[1]),  rep("pd3", dim(matriline_draws)[1])),
  values = c(matriline_draws[,"pleave"], matriline_draws[,"parrive"],  matriline_draws[,"pd2"], matriline_draws[,"pd3"])
)
ggplot(plot.df, aes(values, fill = var))+
  geom_density()+
  facet_wrap(var~., scales = "free", nrow = 2)

matriline_array <- array(matriline_draws[,substr(colnames(matriline_draws),1,5) == "y_sim"],dim = c(dim(matriline_draws)[1], dim(mat_sightings)[[1]],dim(mat_sightings)[[3]],dim(mat_sightings)[[2]] ))

mat_salmon <- array(dim = c(dim(matriline_draws)[1], dim(mat_sightings)[[1]], dim(mat_sightings)[[3]]))

mean_salmon <- apply( exp(salmon_array[1:dim(matriline_draws)[1],,]), c(2,1), mean)

for(i in 1:dim(matriline_draws)[1]){
  for(j in 1:dim(mat_sightings)[[3]]){
    for(k in 1:dim(mat_sightings)[[1]]){
      mat_presence <- matriline_array[i,k,j,]
      mat_presence <- ifelse(mat_presence == 1, 0, 1)
      salmon <- exp(salmon_array[i,k,])
      mat_salmon[i,k,j] <- (sum(salmon*mat_presence)/sum(mat_presence)) - mean_salmon[k,i]
    }
  }
}

mat_salmon
