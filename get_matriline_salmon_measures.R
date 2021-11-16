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


###Some plots

# salmon.plot.df = data.frame(
#   day = 1:153,
#   salmon.rate = exp(colMeans(salmon_array[,10,])),
#   lCI = exp(colQuantiles(salmon_array[,10,], prob = 0.05)),
#   uCI = exp(colQuantiles(salmon_array[,10,], prob = 0.95))
#   
# )
# ggplot(salmon.plot.df, aes(day, salmon.rate, colour = "a", fill = "a"))+
#   geom_line(size = 2)+
#   geom_ribbon(aes(ymin = lCI, ymax = uCI), alpha = 0.5, colour = NA)+
#   scale_colour_manual(values = "hotpink2")+
#   scale_fill_manual(values = "hotpink2")+
#   ylab("Salmon Rate") +
#   xlab("Julian Day")+
#   theme_bw()+
#   theme(
#     axis.text = element_text(size = 30),
#     axis.title = element_text(size = 40),
#     legend.position = "none"
#   )
# 
# 
# #
# state.eg.df = data.frame(
#   day = 1:153,
#   state = ifelse(matriline_array[1,10,1,] == 1, 0, 1)
# )
# ggplot(state.eg.df, aes(day, state))+
#   geom_line(size = 2)+
#   ylab("State") +
#   xlab("Julian Day")+
#   theme_bw()+
#   theme(
#     axis.text.x = element_text(size = 30),
#     axis.text.y = element_text(size = 0),
#     axis.title = element_text(size = 40),
#     legend.position = "none"
#   )
# 
# 
# mat.salmon.eg = data.frame(
#   mat = seq(1, 37,1),
#   salmon.achieved = colMeans(mat_salmon, dim = 1)[10,]
# )
# mat.salmon.eg = filter(mat.salmon.eg, !is.na(salmon.achieved))
# mat.salmon.eg$mat = as.factor(mat.salmon.eg$mat)
# ggplot(mat.salmon.eg, aes(mat, salmon.achieved, fill = mat))+
#   geom_bar(stat = "identity")+
#   ylab("Salmon Achieved") +
#   xlab("Matriline")+
#   theme_bw()+
#   theme(
#     axis.text.x = element_text(size = 0),
#     axis.text.y = element_text(size = 30),
#     axis.title = element_text(size = 40),
#     legend.position = "none"
#   )
