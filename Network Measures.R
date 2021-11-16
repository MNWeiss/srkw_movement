require(tidyverse)
require(igraph)

annual.networks = list()
for(i in 1:length(years)){
  annual.gbi = sighting_matrix[i,,]
  annual.gbi = annual.gbi[rowSums(annual.gbi) != 0,colSums(sighting_matrix[i,,])!=0]
  
  annual.networks[[i]] =  asnipe::get_network(annual.gbi, data_format = "GBI")
  
}



network.measures.byid = list()
for(i in 1:length(annual.networks)){
  net = igraph::graph_from_adjacency_matrix(annual.networks[[i]], mode = "undirected", weighted = TRUE)
  between = betweenness(net, directed = FALSE, weights = 1/igraph::E(net)$weight)
  strength = strength(net)

  network.measures.byid[[i]] =
    data.frame(
      id = V(net)$name,
      year = years[i],
      between.scaled = (between - mean(between))/sd(between),
      strength.scaled = (strength - mean(strength)/sd(strength))
    )
}
network.measures.byid = bind_rows(network.measures.byid)
network.measures.byid

###
annual.matnetworks = list()
for(i in 1:length(years)){
  annual.gbi = mat_sightings[i,,]
  colnames(annual.gbi) = as.character(seq(1, ncol(annual.gbi),1))
  annual.gbi = annual.gbi[rowSums(annual.gbi) != 0,colSums(mat_sightings[i,,])!=0]
  
  annual.matnetworks[[i]] =  asnipe::get_network(annual.gbi, data_format = "GBI")
  
}



matnetwork.measures = list()
for(i in 1:length(annual.matnetworks)){
  net = igraph::graph_from_adjacency_matrix(annual.matnetworks[[i]], mode = "undirected", weighted = TRUE)
  between = betweenness(net, directed = FALSE, weights = 1/igraph::E(net)$weight)
  strength = strength(net)
  
  matnetwork.measures[[i]] =
    data.frame(
      mat_ID =colnames(annual.matnetworks[[i]]),
      year = years[i],
      matbetween.scaled = (between - mean(between))/sd(between),
      matstrength.scaled = (strength - mean(strength)/sd(strength))
    )
}
matnetwork.measures = bind_rows(matnetwork.measures)
matnetwork.measures$mat_ID = as.numeric(matnetwork.measures$mat_ID)
matnetwork.measures



#############################################

network.measures.byid = 
  left_join(
    network.measures.byid,
    data.frame(id = names(mat), mat_ID = unname(mat))
  )


network.measures.bymatriline = 
network.measures.byid %>%
  group_by(year, mat_ID) %>%
  summarise(max.between = max(between.scaled),
            max.strength = max(strength.scaled)
            )








