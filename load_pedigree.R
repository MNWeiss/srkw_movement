require(igraph)
require(kinship2)

attributes = read.csv("Data/Whale_Attributes_NR, SR_ALL_MASTER.csv") #attributes
attributes$id = as.character(attributes$id)
attributes = attributes[!grepl("?",attributes$id,fixed=T),]
attributes = attributes[!grepl("X",attributes$id,fixed=T),]
attributes = attributes[!duplicated(attributes$id),]

attributes$mother[grepl("?",attributes$mother,fixed=T)] = "UNK"
attributes$mother[attributes$mo_certainty > 2] = "UNK"
attributes$father = paste("XX",1:nrow(attributes),sep="")

ped_id = c(as.character(attributes$id), attributes$father)
ped_yob = c(attributes$yob,rep(NA,length(attributes$yob)))
ped_yod = c(attributes$yod,rep(NA,length(attributes$yod)))
ped_ped = rep(ifelse(attributes$Population..N..Northern..S...Southern.=="N",1,2),2)
ped_sex = c(ifelse(attributes$sex..0...females..1...males..2...uknown.==0,2,1), rep(1,nrow(attributes)))
ped_father = c(as.character(attributes$father),rep(NA,length(attributes$father)))
ped_mother = c(as.character(attributes$mother), rep(NA,length(attributes$father)))
ped_father[ped_mother=="UNK"|is.na(ped_mother)] = NA
ped_mother[ped_mother=="UNK"] = NA

pedigree = data.frame(id = ped_id, mother = ped_mother, father = ped_father, pedigree = ped_ped, sex = ped_sex, yob = ped_yob, yod = ped_yod)
ped=pedigree(id=pedigree$id,dadid=pedigree$father,momid=pedigree$mother,sex=pedigree$sex)
kin = kinship(ped)
kin = kin[row.names(kin) %in% attributes$id[attributes$Population..N..Northern..S...Southern.=="S"],colnames(kin) %in% attributes$id[attributes$Population..N..Northern..S...Southern.=="S"]]

kg = graph.adjacency(kin,weighted=T,mode="undirected")
mat = igraph::components(kg)$membership