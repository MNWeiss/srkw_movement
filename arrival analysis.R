require(stringr)

source("load_pedigree.R")

#getting raw data

years <- 1987:2019 # define the years we want

raw_data = read.csv("Data/frames 1976 to 2019.csv") #read in the encounter data from ID folders

raw_data$Date <- as.Date(raw_data$EncDate, format = "%d/%m/%Y") # format the dates as Dates

raw_data$IDs <- strsplit(raw_data$CleanIDs, ",") # get individual IDs in each frame

# determine whether each frame had any SRKW in it
any_srkw <- unlist(lapply(raw_data$IDs, function(z){
  any(grepl("K",z) | grepl("J",z) | grepl("L",z))
}))

raw_data <- raw_data[any_srkw & format(raw_data$Date, "%Y") %in% years,] # subset to the SRKW data in the years we want

all_ids <- unique(unlist(raw_data$IDs))
all_ids <- all_ids[all_ids %in% colnames(kin)]

kin <- kin[all_ids,all_ids]

matriline <- igraph::components(graph.adjacency(ifelse(kin > 0, 1, 0)))$membership
mats <- 1:max(matriline)

all_dates <- seq(min(raw_data$Date),max(raw_data$Date),1)
presence_data <- list()

for(i in 1:length(all_dates)){
  if(any(raw_data$Date == all_dates[i])){
    whales <- unique(unlist(raw_data$IDs[raw_data$Date == all_dates[i]]))
    mat_present <- unique(matriline[match(whales,all_ids)])
    presence <- ifelse(mats %in% mat_present, 1, 0)
    presence_data[[i]] <- data.frame(matriline = mats, presence = presence, date = all_dates[i])
  }else{
    presence_data[[i]] <- data.frame(matriline = mats, presence = 0, date = all_dates[i])
  }
}

presence_data <- do.call(rbind, presence_data)

presence_data$poss_arrival <- NA

for(i in mats){
  print(i)
  mat_data <- presence_data[presence_data$matriline == i,]
  mat_data$poss_arrival <- sapply(mat_data$date, function(z){
    ifelse(any(mat_data$date %in% seq(z-14,z-1,1) & mat_data$presence == 1), 0, 1)
  })
  presence_data[presence_data$matriline == i,] <- mat_data
}

arrival_data <- presence_data[presence_data$poss_arrival == 1 & format(presence_data$date, "%Y") %in% years & as.numeric(format(presence_data$date, "%m")) %in% 5:9,]

salmon <- read.csv("Data/albion.csv") # read in the Albion test fishery data

time_lag <- 10 # define the lag between salmon and whale data (10 days has been used before)

salmon$Date <- as.Date(salmon$Date, "%d-%b-%y") # get the dates as a Date object
salmon$lag_date <- salmon$Date - time_lag # get the lagged dates

arrival_data$CPUE <- sapply(arrival_data$date, function(z){
  mean(salmon$CPUE[salmon$lag_date %in% seq(z-2,z+2,1)],na.rm=T)
})

arrival_data$year <- format(arrival_data$date, "%Y")
arrival_data$year.fac <- as.factor(arrival_data$year)
arrival_data$julian <- as.numeric(format(arrival_data$date, "%j"))

mat_pod <- sapply(mats, function(z){
  unique(substr(all_ids[matriline == z],1,1))
})

all_yod <- attributes$yod[match(all_ids,attributes$id)]
all_yob <- attributes$yob[match(all_ids,attributes$id)]
all_sex <- attributes$sex[match(all_ids,attributes$id)]

arrival_data$pod <- as.factor(mat_pod[arrival_data$matriline])

arrival_data$mat_size <- arrival_data$n_ca <- arrival_data$n_am <- arrival_data$n_pr <- NA

for(m in mats){
  for(y in years){
    arrival_data$mat_size[arrival_data$matriline == m & arrival_data$year == y] <- sum(matriline == m & (all_yod >= y | is.na(all_yod)))
    arrival_data$n_pr[arrival_data$matriline == m & arrival_data$year == y] <- sum(matriline == m & (all_yod >= y | is.na(all_yod)) & (y - all_yob) >= 45 & all_sex == 0)
    arrival_data$n_am[arrival_data$matriline == m & arrival_data$year == y] <- sum(matriline == m & (all_yod >= y | is.na(all_yod)) & (y - all_yob) >= 21 & all_sex == 1)
    arrival_data$n_ca[arrival_data$matriline == m & arrival_data$year == y] <- sum(matriline == m & (all_yod >= y | is.na(all_yod)) & (y - all_yob) <= 2)
  }
}

arrival_data <- arrival_data[arrival_data$mat_size > 0,]

require(mgcv)

m1 <- gam(presence ~ pod*CPUE + n_pr*CPUE + s(julian, by = pod) + s(year.fac, bs = "re", by = pod), data = arrival_data, family = binomial)
