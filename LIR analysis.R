source("load time series data.R")

all_dates <- unique(raw_data$Date)
all_ids <- unique(unlist(raw_data$IDs))

sighting_matrix <- matrix(0, nrow = length(all_dates), ncol = length(all_ids))
colnames(sighting_matrix) <- all_ids
row.names(sighting_matrix) <- all_dates

for(i in 1:length(all_dates)){
  ids <- unique(unlist(raw_data$IDs[raw_data$Date == all_dates[i]]))
  sighting_matrix[i,ids] <- 1
}

day_diff <- as.matrix(dist(all_dates))

max_t <- 100

pairs <- which(day_diff <= max_t & upper.tri(day_diff), arr.ind = T)
lag <- apply(pairs, 1, function(z) day_diff[z[1],z[2]] )

m <- ni <- nj <- NA

for(p in 1:nrow(pairs)){
  i <- pairs[p,1]
  k <- pairs[p,2]
  m[p] <- sum(sighting_matrix[i,]*sighting_matrix[k,])
  ni[p] <- sum(sighting_matrix[i,])
  nj[p] <- sum(sighting_matrix[k,])
}

n <- ni+nj

data <- data.frame(m,lag,ni,nj)

all_lags <- 1:max_t
all_m <- sapply(all_lags, function(z){
  sum(m[lag == z])
})
all_n <- sapply(all_lags, function(z){
  sum(n[lag == z])
})

LIR_data <- data.frame(lag = all_lags, m = all_m, n = all_n, lir = all_m/all_n)

plot(lir ~ lag, data = LIR_data, type = "l")
abline(v = 14)
