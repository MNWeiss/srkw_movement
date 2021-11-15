source("get_matriline_salmon_measures.R")


#########################
# Prepared data
##########################

matriline_data <- list()

all_sex <- attributes$sex..0...females..1...males..2...uknown.[match(all_ids, attributes$id)]

mat_salmon_mean <- apply(mat_salmon, c(2,3), median, na.rm = T)
mean_salmon_means = rowMeans(mean_salmon)

for(i in 1:length(years)){
  age <- years[i] - yob
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
  age_oldest <- sapply(mats, function(z){
    max(age[matriline == z & is.alive == 1])
  })
  age_oldest_f <- sapply(mats, function(z){
    max(age[matriline == z & is.alive == 1 & all_sex == 0])
  })
  matriline_data[[i]] <- data.frame(year = years[i], mat_ID = mats, pod  = pod, mat_size, has_prf, has_am, has_calf, mat_salmon = mat_salmon_mean[i,], mean_salmon = mean_salmon[i],age_oldest, age_oldest_f)
}

matriline_data <- do.call(rbind,matriline_data)
matriline_data <- matriline_data[matriline_data$mat_size > 0,]
matriline_data$pod <- as.factor(matriline_data$pod)
matriline_data$age_oldest_f[is.infinite(matriline_data$age_oldest_f)] <- NA

matriline_data$pod.has.prf = numeric(nrow(matriline_data))
matriline_data$pod.oldest = numeric(nrow(matriline_data))
for(i in 1:nrow(matriline_data)){
  df = filter(matriline_data, 
              year == matriline_data$year[i],
              pod == matriline_data$pod[i]
  )
  matriline_data$pod.has.prf[i] = ifelse(sum(df$has_prf) >0, 1, 0)
  matriline_data$pod.oldest[i] = max(df$age_oldest)
}

###############
#Models
###########################

zscore = function(x){x-mean(x)/sd(x)}
rev_zscore = function(x,y){(x*sd(y))-mean(y)}
 
social_data = list(
  N = nrow(matriline_data),
  mat_salmon = matriline_data$mat_salmon,
  social_var = zscore(matriline_data$mat_size),
  
  output_n = 100,
  output_seq = seq(quantile(social.zed, 0.05), quantile(social.zed, 0.95), length.out = 100)
)

social.mod = 
  stan(
    file = "SocialModel.stan",
    data = social_data,
    chains = 4,
    cores = 4
  )

traceplot(social.mod)
precis(social.mod)

post = extract(social.mod)
plot.df = 
  data.frame(
    matriline.size = rev_zscore(social_data$output_seq, matriline_data$mat_size),
    p.mean = colMeans(post$post_mu),
    p.lCI  = colQuantiles(post$post_mu, probs = 0.2),
    p.uCI  = colQuantiles(post$post_mu, probs = 0.8)
  )
ggplot(plot.df, aes(matriline.size, p.mean))+
  geom_line(size = 2)+
  geom_ribbon(aes(ymin = p.lCI, ymax = p.uCI), alpha  = 0.5, colour = NA) +
  xlab("Matriline Size")+
  ylab("Salmon Achieved") +
  theme_bw() +
  theme(
    # axis.text = element_text(size = 30),
    # axis.title = element_text(size = 40),
    legend.position = "none"
  )


# # PRF in matriling
# 
# PRFmat_data= list(
#   N = nrow(matriline_data),
#   mat_salmon = matriline_data$mat_salmon,
#   mat_size = matriline_data$has_prf,
#   
#   social_seq = c(0,1)
# )
# 
# social.mod = 
#   stan(
#     file = "SocialModel.stan",
#     data = social_data,
#     chains = 4,
#     cores = 4
#   )
# 
# traceplot(social.mod)
# precis(social.mod)
# 
# post = extract(social.mod)
# 
# social.plot = 
#   data.frame(
#     # matriline.size = ((social_data$social_seq)*sd(matriline_data$mat_size)) + mean(matriline_data$mat_size),
#     has.prf = social_data$social_seq,
#     p.mean = colMeans(post$post_mu),
#     p.lCI  = colQuantiles(post$post_mu, probs = 0.2),
#     p.uCI  = colQuantiles(post$post_mu, probs = 0.8)
#   )
# ggplot(social.plot, aes(as.factor(has.prf), p.mean))+
#   geom_point(size = 10)+
#   geom_errorbar(aes(ymin = p.lCI, ymax = p.uCI), width = 0, size = 2) +
#   xlab("Has post-reproductive female")+
#   ylab("Salmon Achieved") +
#   theme_bw() +
#   theme(
#     axis.text = element_text(size = 30),
#     axis.title = element_text(size = 40),
#     legend.position = "none"
#   )
# 
# 
# #####################################
# 
# social_data = list(
#   N = nrow(matriline_data),
#   mat_salmon = matriline_data$mat_salmon,
#   mat_size = matriline_data$pod.has.prf,
#   
#   social_seq = c(0,1)
# )
# 
# social.mod = 
#   stan(
#     file = "SocialModel.stan",
#     data = social_data,
#     chains = 4,
#     cores = 4
#   )
# 
# traceplot(social.mod)
# precis(social.mod)
# 
# post = extract(social.mod)
# 
# social.plot = 
#   data.frame(
#     # matriline.size = ((social_data$social_seq)*sd(matriline_data$mat_size)) + mean(matriline_data$mat_size),
#     has.prf = social_data$social_seq,
#     p.mean = colMeans(post$post_mu),
#     p.lCI  = colQuantiles(post$post_mu, probs = 0.2),
#     p.uCI  = colQuantiles(post$post_mu, probs = 0.8)
#   )
# ggplot(social.plot, aes(as.factor(has.prf), p.mean))+
#   geom_point(size = 10)+
#   geom_errorbar(aes(ymin = p.lCI, ymax = p.uCI), width = 0, size = 2) +
#   xlab("Has post-reproductive female")+
#   ylab("Salmon Achieved") +
#   theme_bw() +
#   theme(
#     axis.text = element_text(size = 30),
#     axis.title = element_text(size = 40),
#     legend.position = "none"
#   )
# 
# ##########################################
# # Matriline size
# 
# social.zed = (matriline_data$pod.oldest - mean(matriline_data$pod.oldest))/ sd(matriline_data$pod.oldest)
# social_data = list(
#   N = nrow(matriline_data),
#   mat_salmon = matriline_data$mat_salmon,
#   mat_size = social.zed,
#   
#   social_seq = seq(quantile(social.zed, 0.05), quantile(social.zed, 0.95), length.out = 100)
# )
# 
# social.mod = 
#   stan(
#     file = "SocialModel.stan",
#     data = social_data,
#     chains = 4,
#     cores = 4
#   )
# 
# traceplot(social.mod)
# precis(social.mod)
# 
# post = extract(social.mod)
# 
# social.plot = 
#   data.frame(
#     matriline.size = ((social_data$social_seq)*sd(matriline_data$pod.oldest)) + mean(matriline_data$pod.oldest),
#     p.mean = colMeans(post$post_mu),
#     p.lCI  = colQuantiles(post$post_mu, probs = 0.2),
#     p.uCI  = colQuantiles(post$post_mu, probs = 0.8)
#   )
# ggplot(social.plot, aes(matriline.size, p.mean))+
#   geom_line(size = 2)+
#   geom_ribbon(aes(ymin = p.lCI, ymax = p.uCI), alpha  = 0.5, colour = NA) +
#   xlab("Age of the oldest female in the POD")+
#   ylab("Salmon Achieved") +
#   theme_bw() +
#   theme(
#     axis.text = element_text(size = 30),
#     axis.title = element_text(size = 40),
#     legend.position = "none"
#   )
# 
# ##########################################
# # Matriline size
# 
# social.zed = (matriline_data$age_oldest - mean(matriline_data$age_oldest))/ sd(matriline_data$age_oldest)
# social_data = list(
#   N = nrow(matriline_data),
#   mat_salmon = matriline_data$mat_salmon,
#   mat_size = social.zed,
#   
#   social_seq = seq(quantile(social.zed, 0.05), quantile(social.zed, 0.95), length.out = 100)
# )
# 
# social.mod = 
#   stan(
#     file = "SocialModel.stan",
#     data = social_data,
#     chains = 4,
#     cores = 4
#   )
# 
# traceplot(social.mod)
# precis(social.mod)
# 
# post = extract(social.mod)
# 
# social.plot = 
#   data.frame(
#     matriline.size = ((social_data$social_seq)*sd(matriline_data$pod.oldest)) + mean(matriline_data$pod.oldest),
#     p.mean = colMeans(post$post_mu),
#     p.lCI  = colQuantiles(post$post_mu, probs = 0.05),
#     p.uCI  = colQuantiles(post$post_mu, probs = 0.95)
#   )
# ggplot(social.plot, aes(matriline.size, p.mean))+
#   geom_line(size = 2)+
#   geom_ribbon(aes(ymin = p.lCI, ymax = p.uCI), alpha  = 0.5, colour = NA) +
#   xlab("Age of the oldest female in the MATRILINE")+
#   ylab("Salmon Achieved") +
#   theme_bw() +
#   theme(
#     axis.text = element_text(size = 30),
#     axis.title = element_text(size = 40),
#     legend.position = "none"
#   )
