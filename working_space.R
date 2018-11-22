# a script to spread out and develop things that will get put into the others
#

# basic analyses of the data

names(prepped_data)
table(prepped_data$Y)

dens <- prepped_data$Y / prepped_data$V

uss <- unique(prepped_data$stationsurvey)
nss <- length(uss)
meandens <- rep(NA, nss)
vardens <- rep(NA, nss)
ndens <- rep(NA, nss)

plot(1, 1, type = 'n', xlim = c(1, nss), ylim = c(0, max(dens)))
for(i in 1:nss){
  matches <- which(prepped_data$stationsurvey == uss[i])
  dens_ss <- dens[matches]
  meandens[i] <- mean(dens_ss)
  vardens[i] <- var(dens_ss)
  ndens[i] <- length(dens_ss)
  points(rep(i, ndens[i]), dens_ss)
}

plot(meandens, vardens)

densmat <- matrix(NA, nrow = nss, ncol = 3)

for(i in 1:nss){
  matches <- which(prepped_data$stationsurvey == uss[i])
  densmat[i, 1:length(matches)] <- dens[matches]
}

plot(data.frame(densmat))
cor(densmat, use = "pairwise.complete.obs") 


Y <- prepped_data$Y
V <- prepped_data$V
ss <- prepped_data$stationsurvey
glmer(Y~1|ss, weight = V, family = "poisson")

glm(Y~1, family = gaussian)


mean(dens)
exp(mean(log(dens + 0.01))) - 0.01

exp(mean(log(rep(0, 10) + 0.01))) - 0.01


uss <- unique(prepped_data$stationsurvey)
nss <- length(uss)
meandens <- rep(NA, nss)
vardens <- rep(NA, nss)
ndens <- rep(NA, nss)

plot(1, 1, type = 'n', xlim = c(1, nss), ylim = c(0, max(dens)))
for(i in 1:nss){
  matches <- which(prepped_data$stationsurvey == uss[i])
  dens_ss <- dens[matches]
  meandens[i] <- mean(dens_ss)
  vardens[i] <- var(dens_ss)
  ndens[i] <- length(dens_ss)
  points(rep(i, ndens[i]), dens_ss)
}