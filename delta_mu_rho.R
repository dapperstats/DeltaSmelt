# single intercept only, no covariates

model {

# priors

  delta_mu ~ dnorm(-0.144, 1/(10^2))
  rho ~ dunif(0.001, 0.999)

# latent model

  for(i in 1:MN){
    delta[i] <- delta_mu
  }

# sampling model

  for(i in 1:NT){
    delta2[i] <- delta[stationsurvey[i]]
    meann[i] <- exp(delta2[i]) * V[i]
  }

# observation model

  for(i in 1:NT){
    Y[i] ~ dpois(meann[i])
  }

# for tracking


}