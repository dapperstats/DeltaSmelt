# single intercept only, no covariates, add general error term

model {

# priors

  mu_delta ~ dnorm(-0.144, 1/(10^2))
  tau_epsilon ~ dgamma(0.001, 0.001) 

# latent model

  for(i in 1:MN){
    delta[i] <- mu_delta
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

}