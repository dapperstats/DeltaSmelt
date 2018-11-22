# single intercept only, no covariates

model {

# priors

  mu_delta ~ dnorm(-0.144, 1/(10^2))


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

# for tracking

  mu_delta_prime <- exp(mu_delta)

}