model {

# priors

  rho ~ dunif(0.9, 0.95)
  tau_epsilon ~ dgamma(0.001, 0.001) 
  mu_delta ~ dnorm(-0.144, 1/(10^2))

# for tracking

  sigma_epsilon <- 1/sqrt(tau_epsilon)


# latent model

  for(i in 1:MN){
    delta[i] ~ dnorm(mu_delta, tau_epsilon)
  }

# sampling model

  for(i in 1:NT){
    delta2[i] <- delta[stationsurvey[i]]
    meann[i] <- exp(delta2[i] * V[i])
    potn[i] ~ dpois(meann[i])
  }

# observation model

  for(i in 1:NT){
    Y[i] ~ dbinom(1, potn[i])
    #Y[i] ~ dbinom(1, potn[i])
  }

}