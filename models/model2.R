# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# JAGS script for model 2
#

model {

# priors

  beta ~ dnorm(-0.122, 1/(10^2))
  tau_ldelta ~ dgamma(0.01, 0.01)
  sigma_ldelta <- sqrt(1 / tau_ldelta)

# process model

  mu_ldelta <- beta

# sampling model

  for(i in 1:NDS){
    ldelta[i] ~ dnorm(mu_ldelta, tau_ldelta)
    delta[i] <- exp(ldelta[i])
  }

# observation model

  for(i in 1:NT){
    Y[i] ~ dpois(delta[DSid[i]] * V[i])
  }

}