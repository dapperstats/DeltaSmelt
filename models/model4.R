# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# JAGS script for model 4
#

model {

# priors

  beta[1] ~ dnorm(-0.122, 1/(4^2))
  for(i in 2:NX){
    beta[i] ~ dnorm(0, 1/(2.5^2))
  }
  tau_ldelta ~ dgamma(0.001, 0.001)
  sigma_ldelta <- sqrt(1 / tau_ldelta)

  chi ~ dgamma(1, 0.5)

# process model

  mu_ldelta <- X[1:NDS, 1:NX] %*% beta[1:NX]

# sampling model

  for(i in 1:NDS){
    ldelta[i] ~ dnorm(mu_ldelta[i], tau_ldelta)
    delta[i] <- exp(ldelta[i])
  }

# observation model

  for(i in 1:NT){
    lzeta[i] <- -chi * W[i]
    zeta[i] <- exp(lzeta[i])
    Y[i] ~ dpois(delta[DSid[i]] * V[i] * zeta[i])
  }

}