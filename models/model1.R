# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# JAGS script for model 1
#

model {

# priors

  beta ~ dnorm(-0.122, 1/(4^2))

# process model

  mu_ldelta <- beta

# sampling model

  delta <- exp(beta)

# observation model

  for(i in 1:NT){
    Y[i] ~ dpois(delta * V[i])
  }

}