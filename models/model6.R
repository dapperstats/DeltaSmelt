# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# JAGS script for model 6
#

model {

# priors

  beta[1] ~ dnorm(-0.122, 1/(4^2))
  for(i in 2:NX){
    beta[i] ~ dnorm(0, 1/(2.5^2))
  }
  tau_ldelta ~ dgamma(0.001, 0.001)
  sigma_ldelta <- sqrt(1 / tau_ldelta)

  chi ~ dnorm(0, 1/(0.5^2)) I(-2, 2)
  tau_lzeta ~ dgamma(0.001, 0.001)
  sigma_lzeta <- sqrt(1 / tau_lzeta)

  phi ~ dgamma(1, 10)
  tau_leta ~ dgamma(0.001, 0.001)
  sigma_leta <- 1 / sqrt(tau_leta)

# process model

  mu_ldelta <- X[1:NDS, 1:NX] %*% beta[1:NX]

# spatial autocorrelation (blocked by year)

  for(i in 1:NS){
    mu_leta[i] <- 0
  }

  for(i in 1:NS){
    for(j in 1:NS){
      SIGMA_leta[i, j] <- (1 / tau_leta) * exp(-phi * DM[i, j])
    } 
  }
  TAU_leta[1:NS, 1:NS] <- inverse(SIGMA_leta[1:NS, 1:NS])

  for(i in 1:NYr){
    letaS[1:NS, i] ~ dmnorm(mu_leta[1:NS], TAU_leta[1:NS, 1:NS])
  }

  for(i in 1:NDS){
    leta[i] <- letaS[Sid[i], Yrid[i]]
  }
  
# sampling model

  for(i in 1:NDS){
    ldelta[i] ~ dnorm(mu_ldelta[i] + leta[i], tau_ldelta)
    delta[i] <- exp(ldelta[i])
  }

# observation model

  for(i in 1:NT){
    mu_lzeta[i] <- chi * W[i]
    lzeta[i] ~ dnorm(mu_lzeta[i], tau_lzeta)
    zeta[i] <- exp(lzeta[i])
    Y[i] ~ dpois(delta[DSid[i]] * V[i] * zeta[i])
  }

}