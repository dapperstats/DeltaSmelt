# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# JAGS script for model 7
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

  lambda ~ dunif(0, 0.999)
  tau_lgamma ~ dgamma(0.01, 0.01)
  sigma_lgamma <- 1 / sqrt(tau_lgamma)

# process model

  mu_ldelta <- X[1:NDS, 1:NX] %*% beta[1:NX]

# temporal autocorrelation (blocked by year)

  for(i in 1:MSPY){
    mu_lgamma[i] <- 0
  }
  for(i in 1:NYr){
    for(j in 1:MSPY){
      for(k in 1:MSPY){
        tdiff[i, j, k] <- abs(week[i, k] - week[i, j])
        SIGMA_lgamma[j, k, i] <- (1/tau_lgamma) * pow(lambda, tdiff[i, j, k])
      }
    }
    TAU_lgamma[1:MSPY, 1:MSPY, i] <- inverse(SIGMA_lgamma[1:MSPY, 1:MSPY, i])
    lgammaS[1:MSPY, i] ~ dmnorm(mu_lgamma[1:MSPY], 
                                TAU_lgamma[1:MSPY, 1:MSPY, i])
  }

  for(i in 1:NDS){
    lgamma[i] <- lgammaS[SO[i], Yrid[i]]
  }
  

# sampling model

  for(i in 1:NDS){
    ldelta[i] ~ dnorm(mu_ldelta[i] + lgamma[i], tau_ldelta)
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