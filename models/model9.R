# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# JAGS script for model 9
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
  lambda ~ dunif(0, 0.999)
  tau_lkappa ~ dgamma(0.01, 0.01)
  sigma_lkappa <- 1 / sqrt(tau_lkappa)

# process model

  mu_ldelta <- X[1:NDS, 1:NX] %*% beta[1:NX]


# spatiotemporal autocorrelation (blocked by year)

  for(i in 1:NYr){
    for(j in 1:MSPY){
      for(k in 1:NS){
        for(l in 1:MSPY){
          for(m in 1:NS){
            SIGMA_lkappa[(j - 1) * NS + k, (l - 1) * NS + m, i] <- 
                 (1 / tau_lkappa) * 
                 pow(lambda, abs(week[i, l] - week[i, j])) * 
                 exp(-phi * DM[k, m])
          } 
        }
      }
    }
    TAU_lkappa[1:(MSSPY), 1:(MSSPY), i] <-
      inverse(SIGMA_lkappa[1:(MSSPY), 1:(MSSPY), i])
    for(j in 1:(MSSPY)){
      mu_lkappa[j, i] <- 0
    }    
    lkappaS[1:(MSSPY), i] ~
      dmnorm(mu_lkappa[1:(MSSPY), i], TAU_lkappa[1:(MSSPY), 1:(MSSPY), i])
  }

  for(i in 1:NDS){
    lkappa[i] <- lkappaS[SSid[i], Yrid[i]]
  }
  
# sampling model

  for(i in 1:NDS){
    ldelta[i] ~ dnorm(mu_ldelta[i] + lkappa[i], tau_ldelta)
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