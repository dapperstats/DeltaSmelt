model {


  mu ~ dnorm(0, 1/(10^2))
  #phi ~ dunif(0.001, 0.999)


  for(i in 1:NY){
    ss[i] ~ dpois(exp(mu))
    for(j in 1:3){
      rho[i,j] <- 0.5
      YY[i,j] ~ dbin(0.5, ss[i])
    }
  }


}