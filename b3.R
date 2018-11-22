# Define model
sink("Nmix.txt")
cat("
model {

# Priors
  log(lambda) <- loglam
  loglam ~ dunif(-10, 10)
  p0 ~ dunif(-10, 10)


# State equation

  for (i in 1:R) {
    N[i] ~ dpois(lambda)
  }

# Observation equation
  for(i in 1:R){
    for (t in 1:T) {
      y[i,t] ~ dbin(p[i,t], N[i])
      p[i,t] <- 1 / (1 + exp( -1 * (p0)))
    }
  }

# Derived quantities
  totalN <- sum(N[]) 
}
",fill=TRUE)
sink()