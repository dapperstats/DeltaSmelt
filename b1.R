simdata <- data.fn()

win.data <- list(y = simdata$y, R = simdata$R, T = simdata$T)

Nst <- apply(simdata$y, 1, max) + 1
inits <- function(){list(N = Nst, sigma = rlnorm(1))}

params <- c("lambda", "loglam", "p0","totalN")
# Could include "N" to get local population size

nc <- 3
nb <- 1000
ni <- 2000
nt <- 1



outJ <- jags(win.data, inits, params, "Nmix.txt", n.chains=nc,
n.iter=ni, n.burn = nb, n.thin=nt)
