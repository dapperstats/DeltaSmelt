data.fn <- function(R = 100, T = 10, lambda = 5, p0 = 1){

  N <- rpois(R, lambda)
  p <- plogis(p0)

  y <- array(dim = c(R, T))
  for(t in 1:T){
    y[,t] <- rbinom(R, N, p)
  }

# Return stuff
return(list(R=R, T=T, lambda=lambda, N=N, p0=p0, p=p, y=y))
}