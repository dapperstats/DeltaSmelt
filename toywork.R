Y <- rpois(100, 0.8)
YY <- matrix(NA, nrow = 100, ncol = 3)
for(i in 1:100){
  YY[i,] <- rbinom(3, Y[i], 0.5)
}

toy_data <- list(NY = length(Y), Y = Y, YY = YY)

toy_run <- run.jags(model = "toy.R", modules = "glm", 
                      monitor = c("mu", "phi"),
                      data = toy_data, inits = inits_fnc_toy,
                      n.chains = 4,
                      adapt = 1000, burnin = 2000, sample = 2000, thin = 1,
                      summarise = FALSE, plots = FALSE, method = "parallel")

plot(toy_run)
summary(toy_run)