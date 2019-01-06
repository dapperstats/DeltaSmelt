obs_pred_cor <- function(data, model){
  pp <- paste0("output/model_output/model", model, "_postpred.RData")  
  load(pp)
  predY <- post$post_pred_Y
  obsY <- log(data$Y + 0.1)
  predY <- (apply(log(predY + 0.1), 2, mean)) 
  cor(obsY, predY)
}

r2 <- function(data, model){
  pp <- paste0("output/model_output/model", model, "_postpred.RData")  
  load(pp)

  obsY <- log(data$Y + 0.1)
  predY <- post$post_pred_Y
  predY <- (apply(log(predY + 0.1), 2, mean)) 

  r2y <- 1 - var(obsY - predY)/ var(obsY)


  obsD <- log(data$Y + 0.1)/data$V
  predD <- post$post_pred_Y
  predD <- (apply(log(predD + 0.1), 2, mean)) 
  predD <- predD/data$V
  DSid <- data$DSid

  nd <- data$NDS
  obsD2 <- rep(NA, nd)
  predD2 <- rep(NA, nd)
  for(j in 1:nd){
    obsD2[j] <- mean(obsD[which(DSid == j)])
    predD2[j] <- mean(predD[which(DSid == j)])
  }

  r2d <- 1 - var(obsD - predD)/ var(obsD)

  c(y = r2y, d = r2d)
}

peak_descrips <- function(data, model){
  mcmc_c <- combine_mod_chains(model)
  mcmc_c

  X <- data$X
  FOYsd <- data$msd["FOYsd"]
  FOYm <- data$msd["FOYm"]
  YEARsd <- data$msd["YEARsd"]
  YEARm <- data$msd["YEARm"] 
  lECavsd <- data$msd["lECavsd"]
  lECavm <- data$msd["lECavm"]
  lSEsd <- data$msd["lSEsd"]
  lSEm <- data$msd["lSEm"]
  lCALsd <- data$msd["lCALsd"]
  lCALm <- data$msd["lCALm"]
  VELsd <- data$msd["VELsd"]
  VELm <- data$msd["VELm"]

  predictionFROYR <- seq(.18, .61, .001)
  PredictionFROYRtrans <- (predictionFROYR - FOYm) / FOYsd  
  peakFOY <- rep(NA, nrow(mcmc_c))
  for(i in 1:nrow(mcmc_c)){

   yy <- PredictionFROYRtrans * mcmc_c[i,2] + 
         PredictionFROYRtrans^2 * mcmc_c[i,3]
   peakFOY[i] <- predictionFROYR[which.max(yy)]
  }
  cat("Jday\n")
  print(round(quantile(peakFOY, c(0.5, 0.025, 0.975))*365))



  predictionYR <- seq(1, 20, 0.01)
  PredictionYRtrans <- (predictionYR - YEARm) / YEARsd  
  peakYR <- rep(NA, nrow(mcmc_c))
  for(i in 1:nrow(mcmc_c)){

   yy <- PredictionYRtrans * mcmc_c[i,4] + 
         PredictionYRtrans^2 * mcmc_c[i,5]
   peakYR[i] <- predictionYR[which.max(yy)]
  }
  cat("Year\n")
  print(round(quantile(peakYR, c(0.5, 0.025, 0.975))))

  predictionlEC <- seq(3.2, 10.4, 0.001)
  PredictionlECtrans <- (predictionlEC - lECavm) / lECavsd  
  peaklEC <- rep(NA, nrow(mcmc_c))
  for(i in 1:nrow(mcmc_c)){

   yy <- PredictionlECtrans * mcmc_c[i,6] + 
         PredictionlECtrans^2 * mcmc_c[i,7]
   peaklEC[i] <- predictionlEC[which.max(yy)]
  }
  cat("EC\n")
  print(round(quantile(exp(peaklEC), c(0.5, 0.025, 0.975))))


  predictionlsecchi <- seq(1.5, 5.5, 0.001)
  predictionlsecchitrans <- (predictionlsecchi - lSEm) / lSEsd  
  peakls <- rep(NA, nrow(mcmc_c))
  for(i in 1:nrow(mcmc_c)){

   yy <- predictionlsecchitrans * mcmc_c[i,8] + 
         predictionlsecchitrans^2 * mcmc_c[i,9]
   peakls[i] <- predictionlsecchi[which.max(yy)]
  }
  cat("Secchi\n")
  print(round(quantile(exp(peakls), c(0.5, 0.025, 0.975))))


  predictionlcal <- seq(-2.4, 11.1, 0.01)
  predictionlcaltrans <- (predictionlcal - lCALm) / lCALsd  
  peaklc <- rep(NA, nrow(mcmc_c))
  for(i in 1:nrow(mcmc_c)){

   yy <- predictionlcaltrans * mcmc_c[i,10] + 
         predictionlcaltrans ^2* mcmc_c[i,11]
   peaklc[i] <- predictionlcal [which.max(yy)]
  }
  cat("Calanoids\n")
  print(round(quantile(exp(peaklc), c(0.5, 0.025, 0.975))))



  predictionVel <- seq(-1, 4.6, 0.001)
  predictionVeltrans <- (predictionVel - VELm) / VELsd 
  peaklv <- rep(NA, nrow(mcmc_c))
  for(i in 1:nrow(mcmc_c)){

   yy <- predictionVeltrans * mcmc_c[i,12] + 
         predictionVeltrans ^2* mcmc_c[i,13]
   peaklv[i] <- predictionVel[which.max(yy)]
  }
  cat("Velocity\n")
  print(round(quantile((peaklv), c(0.5, 0.025, 0.975)), 3))

}


mcmc_table <- function(model){
  mod_mcmc_loc <- paste0("output/model_output/model", model, "_mcmc.RData")  
  load(mod_mcmc_loc)
  mod_mcmc <- mcmc

  ncoef <- ncol(mod_mcmc[[1]])
  coefnames <- colnames(mod_mcmc[[1]])
  sumnames <- c("median", "mean", "sd", "ci", "fr0", "mcerr", "mcpersd",
                "ssef", "ac10", "psrf")
  nsums <- length(sumnames)
  out <- data.frame(matrix(NA, ncoef, nsums))
  rownames(out) <- coefnames
  colnames(out) <- sumnames
  for(i in 1:ncoef){
    out[i,] <- coefsum(mod_mcmc, i)
  }
  tname <- paste0("output/model_output/model", model, "table.csv")
  write.csv(out, tname)
  out
}

coefsum <- function(mod_mcmc, i){
  sumnames <- c("median", "mean", "sd", "ci", "fr0", "mcerr", "mcpersd",
                "ssef", "ac10", "psrf")
  nchains <- length(mod_mcmc)
  itpchain <- nrow(mod_mcmc[[1]])
  mod_mcmc_i <- matrix(NA, ncol = nchains, nrow = itpchain)
  for(j in 1:nchains){
    mod_mcmc_i[,j] <- mod_mcmc[[j]][,i]
  }
  stde <- summary(mod_mcmc)$statistics[i,2]
  out_i <- c(round(median(mod_mcmc_i), 4),
             round(mean(mod_mcmc_i), 4),
             round(sd(mod_mcmc_i), 4),
             paste(round(quantile(mod_mcmc_i, c(0.025, 0.975)), 4),
                   collapse = " - "),
             round(length(which(mod_mcmc_i > 0)) / length(mod_mcmc_i), 3),
             round(stde / sqrt(effectiveSize(mod_mcmc)[i]), 4),
             round(
             (stde / sqrt(effectiveSize(mod_mcmc)[i])) / sd(mod_mcmc_i) * 100, 
             1),
             round(effectiveSize(mod_mcmc)[i], 0),
             round(autocorr.diag(mod_mcmc, lags = 10)[i], 2),
             round(gelman.diag(mod_mcmc)[[1]][i,1], 2))
  names(out_i) <- sumnames
  out_i
}


summaryTows <- function(Y, V, DSid, TO, W){
  NDS <- max(DSid)
  denmat <- matrix(NA, NDS, 3)
  catchmat <- matrix(NA, NDS, 3)
  velmat <- matrix(NA, NDS, 3)
  for(i in 1:NDS){
    specs <- which(DSid == i)
    for(j in 1:length(specs)){
      denmat[i, TO[specs[j]]] <- Y[specs[j]] / V[specs[j]]
      catchmat[i, TO[specs[j]]] <- Y[specs[j]] 
      velmat[i, TO[specs[j]]] <- W[specs[j]] 
    }
  }

  diffmat <- denmat
  diffmat[,2] <- denmat[,2] - denmat[,1]
  diffmat[,3] <- denmat[,3] - denmat[,2]
  diffmat <- na.omit(diffmat)
  diffmat <- diffmat[-which(diffmat[,2] == 0 & diffmat[,3] == 0), ]
  towdiffs <- data.frame(cbind(
                               mean = apply(diffmat, 2, mean),
                               var = apply(diffmat, 2, var),
                               min = apply(diffmat, 2, min),
                               max = apply(diffmat, 2, max)))
  towdiffs <- towdiffs[-1, ]
  rownames(towdiffs) <- c("1-2", "2-3")

  corrs <- round(cor(denmat, use = "pairwise.complete.obs") , 3)
  completions <- table(3 - apply(is.na(denmat), 1, sum))
  completions <- rbind(completions, round(completions / NDS, 3))
  catches <- table(apply(na.omit(catchmat) != 0, 1, sum))
  catches <- rbind(catches, round(catches / nrow(na.omit(catchmat)), 3))
  list("correlations among tows" = corrs, "completed tows" = completions, 
       "tows with catches" = catches, 
       "differences from previous tow" = towdiffs)
}

summaryD <- function(Y, V){
  round(
    c(mean = mean(Y/V), median = median(Y/V), var = var(Y/V),
      min = min(Y/V), max = max(Y/V)),
    3)
}

summaryV <- function(V){
  round(
    c(mean = mean(V), median = median(V), var = var(V), min = min(V),
      max = max(V), 
      prop_0.75_1.25 = length(V[V > 0.75 & V <= 1.25]) / length(V)),
    4)
}



summaryY <- function(Y){
  round(
    c(mean = mean(Y), median = median(Y), var = var(Y), 
      min = min(Y), max = max(Y), prop0 = fr0(Y), median_non0 = mn0(Y)),
    3)
  
}



mn0 <- function(x, ...){
  nz <- x[x!=0]
  mnz <- median(nz)
  return(mnz)
}


fr0 <- function(x, ...){
  fr <- length(x[x==0])/length(x)
  return(fr)
}