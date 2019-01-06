

##############################################################################
#
# inits_fun
#
# prepares the initial values for a model
#
# Inputs: the model number


inits_fun <- function(model){
  rngs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
            "base::Super-Duper", "base::Mersenne-Twister")
  out <- function(chain = chain){NULL}
  if (model == 1){
    out <- function(chain = chain){
             list(.RNG.name = sample(rngs, 1),
                  .RNG.seed = sample(1:1e+06, 1),
                  beta = rnorm(1, -0.122, 0.8))
           }
  }
  if (model == 2){
    out <- function(chain = chain){
             list(.RNG.name = sample(rngs, 1),
                  .RNG.seed = sample(1:1e+06, 1),
                  beta = rnorm(1, -0.122, 0.8),
                  tau_ldelta = rgamma(1, shape = 1.2, rate = 0.7))
           }
  }
  if (model == 3){
    out <- function(chain = chain){
             list(.RNG.name = sample(rngs, 1),
                  .RNG.seed = sample(1:1e+06, 1),
                  beta = c(rnorm(1, -0.122, 0.8), rnorm(data$NX - 1, 0, 2)),
                  tau_ldelta = rgamma(1, shape = 1.2, rate = 0.7))
           }
  }
  if (model == 4){
    out <- function(chain = chain){
             list(.RNG.name = sample(rngs, 1),
                  .RNG.seed = sample(1:1e+06, 1),
                  beta = c(rnorm(1, -0.122, 0.8), rnorm(data$NX - 1, 0, 2)),
                  tau_ldelta = rgamma(1, shape = 1.2, rate = 0.7),
                  chi = rgamma(1, shape = 1, rate = 10))
           }
  }
  if (model == 5){
    out <- function(chain = chain){
             list(.RNG.name = sample(rngs, 1),
                  .RNG.seed = sample(1:1e+06, 1),
                  beta = c(rnorm(1, -0.122, 0.8), rnorm(data$NX - 1, 0, 2)),
                  tau_ldelta = rgamma(1, shape = 1.2, rate = 0.7),
                  chi = rnorm(1, 0, 0.5),
                  tau_lzeta = rgamma(1, shape = 1.2, rate = 0.7))
           }
  }
  if (model == 6){
    out <- function(chain = chain){
             list(.RNG.name = sample(rngs, 1),
                  .RNG.seed = sample(1:1e+06, 1),
                  beta = c(rnorm(1, -0.122, 0.8), rnorm(data$NX - 1, 0, 2)),
                  tau_ldelta = rgamma(1, shape = 1.2, rate = 0.7),
                  chi = rnorm(1, 0, 0.5),
                  tau_lzeta = rgamma(1, shape = 1.2, rate = 0.7),
                  phi = rgamma(1, shape = 1, rate = 10),
                  tau_leta = rgamma(1, shape = 1.2, rate = 0.7))
           }
  }
  if (model == 7){
    out <- function(chain = chain){
             list(.RNG.name = sample(rngs, 1),
                  .RNG.seed = sample(1:1e+06, 1),
                  beta = c(rnorm(1, -0.122, 0.8), rnorm(data$NX - 1, 0, 2)),
                  tau_ldelta = rgamma(1, shape = 1.2, rate = 0.7),
                  chi = rnorm(1, 0, 0.5),
                  tau_lzeta = rgamma(1, shape = 1.2, rate = 0.7),
                  lambda = runif(1, 0.3, 0.7),
                  tau_lgamma = rgamma(1, shape = 1.2, rate = 0.7))
           }
  }
  if (model == 8){
    out <- function(chain = chain){
             list(.RNG.name = sample(rngs, 1),
                  .RNG.seed = sample(1:1e+06, 1),
                  beta = c(rnorm(1, -0.122, 0.8), rnorm(data$NX - 1, 0, 2)),
                  tau_ldelta = rgamma(1, shape = 1.2, rate = 0.7),
                  chi = rnorm(1, 0, 0.5),
                  tau_lzeta = rgamma(1, shape = 1.2, rate = 0.7),
                  phi = rgamma(1, shape = 1, rate = 10),
                  tau_leta = rgamma(1, shape = 1.2, rate = 0.7),
                  lambda = runif(1, 0.3, 0.7),
                  tau_lgamma = rgamma(1, shape = 1.2, rate = 0.7))
           }
  }
  if (model == 9){
    out <- function(chain = chain){
             list(.RNG.name = sample(rngs, 1),
                  .RNG.seed = sample(1:1e+06, 1),
                  beta = c(rnorm(1, -0.122, 0.8), rnorm(data$NX - 1, 0, 2)),
                  tau_ldelta = rgamma(1, shape = 1.2, rate = 0.7),
                  chi = rnorm(1, 0, 0.5),
                  tau_lzeta = rgamma(1, shape = 1.2, rate = 0.7),
                  phi = rgamma(1, shape = 1, rate = 10),
                  lambda = runif(1, 0.3, 0.7),
                  tau_lkappa = rgamma(1, shape = 1.2, rate = 0.7))
           }
  }

  out
}


run_mod <- function(model, data, mcmc = c(8, 1000, 1000, 5000, 1), 
                    keep = FALSE){

  n.chains <- mcmc[1]
  adapt <- mcmc[2]
  burnin <- mcmc[3]
  sample <- mcmc[4]
  thin <- mcmc[5]
  modelfile <- paste0("models/model", model, ".R")
  
  monitor <- NULL
  if (model == 1){
    monitor <- c("beta")
  } else if (model == 2){
    monitor <- c("beta", "tau_ldelta", "sigma_ldelta")
  } else if (model == 3){
    monitor <- c("beta", "tau_ldelta", "sigma_ldelta")
  } else if (model == 4){
    monitor <- c("beta", "tau_ldelta", "sigma_ldelta", "chi")
  } else if (model == 5){
    monitor <- c("beta", "tau_ldelta", "sigma_ldelta", 
                 "chi", "tau_lzeta", "sigma_lzeta")
  } else if (model == 6){
    monitor <- c("beta", "tau_ldelta", "sigma_ldelta", 
                 "chi", "tau_lzeta", "sigma_lzeta",
                 "phi", "tau_leta", "sigma_leta")
  } else if (model == 7){
    monitor <- c("beta", "tau_ldelta", "sigma_ldelta", 
                 "chi", "tau_lzeta", "sigma_lzeta",
                 "lambda", "tau_lgamma", "sigma_lgamma")
  } else if (model == 8){
    monitor <- c("beta", "tau_ldelta", "sigma_ldelta", 
                 "chi", "tau_lzeta", "sigma_lzeta",
                 "phi", "tau_leta", "sigma_leta",
                 "lambda", "tau_lgamma", "sigma_lgamma")
  } else if (model == 9){
    monitor <- c("beta", "tau_ldelta", "sigma_ldelta", 
                 "chi", "tau_lzeta", "sigma_lzeta",
                 "phi", "lambda", "tau_lkappa", "sigma_lkappa")
  }
  keeploc <- keep
  if(keep){
    keeploc <- paste0("output/model_output/model", model)
  }

  mod <- run.jags(model = modelfile, modules = "glm", monitor = monitor,
           inits = inits_fun(model), data = data, n.chains = n.chains,
           adapt = adapt, burnin = burnin, sample = sample, thin = thin,
           summarise = FALSE, plots = FALSE, method = "parallel",
           keep.jags.files = keeploc)
  mcmcname <- paste0("output/model_output/model", model, "_mcmc.RData") 
  mcmc <- mod$mcmc
  save(mcmc, file = mcmcname)
  mod
}


extend_mod <- function(model, newname, mcmc = c(0, 0, 5000)){

  adapt <- mcmc[1]
  burnin <- mcmc[2]
  sample <- mcmc[3]

  if("runjags" %in% class(model)){ 
    jagsobj <- model
    keeploc <- paste0("output/model_output/model", newname)
  }else{
    modelfile <- paste0("output/model_output/model", model)
    jagsobj <- results.jags(modelfile)
    keeploc <- paste0("output/model_output/model", newname)
  }

  extend.jags(jagsobj, adapt = adapt, burnin = burnin, sample = sample, 
           summarise = FALSE, plots = FALSE, method = "parallel",
           keep.jags.files = keeploc)
}


combine_mod_chains <- function(model, thinfreq = 1, burnin = 0, 
                               model1 = NULL, model2 = NULL){

  if(is.null(model1)){
    mod_mcmc_loc <- paste0("output/model_output/model", model, "_mcmc.RData")  
    load(mod_mcmc_loc)
    mod_mcmc <- mcmc
  } else{
    if(is.null(model2)){
      mod_out_loc <- paste0("output/model_output/model", model)  
      mod_in <- results.jags(mod_out_loc, summarise = TRUE)
    } else {
      fn <- paste0("output/model_output/model", model, "_", model2, ".RData")  
      load(fn)
      on <- paste0("m", model, "_", model2)
      mod_in <- eval(parse(text=on))
    }
    mod_mcmc <- as.mcmc.list(mod_in)
  }
  nchains <- length(mod_mcmc)
  init1 <- 1 + burnin
  niter <- nrow(mod_mcmc[[1]]) - burnin
  niterthin <- floor(niter / thinfreq)
  nparam <- ncol(mod_mcmc[[1]])
  totiter <- nchains * niterthin
  out <- matrix(NA, totiter, nparam)


  thins <- seq(init1, nrow(mod_mcmc[[1]]), by = thinfreq)

  for(i in 1:nchains){
    chain_rows <- ((i - 1) * niterthin + 1):(i * niterthin)
    out[chain_rows, ] <- mod_mcmc[[i]][thins, ]
  }
  out
}


eval_mod <- function(model, data, DMY_inv = NULL, nsamps = 1000, 
                     thinfreq = 1, nsamps_MI = 500,
                     burnin = 0, model1 = NULL, model2 = NULL){

  m_mcmc <- combine_mod_chains(model, thinfreq, burnin, model1, model2)
  niter <- nrow(m_mcmc)
  nobs <- length(data$Y)
  post_pred_Y <- matrix(NA, nsamps, nobs)
  post_pred_D <- matrix(NA, nsamps, nobs)
  samps <- sample(1:niter, nsamps)
  for(i in 1:nsamps){
    post_pred_Y[i, ] <- pps(model, data, m_mcmc[samps[i], ])
    post_pred_D[i, ] <- post_pred_Y[i, ] / data$V
  }
  post_mean_D <- apply(post_pred_D, 1, mean)
  post_var_D <- apply(post_pred_D, 1, var)
  post_fr0_Y <- apply(post_pred_Y, 1, fr0)
  post_mn0_Y <- apply(post_pred_Y, 1, mn0)
  post_chi2 <- chi2(post_pred_Y, data, m_mcmc, model)
  post_FT <- FT(post_pred_Y, data, m_mcmc, model)
  if (is.null(DMY_inv)){
    post_MI <- NULL
  } else {
    post_MI <- MI(data, DMY_inv, m_mcmc, model, nsamps_MI)
  }

  post <-   list(post_pred_Y = post_pred_Y, post_pred_D = post_pred_D, 
                 post_mean_D = post_mean_D, post_var_D = post_var_D, 
                 post_fr0_Y = post_fr0_Y, post_mn0_Y = post_mn0_Y,
                 post_chi2 = post_chi2, post_FT = post_FT, post_MI = post_MI)
  fn <- paste0("output/model_output/model", model, "_postpred.RData")  
  save(post, file = fn)
  post
}


MI <- function(data, DMY_inv, m_mcmc, model, nsamps_MI){

  niter <- nrow(m_mcmc)
  samps <- sample(1:niter, nsamps_MI)
  MI_o <- rep(NA, nsamps_MI)
  MI_e <- rep(NA, nsamps_MI)
  MI_sd <- rep(NA, nsamps_MI)
  MI_p <- rep(NA, nsamps_MI)

  for(i in 1:nsamps_MI){
    Yobs <- data$Y
    Yexp <- mod_expected_Y(model, data, m_mcmc[i, ])

    MI <- Moran.I(data$Y - Yexp, DMY_inv)
    MI_o[i] <- MI$observed 
    MI_e[i] <- MI$expected 
    MI_sd[i] <- MI$sd 
    MI_p[i] <- MI$p.value
  }
  p_omni <- length(MI_o[MI_o<MI_e]) / nsamps_MI
  list(p_omni = p_omni, MI_o = MI_o, MI_e = MI_e, MI_sd = MI_sd, MI_p = MI_p)
}




chi2 <- function(pred_Y, data, m_mcmc, model){

  npreds <- nrow(pred_Y)
  chi2_o <- rep(NA, npreds)
  chi2_p <- rep(NA, npreds)

  for(i in 1:npreds){
    Yobs <- data$Y
    Ypred <- pred_Y[i, ]
    Yexp <- mod_expected_Y(model, data, m_mcmc[i, ])
    chi2_o[i] <- sum((Yobs - Yexp)^2 / Yexp)
    chi2_p[i] <- sum((Ypred - Yexp)^2 / Yexp)
  }
  p_omni <- sum(chi2_o < chi2_p) / npreds
  list(p_omni = p_omni, chi2_o = chi2_o, chi2_p = chi2_p)
}

FT <- function(pred_Y, data, m_mcmc, model){

  npreds <- nrow(pred_Y)
  FT_o <- rep(NA, npreds)
  FT_p <- rep(NA, npreds)

  for(i in 1:npreds){
    Yobs <- data$Y
    Ypred <- pred_Y[i, ]
    Yexp <- mod_expected_Y(model, data, m_mcmc[i, ])
    FT_o[i] <- sum((sqrt(Yobs) - sqrt(Yexp))^2)
    FT_p[i] <- sum((sqrt(Ypred) - sqrt(Yexp))^2)
  }
  p_omni <- sum(FT_o < FT_p) / npreds
  list(p_omni = p_omni, FT_o = FT_o, FT_p = FT_p)
}



mod_expected_Y <- function(model, data, m_mcmc_i){
  NS <- data$NS
  SO <- data$SO
  MSPY <- data$MSPY
  MSSPY <- data$MSSPY
  week <- data$week
  NT <- data$NT
  NDS <- data$NDS
  DSid <- data$DSid
  NS <- data$NS
  DM <- data$DM
  NYr <- data$NYr
  Yrid <- data$Yrid
  Sid <- data$Sid
  SSid <- data$SSid
  out <- NULL
  zeta <- 1
  rho <- 1
  if (model == 1){
    delta <- exp(m_mcmc_i)
  } else if (model == 2){
    DS_ldelta <- rnorm(NDS, m_mcmc_i[1], m_mcmc_i[3])
    ldelta <- DS_ldelta[DSid]
    delta <- exp(ldelta)
  } else if (model == 3){
    muDS_ldelta <- data$X %*% m_mcmc_i[1:13]
    DS_ldelta <- rnorm(NDS, muDS_ldelta, m_mcmc_i[15])
    ldelta <- DS_ldelta[DSid]
    delta <- exp(ldelta)
  } else if (model == 4){
    muDS_ldelta <- data$X %*% m_mcmc_i[1:13]
    DS_ldelta <- rnorm(NDS, muDS_ldelta, m_mcmc_i[15]) 
    ldelta <- DS_ldelta[DSid]
    delta <- exp(ldelta)
    lzeta <- -m_mcmc_i[16] * data$W
    zeta <- exp(lzeta)
  } else if (model == 5){
    muDS_ldelta <- data$X %*% m_mcmc_i[1:13]
    DS_ldelta <- rnorm(NDS, muDS_ldelta, m_mcmc_i[15]) 
    ldelta <- DS_ldelta[DSid]
    delta <- exp(ldelta)
    muT_lzeta <- m_mcmc_i[16] * data$W
    lzeta <- rnorm(NT, muT_lzeta, m_mcmc_i[18]) * data$W
    zeta <- exp(lzeta)
  } else if (model == 6){
    muDS_ldelta <- data$X %*% m_mcmc_i[1:13]
    SIGMA_leta <- matrix(NA, NS, NS)
    for(i in 1:NS){
      for(j in 1:NS){
        SIGMA_leta[i, j] <- (1 / m_mcmc_i[20]) * exp(-m_mcmc_i[19] * DM[i, j])
      } 
    }
    letaS <- matrix(NA, NS, NYr)
    for(i in 1:NYr){
      letaS[, i] <- rmvnorm(1, rep(0, NS), SIGMA_leta, method = "svd")
    }
    DS_leta <- rep(NA, NDS)
    for(i in 1:NDS){
      DS_leta[i] <- letaS[Sid[i], Yrid[i]]
    }    
    DS_ldelta <- rnorm(NDS, muDS_ldelta + DS_leta, m_mcmc_i[15]) 
    ldelta <- DS_ldelta[DSid]
    delta <- exp(ldelta)
    muT_lzeta <- m_mcmc_i[16] * data$W
    lzeta <- rnorm(NT, muT_lzeta, m_mcmc_i[18]) * data$W
    zeta <- exp(lzeta)
  }else if (model == 7){
    muDS_ldelta <- data$X %*% m_mcmc_i[1:13]
    SIGMA_lgamma <- array(NA, dim = c(MSPY, MSPY, NYr))    
    lgammaS <- matrix(NA, MSPY, NYr)
    for(i in 1:NYr){
      for(j in 1:MSPY){
        for(k in 1:MSPY){
          tdiff <- abs(week[i, k] - week[i, j])
          SIGMA_lgamma[j, k, i] <- (1/m_mcmc_i[20]) * m_mcmc_i[19] ^ tdiff
        }
      }
      lgammaS[, i] <- rmvnorm(1, rep(0, MSPY), SIGMA_lgamma[ , , i], 
                              method = "svd")
    }
    DS_lgamma <- rep(NA, NDS)
    for(i in 1:NDS){
      DS_lgamma[i] <- lgammaS[SO[i], Yrid[i]]
    }

    DS_ldelta <- rnorm(NDS, muDS_ldelta + DS_lgamma, m_mcmc_i[15]) 
    ldelta <- DS_ldelta[DSid]
    delta <- exp(ldelta)
    muT_lzeta <- m_mcmc_i[16] * data$W
    lzeta <- rnorm(NT, muT_lzeta, m_mcmc_i[18]) * data$W
    zeta <- exp(lzeta)
  } else if (model == 8){
    muDS_ldelta <- data$X %*% m_mcmc_i[1:13]
    SIGMA_lgamma <- array(NA, dim = c(MSPY, MSPY, NYr))    
    lgammaS <- matrix(NA, MSPY, NYr)
    for(i in 1:NYr){
      for(j in 1:MSPY){
        for(k in 1:MSPY){
          tdiff <- abs(week[i, k] - week[i, j])
          SIGMA_lgamma[j, k, i] <- (1/m_mcmc_i[23]) * m_mcmc_i[22] ^ tdiff
        }
      }
      lgammaS[, i] <- rmvnorm(1, rep(0, MSPY), SIGMA_lgamma[ , , i], 
                              method = "svd")
    }
    DS_lgamma <- rep(NA, NDS)
    for(i in 1:NDS){
      DS_lgamma[i] <- lgammaS[SO[i], Yrid[i]]
    }
    SIGMA_leta <- matrix(NA, NS, NS)
    for(i in 1:NS){
      for(j in 1:NS){
        SIGMA_leta[i, j] <- (1 / m_mcmc_i[20]) * exp(-m_mcmc_i[19] * DM[i, j])
      } 
    }
    letaS <- matrix(NA, NS, NYr)
    for(i in 1:NYr){
      letaS[, i] <- rmvnorm(1, rep(0, NS), SIGMA_leta, method = "svd")
    }
    DS_leta <- rep(NA, NDS)
    for(i in 1:NDS){
      DS_leta[i] <- letaS[Sid[i], Yrid[i]]
    }   
    DS_ldelta <- rnorm(NDS, muDS_ldelta + DS_lgamma + DS_leta, m_mcmc_i[15]) 
    ldelta <- DS_ldelta[DSid]
    delta <- exp(ldelta)
    muT_lzeta <- m_mcmc_i[16] * data$W
    lzeta <- rnorm(NT, muT_lzeta, m_mcmc_i[18]) * data$W
    zeta <- exp(lzeta)
  } else if (model == 9){
    muDS_ldelta <- data$X %*% m_mcmc_i[1:13]
    SIGMA_lkappa <- array(NA, dim = c(MSSPY, MSSPY, NYr))    
    lkappaS <- matrix(NA, MSSPY, NYr)

    for(i in 1:NYr){
      for(j in 1:MSPY){
        for(k in 1:NS){
          for(l in 1:MSPY){
            for(m in 1:NS){
              tdiff <- abs(week[i, l] - week[i, j])
              SIGMA_lkappa[(j - 1) * NS + k, (l - 1) * NS + m, i] <- 
                   (1 /m_mcmc_i[21]) * 
                   m_mcmc_i[20] ^ tdiff * 
                   exp(-m_mcmc_i[19] * DM[k, m])
            }
          } 
        }
      }
      lkappaS[, i] <- rmvnorm(1, rep(0, MSSPY), SIGMA_lkappa[ , , i], 
                              method = "svd")
    }

    DS_lkappa <- rep(NA, NDS)
    for(i in 1:NDS){
      DS_lkappa[i] <- lkappaS[SSid[i], Yrid[i]]
    } 
    DS_ldelta <- rnorm(NDS, muDS_ldelta + DS_lkappa, m_mcmc_i[15]) 
    ldelta <- DS_ldelta[DSid]
    delta <- exp(ldelta)
    muT_lzeta <- m_mcmc_i[16] * data$W
    lzeta <- rnorm(NT, muT_lzeta, m_mcmc_i[18]) * data$W
    zeta <- exp(lzeta)
  }


  out <- data$V * delta * zeta * rho
  return(out)
}


pps <- function(model, data, m_mcmc_i){
 dens <- mod_expected_Y(model, data, m_mcmc_i)
 dens[dens > 1e9] <- 1e9 #excessively bad values can cause overflow of rpois
 rpois(length(data$Y), dens)
}

