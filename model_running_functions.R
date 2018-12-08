

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
                  beta = rnorm(1, -0.122, 0.7))
           }
  }
  if (model == 2){
    out <- function(chain = chain){
             list(.RNG.name = sample(rngs, 1),
                  .RNG.seed = sample(1:1e+06, 1),
                  beta = rnorm(1, -0.122, 0.7),
                  tau_ldelta = rgamma(1, shape = 1.2, rate = 0.7))
           }
  }
  if (model == 3){
    out <- function(chain = chain){
             list(.RNG.name = sample(rngs, 1),
                  .RNG.seed = sample(1:1e+06, 1),
                  beta = c(rnorm(1, -0.144, 0.7), rnorm(data$NX - 1, 0, 0.2)),
                  tau_ldelta = rgamma(1, shape = 1.2, rate = 0.7))
           }
  }
  out
}


run_mod <- function(model, data, mcmc = c(8, 1000, 1000, 5000, 1)){

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
  }
  keeploc <- paste0("output/model_output/model", model)

  run.jags(model = modelfile, modules = "glm", monitor = monitor,
           inits = inits_fun(model), data = data, n.chains = n.chains,
           adapt = adapt, burnin = burnin, sample = sample, thin = thin,
           summarise = FALSE, plots = FALSE, method = "parallel",
           keep.jags.files = keeploc)
}

combine_mod_chains <- function(model, thinfreq = 1){

  mod_out_loc <- paste0("output/model_output/model", model)  
  mod_in <- results.jags(mod_out_loc, summarise = TRUE)
  mod_mcmc <- as.mcmc.list(mod_in)
  nchains <- length(mod_mcmc)
  niter <- nrow(mod_mcmc[[1]])
  niterthin <- floor(niter / thinfreq)
  nparam <- ncol(mod_mcmc[[1]])
  totiter <- nchains * niterthin
  out <- matrix(NA, totiter, nparam)
  thins <- seq(1, niter, by = thinfreq)

  for(i in 1:nchains){
    chain_rows <- ((i - 1) * niterthin + 1):(i * niterthin)
    out[chain_rows, ] <- mod_mcmc[[i]][thins, ]
  }
  out
}


eval_mod <- function(model, data, mm, DMY_inv = NULL, nsamps = 1000, 
                     thinfreq = 1, save_out = TRUE, nsamps_MI = 1000){

  m_mcmc <- combine_mod_chains(model, thinfreq)
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

  if (save_out){
    fname <- paste0("output/model_output/model", model, ".RData")
    save(mm, post, file = fname)
  }
  post
}


MI <- function(data, DMY_inv, m_mcmc, model, nsamps){

  niter <- nrow(m_mcmc)
  samps <- sample(1:niter, nsamps)
  MI <- rep(NA, nsamps)

  for(i in 1:nsamps){
    Yobs <- data$Y
    Yexp <- mod_expected_Y(model, data, m_mcmc[i, ])

    MI[i] <- Moran.I(data$Y - Yexp, DMY_inv)$observed
  }
  p_omni <- length(MI[MI<0]) / npreds
  list(p_omni = p_omni, chi2_o = chi2_o, chi2_p = chi2_p)
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
  NDS <- data$NDS
  DSid <- data$DSid
  out <- NULL
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
  }
  out <- data$V * delta
}


pps <- function(model, data, m_mcmc_i){
  rpois(length(data$Y), mod_expected_Y(model, data, m_mcmc_i))
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