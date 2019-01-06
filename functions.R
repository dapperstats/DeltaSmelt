ppc_fig <- function(data, model){

  pp <- paste0("output/model_output/model", model, "_postpred.RData")  
  load(pp)
  predY_thin <- post$post_pred_Y
  predD_thin <- post$post_pred_D

  predictsY_thinned <- predY_thin
  Y <- data$Y
  V <- data$V
  NT <- data$NT

  Vmat <- matrix(rep(V, nrow(predY_thin)), ncol = NT, byrow = TRUE)
  predDbyY <- predictsY_thinned / Vmat 

  obsDbyY <- Y / V

  meanpredicts <- apply(predDbyY, 1, mean)
  varpredicts <- apply(predDbyY, 1, var)
  fr0predicts <- apply(predictsY_thinned, 1, fr0)
  mn0predicts <- apply(predictsY_thinned, 1, mn0)
 
  nrep <- length(meanpredicts)
  predictedfrmatch <- rep(NA, nrep)

  for(i in 1:nrep){  
    x <- predictsY_thinned[, i]
    y <- Y[i]
    predictedfrmatch[i] <- length(x[x==y])/length(x)
  }

  meanY <- mean(Y/V)
  varY <- var(Y/V)
  fr0Y <- fr0(Y)
  mn0Y <- mn0(Y)

  lmp <- log(meanpredicts)
  lmY <- log(meanY)
  lvp <- log(varpredicts)
  lvY <- log(varY)


  # plot

    tiff("output/Fig8.tif", height = 6, width = 6, units = "in",
        res = 600, compression = "lzw")

    par(mfrow = c(2, 2))
    par(mar = c(2, 4, 2, 1))

    # mean

      x1 <- seq(-3, 5, 0.25)
      x2 <- seq(-2.75, 5.25, 0.25)  
      ht <- rep(NA, length(x1))    
      for(i in 1:length(ht)){
        ht[i] <- length(lmp[lmp > x1[i] & lmp <= x2[i]])
      }
      ht <- ht/sum(ht)

      plot(1, 1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', ylab = '', 
           xlab = "", xlim = c(-3, 5.25), ylim = c(0, 0.15))
      for(i in 1:length(ht)){
        rect(x1[i], 0, x2[i], ht[i])
      }
      points(rep(lmY, 2), c(0, 0.145), type = 'l', lwd = 3)

      axis(2, las = 1, labels = F, tck = -0.02, at = seq(0, 0.2, 0.025))
      mtext(side = 2, las = 1, cex = 0.7, at = seq(0, 0.2, 0.05),
            c("0.00", "0.05", "0.1", "0.15", "0.2"), line = 0.5)
      mtext(side = 2, "Frequency", cex = 1., line = 2.5)
      xaxs <- c(0.1, 1.0, 10, 100)
      mtext(side = 1, at = log(xaxs), xaxs, line = -0.5, cex = 0.6)
      mtext(side = 1, "Mean fish per 1000 m", cex = 0.9, line = 1)
      mtext(side = 1, "3", cex = 0.7, line = 0.75, at = 4.275)
      text(x = -1.75, y = 0.14, adj = 0,
           round(length(meanpredicts[meanpredicts < meanY])/
                 length(meanpredicts), 3), 
           cex = 0.9)
      text(-5.75, 0.165, "a", xpd = T, cex = 1.5)

    # variance

      x1 <- seq(-1, 18, .75)
      x2 <- seq(-0.25, 18.75, .75)  
      ht <- rep(NA, length(x1))    
      for(i in 1:length(ht)){
        ht[i] <- length(lvp[lvp > x1[i] & lvp <= x2[i]])
      }
      ht <- ht/sum(ht)

      plot(1, 1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', ylab = '', 
           xlab = "", xlim = c(-0.5, 18.5), ylim = c(0, 0.15))
      for(i in 1:length(ht)){
        rect(x1[i], 0, x2[i], ht[i])
      }
      points(rep(lvY, 2), c(0, 0.145), type = 'l', lwd = 3)

      axis(2, las = 1, labels = F, tck = -0.02, at = seq(0, 0.2, 0.025))
      mtext(side = 2, las = 1, cex = 0.7, at = seq(0, 0.2, 0.05),
            c("0.00", "0.05", "0.1", "0.15", "0.2"), line = 0.5)
      mtext(side = 2, "Frequency", cex = 1., line = 2.5)
      xaxs <- 10^(0:7)
      xaxst <- c("1", "10", "100", "1e4", "1e5", "1e6", "1e7", "1e8")
      mtext(side = 1, at = log(xaxs), xaxst, line = -0.5, cex = 0.6)
      mtext(side = 1, "Variance fish per 1000 m", cex = 0.9, line = 1)
      mtext(side = 1, "3", cex = 0.7, line = 0.75, at = 17.25)
      text(x = 0.2, y = 0.14, adj = 0,
           round(length(varpredicts[varpredicts < varY])/
                  length(varpredicts), 2), 
           cex = 0.9)
      text(-7, 0.165, "b", xpd = T, cex = 1.5)

    # fraction 0s

      x1 <- seq(0.7, 0.9875, 0.0125)
      x2 <- seq(0.7125, 1, 0.0125)  
      ht <- rep(NA, length(x1))    
      for(i in 1:length(ht)){
        ht[i] <- length(fr0predicts[fr0predicts > x1[i] & 
                                    fr0predicts <= x2[i]])
      }
      ht <- ht/sum(ht)

      plot(1, 1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', ylab = '', 
           xlab = "", xlim = c(0.7, 1.0), ylim = c(0, 0.25))
      for(i in 1:length(ht)){
        rect(x1[i], 0, x2[i], ht[i])
      }
      points(rep(fr0Y, 2), c(0, 0.225), type = 'l', lwd = 3)

      axis(2, las = 1, labels = F, tck = -0.02, at = seq(0, 0.25, 0.025))
      mtext(side = 2, las = 1, cex = 0.7, at = seq(0, 0.25, 0.05),
            c("0.00", "0.05", "0.1", "0.15", "0.2", "0.25"), line = 0.5)
      mtext(side = 2, "Frequency", cex = 1., line = 2.5)
      xaxs <- seq(0.7, 1.0, 0.1)
      mtext(side = 1, at = xaxs, xaxs, line = -0.5, cex = 0.6)
      mtext(side = 1, "Fraction of tows with 0 fish", cex = 0.9, 
            line = 1)
      text(x = 0.78, y = 0.2125, adj = 0,
           round(length(fr0predicts[fr0predicts < fr0Y])/
                  length(fr0predicts), 3), 
           cex = 0.9)

      text(0.6, 0.275, "c", xpd = T, cex = 1.5)


    # median of non-0s

      x1 <- seq(1, 5, 1)
      ht <- rep(NA, length(x1))    
      for(i in 1:length(ht)){
        ht[i] <- length(ceiling(mn0predicts[mn0predicts == x1[i]]))
      }
      ht <- ht/sum(ht)

      plot(1, 1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', ylab = '', 
           xlab = "", xlim = c(0.5, 5.5), ylim = c(0, 0.9))
      for(i in 1:length(ht)){
        rect(x1[i]-.25, 0, x1[i]+.25, ht[i])
      }
      points(rep(mn0Y, 2), c(0, 0.9), type = 'l', lwd = 3)

      axis(2, las = 1, labels = F, tck = -0.02, at = seq(0, 0.9, 0.1))
      mtext(side = 2, las = 1, cex = 0.7, at = seq(0, 0.8, 0.2),
            c("0.0", "0.2", "0.4", "0.6", "0.8"), line = 0.5)
      mtext(side = 2, "Frequency", cex = 1., line = 2.5)
      xaxs <- 1:5
      mtext(side = 1, at = xaxs, xaxs, line = -0.5, cex = 0.6)
      mtext(side = 1, "Median of tows with > 0 fish", cex = 0.9, 
            line = 1.)
      text(x = 1.125, y = 0.875, adj = 0,
           round(length(mn0predicts[mn0predicts < mn0Y])/
                  length(mn0predicts), 2), 
           cex = 0.9)

      text(-0.9, 0.935, "d", xpd = T, cex = 1.5)

  dev.off()

}

date_hmap <- function(data, model){

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
  
  tname <- paste0("output/model_output/model", model, "table.csv")
  STe <- read.csv(tname)  

  pp <- paste0("output/model_output/model", model, "_postpred.RData")  
  load(pp)
  predictsY <- post$post_pred_Y

  # get tow level catches and back-scaled covariates

    Yt <- data$Y
    Vt <- data$V
    pYmt <- apply(predictsY, 2, mean)  

    slY <- rep(NA, nrow(X))
    slV <- rep(NA, nrow(X)) 
    slpY <- rep(NA, nrow(X))
    rslpY <- rep(NA, nrow(X))

    for(i in 1:nrow(X)){

      spots <- which(data$DSid == i)
      
      if(length(spots) > 0){
        slY[i] <- sum(Yt[spots])
        slV[i] <- sum(Vt[spots])
        slpY[i] <- sum(pYmt[spots])
        rslpY[i] <- round(sum(pYmt[spots]))
      }
    } 

    tl_station <- data$Sid[data$DSid]
    tl_foy <- X[data$DSid, 2] * FOYsd + FOYm           
    tl_yr <- X[data$DSid, 4] * YEARsd + YEARm  

    foyt <- X[,2] * FOYsd + FOYm  
    yrt <- X[,4] * YEARsd + YEARm  
    ect <- X[,6] * lECavsd + lECavm
    set <- X[,8] * lSEsd + lSEm
    calt <- X[,10] * lCALsd + lCALm
    velt <- X[,12] * VELsd + VELm  

    station <- data$Sid
    dfCS <- data$DM[,1]

    tl_dfCS <- dfCS[tl_station]
    sl_dfCS <- dfCS[station]
    tl_jday <- round(tl_foy * 365)
    sl_jday <- round(foyt * 365)



  # plot

    
    dcp <- colorRampPalette(c(rgb(0.8, 0.8, 0.8), rgb(0, 0, 0)), 
                            space = "rgb", interpolate = "spline")

    tiff("output/Fig6.tif", height = 6, width = 4, units = "in", res = 600, 
         compression = "lzw")

    par(mfrow = c(3, 2), mar = c(2, 3, 2, 1))


  # predicted density

    plot(tl_jday, tl_dfCS, ylim = c(0, 80), xlim = c(60, 225), type = 'n', 
         bty = 'L', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis(1, c(60, 91, 121, 152, 182, 213), tck = -0.03, labels = F)
    mtext(side = 1, c("3/1", "4/1", "5/1", "6/1", "7/1", "8/1"), 
          at = c(60, 91, 121, 152, 182, 213), cex = 0.5, line = 0.5)
    axis(2, seq(0, 80, 20), tck = -0.03, labels = F)
    mtext(side = 2, seq(0, 80, 20), at = seq(0, 80, 20), las = 1, cex = 0.5, 
          line = 0.5)
    mtext(side = 2, "Distance from Carquinez Strait (km)", cex = 0.5, 
          line = 1.75)
    mtext(side = 3, "Model-predicted density", line = 0.25, cex = 0.4)
    mtext(side = 3, "(fish per 1000 m  )", line = -0.5, cex = 0.4)
    mtext(side = 3, "3", line = -0.25, cex = 0.3, at = 170)
    text(15, 90, xpd = T, "a", cex = 1.25)

    m1 <- gam(rslpY ~ te(sl_dfCS, sl_jday) + te(yrt), 
              family = nb, weights = slV)

    xxdist <- seq(min(sl_dfCS), max(sl_dfCS), 1)
    xxjd <- seq(min(na.omit(sl_jday)), max(na.omit(sl_jday)), 1)
    prdist <- rep(xxdist, each = length(xxjd))
    prjd <- rep(xxjd, length(xxdist))
    pryos <- rep(15, length(prdist))
    newx <- list(sl_dfCS = prdist, sl_jday = prjd, yrt = pryos)
    prde <- predict(m1, newdata = newx, type = "response")
    prde <- prde / mean(na.omit(slV))

    prcoln <- (prde - min(prde)) / (max(prde) - min(prde)) * 98 + 1
    prcolv <- dcp(100)[round(prcoln)]

    for(i in 1:length(prcolv)){
      rect(prjd[i] - 0.5, prdist[i] - 0.5, prjd[i] + 0.5, prdist[i] + 0.5, 
           col = prcolv[i], border = NA)
    }

    lab <- round(seq(min(prde), max(prde), length.out = 5), 2)

    contour(xxjd, xxdist, 
            matrix(prcoln, nrow = length(xxjd), ncol = length(xxdist)),
            drawlabels = TRUE, nlevels = 5, add = TRUE, labels = lab, 
            labcex = 0.4, col = "white")

  # observed density

    plot(tl_jday, tl_dfCS, ylim = c(0, 80), xlim = c(60, 225), type = 'n', 
         bty = 'L', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis(1, c(60, 91, 121, 152, 182, 213), tck = -0.03, labels = F)
    mtext(side = 1, c("3/1", "4/1", "5/1", "6/1", "7/1", "8/1"), 
          at = c(60, 91, 121, 152, 182, 213), cex = 0.5, line = 0.5)
    axis(2, seq(0, 80, 20), tck = -0.03, labels = F)
    mtext(side = 2, seq(0, 80, 20), at = seq(0, 80, 20), las = 1, 
          cex = 0.5, line = 0.5)
    mtext(side = 2, "Distance from Carquinez Strait (km)",
          cex = 0.5, line = 1.75)
    mtext(side = 3, "Observed density", line = 0.25, cex = 0.4)
    mtext(side = 3, "(fish per 1000 m  )", line = -0.5, cex = 0.4)
    mtext(side = 3, "3", line = -0.25, cex = 0.3, at = 170)
    text(15, 90, xpd = T, "b", cex = 1.25)


    m1 <- gam(slY ~ te(sl_dfCS) + te(sl_jday) + te(yrt), 
              family = nb, weights = slV)

    xxdist <- seq(min(sl_dfCS), max(sl_dfCS), 1)
    xxjd <- seq(min(na.omit(sl_jday)), max(na.omit(sl_jday)), 1)
    prdist <- rep(xxdist, each = length(xxjd))
    prjd <- rep(xxjd, length(xxdist))
    pryos <- rep(15, length(prdist))
    newx <- list(sl_dfCS = prdist, sl_jday = prjd, yrt = pryos)
    prde <- predict(m1, newdata = newx, type = "response") 
    prde <- prde / mean(na.omit(slV))

    prcoln <- (prde - min(prde)) / (max(prde) - min(prde)) * 98 + 1
    prcolv <- dcp(100)[round(prcoln)]

    for(i in 1:length(prcolv)){
      rect(prjd[i] - 0.5, prdist[i] - 0.5, prjd[i] + 0.5, prdist[i] + 0.5, 
           col = prcolv[i], border = NA)
    }

    lab <- round(seq(min(prde), max(prde), length.out = 5), 2)

    contour(xxjd, xxdist, 
            matrix(prcoln, nrow = length(xxjd), ncol = length(xxdist)),
            drawlabels = TRUE, nlevels = 5, add = TRUE, labels = lab, 
            labcex = 0.4, col = "white")

  # ec

    plot(sl_jday, sl_dfCS, ylim = c(0, 80), xlim = c(60, 225), type = 'n', 
         bty = 'L', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')

    axis(1, c(60, 91, 121, 152, 182, 213), tck = -0.03, labels = F)
    mtext(side = 1, c("3/1", "4/1", "5/1", "6/1", "7/1", "8/1"), 
          at = c(60, 91, 121, 152, 182, 213), cex = 0.5, line = 0.5)
    axis(2, seq(0, 80, 20), tck = -0.03, labels = F)
    mtext(side = 2, seq(0, 80, 20), at = seq(0, 80, 20), las = 1, 
          cex = 0.5, line = 0.5)
    mtext(side = 2, "Distance from Carquinez Strait (km)",
          cex = 0.5, line = 1.75)
    mtext(side = 3, expression(paste("Electroconductivity (", mu, "S/cm)")), 
          line = -0.25, cex = 0.4)
    text(15, 90, xpd = T, "c", cex = 1.25)

    m1 <- gam(ect ~ te(sl_dfCS, sl_jday) + te(yrt), family = scat)

    xxdist <- seq(min(sl_dfCS), max(sl_dfCS), 1)
    xxjd <- seq(min(na.omit(sl_jday)), max(na.omit(sl_jday)), 1)
    prdist <- rep(xxdist, each = length(xxjd))
    prjd <- rep(xxjd, length(xxdist))
    pryos <- rep(15, length(prdist))
    newx <- list(sl_dfCS = prdist, sl_jday = prjd, yrt = pryos)
    prec <- predict(m1, newdata = newx)

    prcoln <- (prec - min(prec)) / (max(prec) - min(prec)) * 98 + 1
    prcolv <- dcp(100)[round(prcoln)]

    for(i in 1:length(prcolv)){
      rect(prjd[i] - 0.5, prdist[i] - 0.5, prjd[i] + 0.5, prdist[i] + 0.5, 
           col = prcolv[i], border = NA)
    }

    lab <- round(exp(seq(min(prec), max(prec), length.out = 5)))

    contour(xxjd, xxdist, 
            matrix(prcoln, nrow = length(xxjd), ncol = length(xxdist)),
            drawlabels = TRUE, nlevels = 5, add = TRUE, labels = lab, 
            labcex = 0.4, col = "white")

  # sec

    plot(sl_jday, sl_dfCS, ylim = c(0, 80), xlim = c(60, 225), type = 'n', 
         bty = 'L', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis(1, c(60, 91, 121, 152, 182, 213), tck = -0.03, labels = F)
    mtext(side = 1, c("3/1", "4/1", "5/1", "6/1", "7/1", "8/1"), 
          at = c(60, 91, 121, 152, 182, 213), cex = 0.5, line = 0.5)
    axis(2, seq(0, 80, 20), tck = -0.03, labels = F)
    mtext(side = 2, seq(0, 80, 20), at = seq(0, 80, 20), las = 1, 
          cex = 0.5, line = 0.5)
    mtext(side = 2, "Distance from Carquinez Strait (km)",
          cex = 0.5, line = 1.75)
    mtext(side = 3, "Secchi depth (cm)", 
          line = -0.25, cex = 0.4)
    text(15, 90, xpd = T, "d", cex = 1.25)

    m1 <- gam(set ~ te(sl_dfCS, sl_jday, yrt))

    xxdist <- seq(min(sl_dfCS), max(sl_dfCS), 1)
    xxjd <- seq(min(na.omit(sl_jday)), max(na.omit(sl_jday)), 1)
    prdist <- rep(xxdist, each = length(xxjd))
    prjd <- rep(xxjd, length(xxdist))
    pryos <- rep(15, length(prdist))
    newx <- list(sl_dfCS = prdist, sl_jday = prjd, yrt = pryos)
    prsec <- predict(m1, newdata = newx)

    prcoln <- (prsec - min(prsec)) / (max(prsec) - min(prsec)) * 98 + 1
    prcolv <- dcp(100)[round(prcoln)]

    for(i in 1:length(prcolv)){
      rect(prjd[i] - 0.5, prdist[i] - 0.5, prjd[i] + 0.5, prdist[i] + 0.5, 
           col = prcolv[i], border = NA)
    }

    lab <- round(exp(seq(min(prsec), max(prsec), length.out = 5)))

    contour(xxjd, xxdist, 
            matrix(prcoln, nrow = length(xxjd), ncol = length(xxdist)),
            drawlabels = TRUE, nlevels = 5, add = TRUE, labels = lab, 
            labcex = 0.4, col = "white")

  # cal

    plot(sl_jday, sl_dfCS, ylim = c(0, 80), xlim = c(60, 225), type = 'n', 
         bty = 'L', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis(1, c(60, 91, 121, 152, 182, 213), tck = -0.03, labels = F)
    mtext(side = 1, c("3/1", "4/1", "5/1", "6/1", "7/1", "8/1"), 
          at = c(60, 91, 121, 152, 182, 213), cex = 0.5, line = 0.5)
    axis(2, seq(0, 80, 20), tck = -0.03, labels = F)
    mtext(side = 2, seq(0, 80, 20), at = seq(0, 80, 20), las = 1, 
          cex = 0.5, line = 0.5)
    mtext(side = 2, "Distance from Carquinez Strait (km)",
          cex = 0.5, line = 1.75)
    mtext(side = 3, "Calanoid density (ind. per m  )", line = -0.25,
          cex = 0.4)
    mtext(side = 3, "3", line = -0.1, cex = 0.3, at = 190)
    text(15, 90, xpd = T, "e", cex = 1.25)

    m1 <- gam(calt ~ te(sl_dfCS, sl_jday, yrt))

    xxdist <- seq(min(sl_dfCS), max(sl_dfCS), 1)
    xxjd <- seq(min(na.omit(sl_jday)), max(na.omit(sl_jday)), 1)
    prdist <- rep(xxdist, each = length(xxjd))
    prjd <- rep(xxjd, length(xxdist))
    pryos <- rep(15, length(prdist))
    newx <- list(sl_dfCS = prdist, sl_jday = prjd, yrt = pryos)
    prcal <- predict(m1, newdata = newx)

    prcoln <- (prcal - min(prcal)) / (max(prcal) - min(prcal)) * 98 + 1
    prcolv <- dcp(100)[round(prcoln)]

    for(i in 1:length(prcolv)){
      rect(prjd[i] - 0.5, prdist[i] - 0.5, prjd[i] + 0.5, prdist[i] + 0.5, 
           col = prcolv[i], border = NA)
    }

    lab <- round(exp(seq(min(prcal), max(prcal), length.out = 5)))

    contour(xxjd, xxdist, 
            matrix(prcoln, nrow = length(xxjd), ncol = length(xxdist)),
            drawlabels = TRUE, nlevels = 5, add = TRUE, labels = lab, 
            labcex = 0.4, col = "white")

  # vel

    plot(sl_jday, sl_dfCS, ylim = c(0, 80), xlim = c(60, 225), type = 'n', 
         bty = 'L', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis(1, c(60, 91, 121, 152, 182, 213), tck = -0.03, labels = F)
    mtext(side = 1, c("3/1", "4/1", "5/1", "6/1", "7/1", "8/1"), 
          at = c(60, 91, 121, 152, 182, 213), cex = 0.5, line = 0.5)
    axis(2, seq(0, 80, 20), tck = -0.03, labels = F)
    mtext(side = 2, seq(0, 80, 20), at = seq(0, 80, 20), las = 1, 
          cex = 0.5, line = 0.5)
    mtext(side = 2, "Distance from Carquinez Strait (km)",
          cex = 0.5, line = 1.75)
    mtext(side = 3, "Weekly average velocity (m/s)", 
          line = -0.25, cex = 0.4)
    text(15, 90, xpd = T, "f", cex = 1.25)

    m1 <- gam(velt ~ te(sl_dfCS, sl_jday, yrt), family = scat)

    xxdist <- seq(min(sl_dfCS), max(sl_dfCS), 1)
    xxjd <- seq(min(na.omit(sl_jday)), max(na.omit(sl_jday)), 1)
    prdist <- rep(xxdist, each = length(xxjd))
    prjd <- rep(xxjd, length(xxdist))
    pryos <- rep(15, length(prdist))
    newx <- list(sl_dfCS = prdist, sl_jday = prjd, yrt = pryos)
    prvel <- predict(m1, newdata = newx)

    prcoln <- (prvel - min(prvel)) / (max(prvel) - min(prvel)) * 98 + 1
    prcolv <- dcp(100)[round(prcoln)]

    for(i in 1:length(prcolv)){
      rect(prjd[i] - 0.5, prdist[i] - 0.5, prjd[i] + 0.5, prdist[i] + 0.5, 
           col = prcolv[i], border = NA)
    }

    lab <- round(seq(min(prvel), max(prvel), length.out = 5), 1)

    contour(xxjd, xxdist, 
            matrix(prcoln, nrow = length(xxjd), ncol = length(xxdist)),
            drawlabels = TRUE, nlevels = 5, add = TRUE, labels = lab, 
            labcex = 0.4, col = "white")

  dev.off()

}



flow_hmap <- function(data, model){

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
  outflow <- data$outflow

  tname <- paste0("output/model_output/model", model, "table.csv")
  STe <- read.csv(tname)  

  pp <- paste0("output/model_output/model", model, "_postpred.RData")  
  load(pp)
  predictsY <- post$post_pred_Y

  # get tow level catches and back-scaled covariates

    Yt <- data$Y
    Vt <- data$V
    pYmt <- apply(predictsY, 2, mean)  

    slY <- rep(NA, nrow(X))
    slV <- rep(NA, nrow(X)) 
    slpY <- rep(NA, nrow(X))
    rslpY <- rep(NA, nrow(X))

    for(i in 1:nrow(X)){

      spots <- which(data$DSid == i)
      
      if(length(spots) > 0){
        slY[i] <- sum(Yt[spots])
        slV[i] <- sum(Vt[spots])
        slpY[i] <- sum(pYmt[spots])
        rslpY[i] <- round(sum(pYmt[spots]))
      }
    } 

    tl_station <- data$Sid[data$DSid]
    tl_foy <- X[data$DSid, 2] * FOYsd + FOYm           
    tl_yr <- X[data$DSid, 4] * YEARsd + YEARm  

    foyt <- X[,2] * FOYsd + FOYm  
    yrt <- X[,4] * YEARsd + YEARm  
    ect <- X[,6] * lECavsd + lECavm
    set <- X[,8] * lSEsd + lSEm
    calt <- X[,10] * lCALsd + lCALm
    velt <- X[,12] * VELsd + VELm  

  # get  distance from carquinez strait  outflow and jday

    station <- data$Sid
    dfCS <- data$DM[,1]

    tl_dfCS <- dfCS[tl_station]
    sl_dfCS <- dfCS[station]
    tl_jday <- round(tl_foy * 365)
    sl_jday <- round(foyt * 365)


    tl_calyr <- tl_yr + 1994
    tl_calnyd <- paste0(tl_calyr, "-01-01")
    tl_caldate <- as.Date(tl_jday, origin = as.Date(tl_calnyd))

    sl_calyr <- yrt + 1994
    sl_calnyd <- paste0(sl_calyr, "-01-01")
    sl_caldate <- as.Date(sl_jday, origin = as.Date(sl_calnyd))

    tl_outf <- rep(NA, length(tl_caldate))
    for(i in 1:length(tl_caldate)){
      if(!is.na(tl_caldate[i])){
        tl_outf[i] <- log10(outflow$Outflow[outflow$Date == tl_caldate[i]])
      }
    }
    sl_outf <- rep(NA, length(sl_caldate))
    for(i in 1:length(sl_caldate)){
      if(!is.na(sl_caldate[i])){
        sl_outf[i] <- log10(outflow$Outflow[outflow$Date == sl_caldate[i]])
      }
    }


  # plot
    
    dcp <- colorRampPalette(c(rgb(0.8, 0.8, 0.8), rgb(0, 0, 0)), 
                            space = "rgb", interpolate = "spline")

    tiff("output/Fig7.tif", height = 6, width = 4, units = "in", res = 600, 
         compression = "lzw")
 
    par(mfrow = c(3, 2), mar = c(2.5, 3, 1.5, 1))

    xtix <- log10(c(seq(10, 100, 10), seq(100, 900, 100), 
                    seq(1000, 9000, 1000)))

  # predicted density

    plot(tl_jday, tl_outf, ylim = c(0, 80), xlim = c(1.8, 3.91), 
         type = 'n', bty = 'L', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis(1, at = c(2, 3), tck = -0.03, labels = F)
    axis(1, at = xtix, tck = -0.01, labels = F)
    mtext(side = 1, c(100, 1000), at = 2:3, las = 1, cex = 0.5, line = 0.5)
    axis(2, seq(0, 80, 20), tck = -0.03, labels = F)
    mtext(side = 2, seq(0, 80, 20), at = seq(0, 80, 20), las = 1, 
          cex = 0.5, line = 0.5)
    mtext(side = 2, "Distance from Carquinez Strait (km)",
          cex = 0.5, line = 1.75)
    mtext(side = 1, "Delta outflow (m  / s)", cex = 0.5, line = 1.35)
    mtext(side = 1, "3", at = 3.24, cex = 0.4, line = 1.175)
    mtext(side = 3, "Model-predicted density", line = 0.25, cex = 0.4)
    mtext(side = 3, "(fish per 1000 m  )", line = -0.5, cex = 0.4)
    mtext(side = 3, "3", line = -0.25, cex = 0.3, at = 3.2)
    text(1.25, 90, xpd = T, "a", cex = 1.25)
  
    m1 <- gam(rslpY ~ te(sl_dfCS, sl_outf) + te(yrt) + ti(yrt, sl_dfCS), 
                family = nb, weights = slV)

    xxdist <- seq(min(sl_dfCS), max(sl_dfCS), 1)
    xxoutf <- seq(min(na.omit(sl_outf)), max(na.omit(sl_outf)), 0.01)
    prdist <- rep(xxdist, each = length(xxoutf))
    proutf <- rep(xxoutf, length(xxdist))
    pryos <- rep(15, length(prdist))
    newx <- list(sl_dfCS = prdist, sl_outf = proutf, yrt = pryos)
    prde <- predict(m1, newdata = newx, type = "response")
    prde <- prde / mean(na.omit(slV))

    prcoln <- (prde - min(prde)) / (max(prde) - min(prde)) * 98 + 1
    prcolv <- dcp(100)[round(prcoln)]

    for(i in 1:length(prcolv)){
      rect(proutf[i] - 0.005, prdist[i] - 0.5, 
           proutf[i] + 0.005, prdist[i] + 0.5, 
           col = prcolv[i], border = NA)
    }

    lab <- round(seq(min(prde), max(prde), length.out = 5), 2)

    contour(xxoutf, xxdist, 
            matrix(prcoln, nrow = length(xxoutf), ncol = length(xxdist)),
            drawlabels = TRUE, nlevels = 5, add = TRUE, labels = lab, 
            labcex = 0.4, col = "white")

  # observed density

    plot(tl_jday, tl_outf, ylim = c(0, 80), xlim = c(1.8, 3.91), 
         type = 'n', bty = 'L', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis(1, at = c(2, 3), tck = -0.03, labels = F)
    axis(1, at = xtix, tck = -0.01, labels = F)
    mtext(side = 1, c(100, 1000), at = 2:3, las = 1, cex = 0.5, line = 0.5)
    axis(2, seq(0, 80, 20), tck = -0.03, labels = F)
    mtext(side = 2, seq(0, 80, 20), at = seq(0, 80, 20), las = 1, 
          cex = 0.5, line = 0.5)
    mtext(side = 2, "Distance from Carquinez Strait (km)",
          cex = 0.5, line = 1.75)
    mtext(side = 1, "Delta outflow (m  / s)", cex = 0.5, line = 1.35)
    mtext(side = 1, "3", at = 3.24, cex = 0.4, line = 1.175)
    mtext(side = 3, "Observed density", line = 0.25, cex = 0.4)
    mtext(side = 3, "(fish per 1000 m  )", line = -0.5, cex = 0.4)
    mtext(side = 3, "3", line = -0.25, cex = 0.3, at = 3.2)
    text(1.25, 90, xpd = T, "b", cex = 1.25)
  
    m1 <- gam(slY ~ te(sl_dfCS, sl_outf), 
                family = nb, weights = slV)

    xxdist <- seq(min(sl_dfCS), max(sl_dfCS), 1)
    xxoutf <- seq(min(na.omit(sl_outf)), max(na.omit(sl_outf)), 0.01)
    prdist <- rep(xxdist, each = length(xxoutf))
    proutf <- rep(xxoutf, length(xxdist))
    pryos <- rep(15, length(prdist))
    newx <- list(sl_dfCS = prdist, sl_outf = proutf, yrt = pryos)
    prde <- predict(m1, newdata = newx, type = "response")
    prde <- prde / mean(na.omit(slV))

    prcoln <- (prde - min(prde)) / (max(prde) - min(prde)) * 98 + 1
    prcolv <- dcp(100)[round(prcoln)]

    for(i in 1:length(prcolv)){
      rect(proutf[i] - 0.005, prdist[i] - 0.5, 
           proutf[i] + 0.005, prdist[i] + 0.5, 
           col = prcolv[i], border = NA)
    }

    lab <- round(seq(min(prde), max(prde), length.out = 5), 2)

    contour(xxoutf, xxdist, 
            matrix(prcoln, nrow = length(xxoutf), ncol = length(xxdist)),
            drawlabels = TRUE, nlevels = 5, add = TRUE, labels = lab, 
            labcex = 0.4, col = "white")

  # ec

    plot(tl_jday, tl_outf, ylim = c(0, 80), xlim = c(1.8, 3.91), 
         type = 'n', bty = 'L', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis(1, at = c(2, 3), tck = -0.03, labels = F)
    axis(1, at = xtix, tck = -0.01, labels = F)
    mtext(side = 1, c(100, 1000), at = 2:3, las = 1, cex = 0.5, line = 0.5)
    axis(2, seq(0, 80, 20), tck = -0.03, labels = F)
    mtext(side = 2, seq(0, 80, 20), at = seq(0, 80, 20), las = 1, 
          cex = 0.5, line = 0.5)
    mtext(side = 2, "Distance from Carquinez Strait (km)",
          cex = 0.5, line = 1.75)
    mtext(side = 1, "Delta outflow (m  / s)", cex = 0.5, line = 1.35)
    mtext(side = 1, "3", at = 3.24, cex = 0.4, line = 1.175)
    mtext(side = 3, expression(paste("Electroconductivity (", mu, "S/cm)")), 
          line = -0.25, cex = 0.4)
    text(1.25, 90, xpd = T, "c", cex = 1.25)

    m1 <- gam(ect ~ te(sl_dfCS, sl_outf, yrt), family = scat)

    xxdist <- seq(min(sl_dfCS), max(sl_dfCS), 1)
    xxoutf <- seq(min(na.omit(sl_outf)), max(na.omit(sl_outf)), 0.01)
    prdist <- rep(xxdist, each = length(xxoutf))
    proutf <- rep(xxoutf, length(xxdist))
    pryos <- rep(15, length(prdist))
    newx <- list(sl_dfCS = prdist, sl_outf = proutf, yrt = pryos)
    prec <- predict(m1, newdata = newx, type = "response")

    prcoln <- (prec - min(prec)) / (max(prec) - min(prec)) * 98 + 1
    prcolv <- dcp(100)[round(prcoln)]

    for(i in 1:length(prcolv)){
      rect(proutf[i] - 0.005, prdist[i] - 0.5, 
           proutf[i] + 0.005, prdist[i] + 0.5, 
           col = prcolv[i], border = NA)
    }

    lab <- round(exp(seq(min(prec), max(prec), length.out = 5)))

    contour(xxoutf, xxdist, 
            matrix(prcoln, nrow = length(xxoutf), ncol = length(xxdist)),
            drawlabels = TRUE, nlevels = 5, add = TRUE, labels = lab, 
            labcex = 0.4, col = "white")

  # sec

    plot(tl_jday, tl_outf, ylim = c(0, 80), xlim = c(1.8, 3.91), 
         type = 'n', bty = 'L', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis(1, at = c(2, 3), tck = -0.03, labels = F)
    axis(1, at = xtix, tck = -0.01, labels = F)
    mtext(side = 1, c(100, 1000), at = 2:3, las = 1, cex = 0.5, line = 0.5)
    axis(2, seq(0, 80, 20), tck = -0.03, labels = F)
    mtext(side = 2, seq(0, 80, 20), at = seq(0, 80, 20), las = 1, 
          cex = 0.5, line = 0.5)
    mtext(side = 2, "Distance from Carquinez Strait (km)",
          cex = 0.5, line = 1.75)
    mtext(side = 1, "Delta outflow (m  / s)", cex = 0.5, line = 1.35)
    mtext(side = 1, "3", at = 3.24, cex = 0.4, line = 1.175)
    mtext(side = 3, "Secchi depth (cm)", line = -0.25, cex = 0.4)
    text(1.25, 90, xpd = T, "d", cex = 1.25)

    m1 <- gam(set ~ te(sl_dfCS, sl_outf) + te(yrt))

    xxdist <- seq(min(sl_dfCS), max(sl_dfCS), 1)
    xxoutf <- seq(min(na.omit(sl_outf)), max(na.omit(sl_outf)), 0.01)
    prdist <- rep(xxdist, each = length(xxoutf))
    proutf <- rep(xxoutf, length(xxdist))
    pryos <- rep(15, length(prdist))
    newx <- list(sl_dfCS = prdist, sl_outf = proutf, yrt = pryos)
    prsec <- predict(m1, newdata = newx, type = "response")

    prcoln <- (prsec - min(prsec)) / (max(prsec) - min(prsec)) * 98 + 1
    prcolv <- dcp(100)[round(prcoln)]

    for(i in 1:length(prcolv)){
      rect(proutf[i] - 0.005, prdist[i] - 0.5, 
           proutf[i] + 0.005, prdist[i] + 0.5, 
           col = prcolv[i], border = NA)
    }

    lab <- round(exp(seq(min(prsec), max(prsec), length.out = 5)))

    contour(xxoutf, xxdist, 
            matrix(prcoln, nrow = length(xxoutf), ncol = length(xxdist)),
            drawlabels = TRUE, nlevels = 5, add = TRUE, labels = lab, 
            labcex = 0.4, col = "white")

  # cal

    plot(tl_jday, tl_outf, ylim = c(0, 80), xlim = c(1.8, 3.91), 
         type = 'n', bty = 'L', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis(1, at = c(2, 3), tck = -0.03, labels = F)
    axis(1, at = xtix, tck = -0.01, labels = F)
    mtext(side = 1, c(100, 1000), at = 2:3, las = 1, cex = 0.5, line = 0.5)
    axis(2, seq(0, 80, 20), tck = -0.03, labels = F)
    mtext(side = 2, seq(0, 80, 20), at = seq(0, 80, 20), las = 1, 
          cex = 0.5, line = 0.5)
    mtext(side = 2, "Distance from Carquinez Strait (km)",
          cex = 0.5, line = 1.75)
    mtext(side = 1, "Delta outflow (m  / s)", cex = 0.5, line = 1.35)
    mtext(side = 1, "3", at = 3.24, cex = 0.4, line = 1.175)
    mtext(side = 3, "Calanoid density (ind. per m  )", line = -0.25, 
          cex = 0.4)
    mtext(side = 3, "3", line = -0.1, cex = 0.3, at = 3.47)
    text(1.25, 90, xpd = T, "e", cex = 1.25)

    m1 <- gam(calt ~ te(sl_dfCS, sl_outf) + te(yrt))

    xxdist <- seq(min(sl_dfCS), max(sl_dfCS), 1)
    xxoutf <- seq(min(na.omit(sl_outf)), max(na.omit(sl_outf)), 0.01)
    prdist <- rep(xxdist, each = length(xxoutf))
    proutf <- rep(xxoutf, length(xxdist))
    pryos <- rep(15, length(prdist))
    newx <- list(sl_dfCS = prdist, sl_outf = proutf, yrt = pryos)
    prcal <- predict(m1, newdata = newx, type = "response")

    prcoln <- (prcal - min(prcal)) / (max(prcal) - min(prcal)) * 98 + 1
    prcolv <- dcp(100)[round(prcoln)]

    for(i in 1:length(prcolv)){
      rect(proutf[i] - 0.005, prdist[i] - 0.5, 
           proutf[i] + 0.005, prdist[i] + 0.5, 
           col = prcolv[i], border = NA)
    }

    lab <- round(exp(seq(min(prcal), max(prcal), length.out = 5)))

    contour(xxoutf, xxdist, 
            matrix(prcoln, nrow = length(xxoutf), ncol = length(xxdist)),
            drawlabels = TRUE, nlevels = 5, add = TRUE, labels = lab, 
            labcex = 0.4, col = "white")

  # vel

    plot(tl_jday, tl_outf, ylim = c(0, 80), xlim = c(1.8, 3.91), 
         type = 'n', bty = 'L', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis(1, at = c(2, 3), tck = -0.03, labels = F)
    axis(1, at = xtix, tck = -0.01, labels = F)
    mtext(side = 1, c(100, 1000), at = 2:3, las = 1, cex = 0.5, line = 0.5)
    axis(2, seq(0, 80, 20), tck = -0.03, labels = F)
    mtext(side = 2, seq(0, 80, 20), at = seq(0, 80, 20), las = 1, 
          cex = 0.5, line = 0.5)
    mtext(side = 2, "Distance from Carquinez Strait (km)",
          cex = 0.5, line = 1.75)
    mtext(side = 1, "Delta outflow (m  / s)", cex = 0.5, line = 1.35)
    mtext(side = 1, "3", at = 3.24, cex = 0.4, line = 1.175)
    mtext(side = 3, "Weekly average velocity (m/s)", line = -0.25, cex = 0.4)
    text(1.25, 90, xpd = T, "f", cex = 1.25)
 
    m1 <- gam(velt ~ te(sl_dfCS, sl_outf) + te(yrt), family = scat)

    xxdist <- seq(min(sl_dfCS), max(sl_dfCS), 1)
    xxoutf <- seq(min(na.omit(sl_outf)), max(na.omit(sl_outf)), 0.01)
    prdist <- rep(xxdist, each = length(xxoutf))
    proutf <- rep(xxoutf, length(xxdist))
    pryos <- rep(15, length(prdist))
    newx <- list(sl_dfCS = prdist, sl_outf = proutf, yrt = pryos)
    prvel <- predict(m1, newdata = newx, type = "response")

    prcoln <- (prvel - min(prvel)) / (max(prvel) - min(prvel)) * 98 + 1
    prcolv <- dcp(100)[round(prcoln)]

    for(i in 1:length(prcolv)){
      rect(proutf[i] - 0.005, prdist[i] - 0.5, 
           proutf[i] + 0.005, prdist[i] + 0.5, 
           col = prcolv[i], border = NA)
    }

    lab <- round((seq(min(prvel), max(prvel), length.out = 5)), 1)

    contour(xxoutf, xxdist, 
            matrix(prcoln, nrow = length(xxoutf), ncol = length(xxdist)),
            drawlabels = TRUE, nlevels = 5, add = TRUE, labels = lab, 
            labcex = 0.4, col = "white")

  dev.off()
}


ac_fig <- function(model){

  tname <- paste0("output/model_output/model", model, "table.csv")
  STe <- read.csv(tname)

  tiff("output/Fig5.tif", height = 4, width = 6, units = "in", res = 600, 
       compression = "lzw")

  par(mar = c(3, 3, 1, 1))
  plot(1, 1, type = 'n', bty = 'n', xlab = '', ylab = '', 
       xaxt = 'n', yaxt = 'n', ylim = c(0,1), xlim = c(0,1))

  # spacetime

    par(fig = c(0, 2/3, 0, 1), new = T)


        tdif <- seq(0, 18, 0.1)
        sdif <- seq(0, 78, 0.1)
        stdif <- expand.grid(tdif, sdif)
        stacf <- exp(-STe[19, "median"] * stdif[,2]) * 
                 STe[20, "median"]^stdif[,1]
        stacfm <- matrix(NA, length(tdif), length(sdif))
        for(i in 1:length(sdif)){
           stacfm[, i] <-  exp(-STe[19, "median"] * sdif[i]) * 
                               STe[20, "median"]^tdif
        }

        lb <- round(stacf, 2) * -1 + 1
        plot(1, 1, type = "n", xaxt = 'n', yaxt = 'n', xlab = "", ylab = "",
             ylim = c(0, 78), xlim = c(0, 18), bty = "L")

        for(i in 1:nrow(stdif))
          rect(stdif[i,1], stdif[i,2], stdif[i,1] + 0.1, stdif[i,2] + 0.1,
               col = rgb(lb[i], lb[i], lb[i]), border = NA)

        contour(tdif, sdif, stacfm, levels = seq(0.00, 1, 0.1), 
                labcex = 0.5, add = TRUE, col = "white")

        axis(2, las = 1, labels = F, tck = -0.02)
        mtext(side = 2, cex = 0.75, seq(0, 80, 20), at = seq(0, 80, 20),
              las = 1, line = 0.5)
        axis(2, las = 1, labels = F, tck = -0.007, at = seq(0,80,10))
        mtext(side = 2, "In-water distance (km)", line = 1.75, cex = 1.1)
  

        axis(1, las = 1, labels = F, tck = -0.02)
        axis(1, las = 1, labels = F, tck = -0.007, at = seq(1, 20, 1))
        mtext(side = 1, cex = 0.75, seq(0, 15, 5), at = seq(0, 15, 5),
              las = 1, line = 0.5)
        mtext(side = 1, "Time lag (weeks)", line = 1.75, cex = 0.9)

        text(-3.5, 82, "a", cex = 1, xpd = T)

  par(mar = c(2, 2, 1, 1))

  # space
     l95 <- as.numeric(strsplit(
               as.character(STe[19, "ci"]), " - ")[[1]][1])
     u95 <- as.numeric(strsplit(
               as.character(STe[19, "ci"]), " - ")[[1]][2])

    
    par(fig = c(2/3, 1, 1/2, 1), new = T)

        sdif <- seq(0, 78, 0.001)
        sacf <- exp(-STe[19, "median"] * sdif)
        sacf1 <- exp(-l95 * sdif)
        sacf2 <- exp(-u95 * sdif)

    plot(sdif, sacf, type = 'l', ylim = c(0,1.05), lwd = 2, bty = "L", 
          xaxt = "n", yaxt = "n", ylab = "", xlab = "")

        points(sdif, sacf1, type = 'l', lwd = 1, lty = 3)
        points(sdif, sacf2, type = 'l', lwd = 1, lty = 3)

        axis(1, las = 1, labels = F, tck = -0.02)
        axis(1, las = 1, labels = F, tck = -0.007, at = seq(0,80,10))
        mtext(side = 1, at = seq(0, 80, 20), seq(0, 80, 20),
              las = 1, cex = 0.5, line = -0.25)
        mtext(side = 1, "In-water distance (km)", line = 0.5, cex = 0.6)
  
        axis(2, las = 1, labels = F, tck = -0.02, at = seq(0, 1, 0.5))
        axis(2, las = 1, labels = F, tck = -0.007, at = seq(0, 1, 0.1))
        mtext(side = 2, at = seq(0, 1, .5), c("0.0", "0.5", "1.0"),
              las = 1, cex = 0.5, line = 0.5)
        mtext(side = 2, "Spatial autocorrelation", line = 1.25, cex = 0.6)

      text(-22.5, 1.15, "b", cex = 1, xpd = T)

  # time 

     l95 <- as.numeric(strsplit(
             as.character(STe[20, "ci"]), " - ")[[1]][1])
     u95 <- as.numeric(strsplit(
             as.character(STe[20, "ci"]), " - ")[[1]][2])

        par(fig = c(2/3, 1, 0, 1/2), new = T)

        tdif <- seq(0, 18, 0.001)
        tacf <-  STe[20, "median"]^tdif
        tacf1 <- l95 ^tdif
        tacf2 <- u95^tdif
  
        plot(tdif, tacf, type = 'l', ylim = c(0, 1.05), lwd = 2, bty = "L", 
          xaxt = "n", yaxt = "n", ylab = "", xlab = "")

        points(tdif, tacf1, type = 'l', lwd = 1, lty = 3)
        points(tdif, tacf2, type = 'l', lwd = 1, lty = 3)


        axis(1, las = 1, labels = F, tck = -0.02)
        axis(1, las = 1, labels = F, tck = -0.007, at = seq(1, 20, 1))
        mtext(side = 1, at = seq(0, 15, 5), seq(0, 15, 5),
              las = 1, cex = 0.5, line = -0.25)
        mtext(side = 1, "Time lag (weeks)", line = 0.5, cex = 0.6)
 
        axis(2, las = 1, labels = F, tck = -0.02, at = seq(0, 1, 0.5))
        axis(2, las = 1, labels = F, tck = -0.007, at = seq(0, 1, 0.1))
        mtext(side = 2, at = seq(0, 1, .5), c("0.0", "0.5", "1.0"),
              las = 1, cex = 0.5, line = 0.5)
        mtext(side = 2, "Temporal autocorrelation", line = 1.25, cex = 0.6)

      text(-5.5, 1.15, "c", cex = 1, xpd = T)


  dev.off()
}


covariates_fig <- function(data, model){

  mcmc_fn <- paste0("output/model_output/model", model, "_mcmc.RData")
  mcmc_c <- combine_mod_chains(9)
  tname <- paste0("output/model_output/model", model, "table.csv")
  STe <- read.csv(tname)

  Xr <- data$X
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
  
  # covariate impacts

    # get survey level densities and back-scaled covariates

      Ysld <- rep(NA, nrow(Xr))
      Yfoy <- rep(NA, nrow(Xr))
      Yyr <- rep(NA, nrow(Xr))
      Ylec <- rep(NA, nrow(Xr))
      Ylsec <- rep(NA, nrow(Xr))
      Ylcal <- rep(NA, nrow(Xr))
      Yvel <- rep(NA, nrow(Xr))
      avIV <- rep(NA, nrow(Xr))

      foyt <- Xr[,2] * FOYsd + FOYm  
      yrt <- Xr[,4] * YEARsd + YEARm  
      ect <- Xr[,6] * lECavsd + lECavm  
      set <- Xr[,8] * lSEsd + lSEm  
      calt <- Xr[,10] * lCALsd + lCALm  
      velt <- Xr[,12] * VELsd + VELm  

      for(i in 1:nrow(Xr)){

        spots <- which(data$DSid == i)
          
        Yt <- data$Y[spots]
        Vt <- data$V[spots]
  
        if(length(spots) > 0){

          Ysld[i] <- mean(Yt/Vt)
  
          Yfoy[i] <- foyt[i]
          Yyr[i] <- yrt[i]
          Ylec[i] <- ect[i]
          Ylsec[i] <- set[i]
          Ylcal[i] <- calt[i]
          Yvel[i] <- velt[i]

        }
      }

    # median and mean values for all inputs as input

      medianinputs <- apply(Xr, 2, median, na.rm = T)
      meaninputs <- apply(Xr, 2, mean, na.rm = T)

    # median impacts

      medianimpacts <- as.numeric(STe[1:13, "median"])
      shiftedmedianimpacts <- medianimpacts
      shiftedmedianimpacts[1] <- log(mean(Ysld, na.rm = T))




  # PLOT

      tiff("output/Fig4.tif", height = 6, width = 4, units = "in",
          res = 600, compression = "lzw")

      par(mfrow = c(3, 2), mar = c(3, 3.5, 1, 1))

        logoffset <- 0.25
        Nexamps <- 100

      # seasonality

        set.seed(213)
        plot(Yfoy, log(jitter(Ysld, 4) + logoffset), cex = 0.75, lwd = 1, 
             col = rgb(0.075, 0.075, 0.075, 0.05), xlim = c(58, 220)/365,
             bty = "L", 
             ylim = c(-1.9, 5.5), xaxt = "n", yaxt = "n", 
             ylab = "", xlab = "")

        predictionFROYR <- seq(.18, .61, .001)
        PredictionFROYRtrans <- (predictionFROYR - FOYm) / FOYsd  
        predictiontable <- matrix(c(1 , rep(0, 12)), 
                                  nrow = length(PredictionFROYRtrans), 
                                  byrow = T, ncol = 13)
        predictiontable[ ,2] <- PredictionFROYRtrans
        predictiontable[ ,3] <- PredictionFROYRtrans^2

        for(i in 1:Nexamps){

          impacttouse <- shiftedmedianimpacts
          impacttouse[2] <- sample(mcmc_c[,2], 1) 
          impacttouse[3] <- sample(mcmc_c[,3], 1)
          predictedY <- log(exp(predictiontable %*% impacttouse) + logoffset)

          points(predictionFROYR, predictedY, type = 'l', lwd = 1, 
                 col = rgb(0.1, 0.6, 0.8, 0.1))
  
        }

        predictedY <- log(exp(predictiontable %*% shiftedmedianimpacts) + 
                          logoffset)
        points(predictionFROYR, predictedY, type = 'l', lwd = 2, 
               col = rgb(0, 0, 0, 1))

        ymain <- c(0, 1.0, 10, 100)
        ysec <- c(seq(0, 1, 0.1), seq(1, 10, 1), seq(10, 100, 10), 
                  seq(100, 1000, 100))
        axis(2, las = 1, cex.axis = 0.75, at = log(ymain + logoffset), 
             labels = F, tck = -0.03)
        mtext(side = 2, ymain, at = log(ymain + logoffset), cex = 0.5, 
              line = 0.5, las = 1)
        axis(2, las = 1, cex.axis = 0.75, at = log(ysec + logoffset), 
             labels = F, tck = -0.01)
        mtext(side = 2, expression(paste(Fish, " (per 1000 m"^3, ")")),
              line = 1.75, cex = 0.65) 
        axis(1, at = c(60, 91, 121, 152, 182, 213)/365, 
             labels = F, tck = -0.03)
        mtext(side = 1, at = c(60, 91, 121, 152, 182, 213)/365,
              c("3/1", "4/1", "5/1", "6/1", "7/1", "8/1"), las = 1, 
              cex= 0.5, line = 0.5)
        mtext(side = 1, "Time of year", line = 1.5, cex = 0.6)

       text(12/365, 6, "a", xpd = T, cex = 1) 



      # Year

        set.seed(765)

        plot(Yyr, log(jitter(Ysld, 4) + logoffset), cex = 0.75, lwd = 1, 
             col = rgb(0.075, 0.075, 0.075, 0.05), bty = "L", 
             ylim = c(-1.9, 5.5), xaxt = "n", yaxt = "n", 
             ylab = "", xlab = "", xlim = c(-0.5, 21.5))

        predictionYR <- seq(1, 20, 0.01)
        PredictionYRtrans <- (predictionYR - YEARm) / YEARsd  
        predictiontable <- matrix(c(1 , rep(0, 12)), 
                                  nrow = length(predictionYR), 
                                  byrow = T, ncol = 13)
        predictiontable[ ,4] <- PredictionYRtrans
        predictiontable[ ,5] <- PredictionYRtrans^2

        for(i in 1:Nexamps){

          impacttouse <- shiftedmedianimpacts
          impacttouse[4] <- sample(mcmc_c[,4], 1) 
          impacttouse[5] <- sample(mcmc_c[,5], 1)
          predictedY <- log(exp(predictiontable %*% impacttouse) + logoffset)

          points(predictionYR, predictedY, type = 'l', lwd = 1, 
                 col = rgb(0.1, 0.6, 0.8, 0.1))  
        }

        predictedY <- log(exp(predictiontable %*% shiftedmedianimpacts) + 
                          logoffset)
        points(predictionYR, predictedY, type = 'l', lwd = 2, 
               col = rgb(0, 0, 0, 1))

        ymain <- c(0, 1.0, 10, 100)
        ysec <- c(seq(0, 1, 0.1), seq(1, 10, 1), seq(10, 100, 10), 
                  seq(100, 1000, 100))
        axis(2, las = 1, cex.axis = 0.75, at = log(ymain + logoffset), 
             labels = F, tck = -0.03)
        mtext(side = 2, ymain, at = log(ymain + logoffset), cex = 0.5, 
              line = 0.5, las = 1)
        axis(2, las = 1, cex.axis = 0.75, at = log(ysec + logoffset), 
             labels = F, tck = -0.01)
        mtext(side = 2, expression(paste(Fish, " (per 1000 m"^3, ")")),
              line = 1.75, cex = 0.65) 
        xmain <- seq(1995, 2015, 5)
        axis(1, las = 1, at = xmain - 1994, labels = F, tck = -0.03)
        mtext(side = 1, cex = 0.5, at = xmain - 1994, xmain, line = 0.5)
        mtext(side = 1, "Year", line = 1.5, cex = 0.6)

       text(-6.8, 6, "b", xpd = T, cex = 1) 


      # EC

        set.seed(223)
        plot(Ylec, log(jitter(Ysld, 4) + logoffset), cex = 0.75, lwd = 1, 
             col = rgb(0.075, 0.075, 0.075, 0.05), bty = "L", 
             ylim = c(-1.9, 5.5),
             xaxt = "n", yaxt = "n", ylab = "", xlab = "")

        predictionlEC <- seq(3.2, 10.4, 0.001)
        PredictionlECtrans <- (predictionlEC - lECavm) / lECavsd  
        predictiontable <- matrix(c(1 , rep(0, 12)), 
                                  nrow = length(predictionlEC ), byrow = T, 
                                  ncol = 13)
        predictiontable[ ,6] <- PredictionlECtrans
        predictiontable[ ,7] <- PredictionlECtrans^2

        for(i in 1:Nexamps){

          impacttouse <- shiftedmedianimpacts
          impacttouse[6] <- sample(mcmc_c[,6], 1) 
          impacttouse[7] <- sample(mcmc_c[,7], 1)
          predictedY <- log(exp(predictiontable %*% impacttouse) + logoffset)

          points(predictionlEC, predictedY, type = 'l', lwd = 1, 
                 col = rgb(0.1, 0.6, 0.8, 0.1))
        }

        predictedY <- log(exp(predictiontable %*% shiftedmedianimpacts) + 
                          logoffset)
        points(predictionlEC, predictedY, type = 'l', lwd = 2, 
               col = rgb(0, 0, 0, 1))

        ymain <- c(0, 1.0, 10, 100)
        ysec <- c(seq(0, 1, 0.1), seq(1, 10, 1), seq(10, 100, 10), 
                  seq(100, 1000, 100))
        axis(2, las = 1, cex.axis = 0.75, at = log(ymain + logoffset), 
             labels = F, tck = -0.03)
        mtext(side = 2, ymain, at = log(ymain + logoffset), cex = 0.5, 
              line = 0.5, las = 1)
        axis(2, las = 1, cex.axis = 0.75, at = log(ysec + logoffset), 
             labels = F, tck = -0.01)
        mtext(side = 2, expression(paste(Fish, " (per 1000 m"^3, ")")),
              line = 1.75, cex = 0.65) 
        ECV <- c(100, 1000, 10000)
        ECV2 <- c(seq(10,100,10), seq(100, 1000, 100), seq(1000, 10000, 1000),
                  seq(10000, 100000, 10000))
        axis(1, las = 1, cex.axis = 1.25, at = log(ECV), labels = F, 
             tck = -0.03)
        axis(1, las = 1, labels = F, tck = -0.008, at = log(ECV2))
        mtext(side = 1 , ECV, at = log(ECV), cex = 0.5, line = 0.5)
        mtext(side = 1, expression(paste("Electroconductivity (",
                                   mu, "S/cm)")),
              line = 1.75, cex = 0.6)

       text(log(3.35), 6, "c", xpd = T, cex = 1) 

      # Secchi

        set.seed(465)
        plot(Ylsec, log(jitter(Ysld, 4) + logoffset), cex = 0.75, lwd = 1, 
             col = rgb(0.075, 0.075, 0.075, 0.05), bty = "L", 
             ylim = c(-1.9, 5.5),
             xaxt = "n", yaxt = "n", ylab = "", xlab = "")

        predictionlsecchi <- seq(1.5, 5.5, 0.001)
        predictionlsecchitrans <- (predictionlsecchi - lSEm) / lSEsd  
        predictiontable <- matrix(c(1 , rep(0, 12)), 
                                  nrow = length(predictionlsecchi ), 
                                  byrow = T, ncol = 13)
        predictiontable[ ,8] <- predictionlsecchitrans
        predictiontable[ ,9] <- predictionlsecchitrans^2

        for(i in 1:Nexamps){

          impacttouse <- shiftedmedianimpacts
          impacttouse[8] <- sample(mcmc_c[,8], 1) 
          impacttouse[9] <- sample(mcmc_c[,9], 1)
          predictedY <- log(exp(predictiontable %*% impacttouse) + logoffset)

          points(predictionlsecchi , predictedY, type = 'l', lwd = 1, 
                 col = rgb(0.1, 0.6, 0.8, 0.1))
        }

        predictedY <- log(exp(predictiontable %*% shiftedmedianimpacts) + 
                          logoffset)
        points(predictionlsecchi , predictedY, type = 'l', lwd = 2, 
               col = rgb(0, 0, 0, 1))
  
        ymain <- c(0, 1.0, 10, 100)
        ysec <- c(seq(0, 1, 0.1), seq(1, 10, 1), seq(10, 100, 10), 
                  seq(100, 1000, 100))
        axis(2, las = 1, cex.axis = 0.75, at = log(ymain + logoffset), 
             labels = F, tck = -0.03)
        mtext(side = 2, ymain, at = log(ymain + logoffset), cex = 0.5, 
              line = 0.5, las = 1)
        axis(2, las = 1, cex.axis = 0.75, at = log(ysec + logoffset), 
             labels = F, tck = -0.01)
        mtext(side = 2, expression(paste(Fish, " (per 1000 m"^3, ")")),
              line = 1.75, cex = 0.65) 
        SDV <- c(1, 10, 100)
        SDV2 <- c(seq(1,10,1), seq(10, 100, 10), seq(100, 1000, 100))
        axis(1, las = 1, cex.axis = 1.25, at = log(SDV), labels = F, 
             tck = -0.03)
        axis(1, las = 1, labels = F, tck = -0.008, at = log(SDV2))
        mtext(side = 1 , SDV[2:3], at = log(SDV[2:3]), cex = 0.5, line = 0.5)
        mtext(side = 1, "Secchi depth (cm)", line = 1.75, cex = 0.6)
  
       text(log(1.75), 6, "d", xpd = T, cex = 1) 


      # Cal

        set.seed(123)
        plot(Ylcal, log(jitter(Ysld, 4) + logoffset), cex = 0.75, lwd = 1, 
             col = rgb(0.075, 0.075, 0.075, 0.05), bty = "L", 
             ylim = c(-1.9, 5.5),
             xaxt = "n", yaxt = "n", ylab = "", xlab = "")

        predictionlcal <- seq(-2.4, 11.1, 0.01)
        Predictionlcaltrans <- (predictionlcal - lCALm) / lCALsd  
        predictiontable <- matrix(c(1 , rep(0, 12)), 
                                  nrow = length(predictionlcal ), 
                                  byrow = T, ncol = 13)
        predictiontable[ ,10] <- Predictionlcaltrans
        predictiontable[ ,11] <- Predictionlcaltrans^2
  
        for(i in 1:Nexamps){
  
          impacttouse <- shiftedmedianimpacts
          impacttouse[10] <- sample(mcmc_c[,10], 1) 
          impacttouse[11] <- sample(mcmc_c[,11], 1)
          predictedY <- log(exp(predictiontable %*% impacttouse) + logoffset)

          points(predictionlcal, predictedY, type = 'l', lwd = 1, 
                 col = rgb(0.1, 0.6, 0.8, 0.1))
        }

        predictedY <- log(exp(predictiontable %*% shiftedmedianimpacts) + 
                          logoffset)
        points(predictionlcal, predictedY, type = 'l', lwd = 2, 
               col = rgb(0, 0, 0, 1))

        ymain <- c(0, 1.0, 10, 100)
        ysec <- c(seq(0, 1, 0.1), seq(1, 10, 1), seq(10, 100, 10), 
                  seq(100, 1000, 100))
        axis(2, las = 1, cex.axis = 0.75, at = log(ymain + logoffset), 
             labels = F, tck = -0.03)
        mtext(side = 2, ymain, at = log(ymain + logoffset), cex = 0.5, 
              line = 0.5, las = 1)
        axis(2, las = 1, cex.axis = 0.75, at = log(ysec + logoffset), 
             labels = F, tck = -0.01)
        mtext(side = 2, expression(paste(Fish, " (per 1000 m"^3, ")")),
              line = 1.75, cex = 0.65) 
        CDV <- c(1, 10, 100, 1000, 10000)
        CDV2 <- c(seq(.1,1,.1), seq(1,10,1), seq(10,100,10), 
                  seq(100, 1000, 100), seq(1000, 10000, 1000), 
                  seq(10000, 100000, 10000))
        axis(1, las = 1, cex.axis = 1.25, at = log(CDV), labels = F, 
             tck = -0.03)
        axis(1, las = 1, cex.axis = 1.25, at = log(CDV2)[1], labels = F, 
             tck = -0.03)
        axis(1, las = 1, labels = F, tck = -0.008, at = log(CDV2))
        mtext(side = 1 , CDV, at = log(CDV), cex = 0.5, line = 0.5)
        mtext(side = 1, "0", at = log(CDV2)[1], cex = 0.5, line = 0.5)
        mtext(side = 1, 
              expression(paste("Calanoid density", " (ind. per m"^3, ")")), 
              line = 1.75, cex = 0.6)
  
       text(log(0.0025), 6, "e", xpd = T, cex = 1) 


      # Vel

        set.seed(111)
        plot(Yvel, log(jitter(Ysld, 4) + logoffset), cex = 0.75, lwd = 1, 
             col = rgb(0.075, 0.075, 0.075, 0.05), bty = "L", 
             ylim = c(-1.9, 5.5),
             xaxt = "n", yaxt = "n", ylab = "", xlab = "")

        predictionVel <- seq(-1, 4.6, 0.01)
        predictionVeltrans <- (predictionVel - VELm) / VELsd  
        predictiontable <- matrix(c(1 , rep(0, 12)), 
                                  nrow = length(predictionVel ), byrow = T, 
                                  ncol = 13)
        predictiontable[ ,12] <- predictionVeltrans
        predictiontable[ ,13] <- predictionVeltrans^2
  
        for(i in 1:Nexamps){
  
          impacttouse <- shiftedmedianimpacts
          impacttouse[12] <- sample(mcmc_c[,12], 1) 
          impacttouse[13] <- sample(mcmc_c[,13], 1)
          predictedY <- log(exp(predictiontable %*% impacttouse) + logoffset)

          points(predictionVel, predictedY, type = 'l', lwd = 1, 
                 col = rgb(0.1, 0.6, 0.8, 0.1))  
        }
  
        predictedY <- log(exp(predictiontable %*% shiftedmedianimpacts) + 
                          logoffset)
        points(predictionVel, predictedY, type = 'l', lwd = 2, 
               col = rgb(0, 0, 0, 1))
  
        ymain <- c(0, 1.0, 10, 100)
        ysec <- c(seq(0, 1, 0.1), seq(1, 10, 1), seq(10, 100, 10), 
                  seq(100, 1000, 100))
        axis(2, las = 1, cex.axis = 0.75, at = log(ymain + logoffset), 
             labels = F, tck = -0.03)
        mtext(side = 2, ymain, at = log(ymain + logoffset), cex = 0.5, 
              line = 0.5, las = 1)
        axis(2, las = 1, cex.axis = 0.75, at = log(ysec + logoffset), 
             labels = F, tck = -0.01)
        mtext(side = 2, expression(paste(Fish, " (per 1000 m"^3, ")")),
              line = 1.75, cex = 0.65) 

        VV <- seq(0, 1.2, 0.4) 
        VVx <- c("0.0", "0.4", "0.8", "1.2")
        VVf <- VV / 0.3048
        VV2 <- seq(-0.3, 1.4, 0.1)
        VV2f <- VV2 / 0.3048
        axis(1, las = 1, cex.axis = 1.25, at = VVf, labels = F, tck = -0.03)
        axis(1, las = 1, labels = F, tck = -0.008, at = (VV2f))
        mtext(side = 1, VVx, at = VVf, cex = 0.5, line = 0.5)
        mtext(side = 1, "Weekly average velocity (m/s)", line = 1.75, 
              cex = 0.6)
  
       text(-2.51, 6, "f", xpd = T, cex = 1) 

  dev.off()
}


##############################################################################
#
# priors_fig
#
# creates Figure 3
#
# Inputs: nothing
#
# Outputs: tiff of Fig 3 saved out
#

priors_fig <- function(){

      tiff("output/Fig3.tif", height = 4, width = 8, units = "in",
          res = 600, compression = "lzw")

      par(mfrow = c(2, 4), mar = c(3, 4, 1, 1))

      xx <- seq(0.0001, .01, .0001)
      yy <- dgamma(xx, shape = 0.001, rate = 0.001)
      plot(xx, yy, type='l', las = 1, lwd = 2, xlab = "", ylab = "", 
           cex.axis = 0.75, xaxt = 'n', yaxt = "n", bty = "L")
      mtext(side = 2, "Probability density", cex = 0.75, line = 2.5)
      mtext(side = 1, "Parameter value", cex = 0.75, line = 2)
      text(0.0095, 9.5, cex = 1.25, adj = 1,
           expression(paste(tau[zeta], ",", tau[epsilon])))
      axis(1, cex.axis = 0.75, at = seq(0, 0.010, 0.001), labels = FALSE, 
           tck = -0.01)
      axis(1, cex.axis = 0.75, at = seq(0, 0.010, 0.005), labels = FALSE, 
           tck = -0.02)
      mtext(side = 1, las = 1, seq(0, 0.010, 0.005), cex = 0.6, 
            at = seq(0, 0.01, 0.005), line = 0.5)
      axis(2, cex.axis = 0.75, at = seq(0, 10, 1), labels = FALSE, 
           tck = -0.01)
      axis(2, cex.axis = 0.75, at = seq(0, 10, 5), labels = FALSE, 
           tck = -0.025)
      mtext(side = 2, las = 1, seq(0, 10, 5), cex = 0.6, at = seq(0, 10, 5),
            line = 0.5)

      xx <- seq(0.0001, .01, .0001)
      yy <- dgamma(xx, shape = 0.01, rate = 0.01)
      plot(xx, yy, type='l', las = 1, lwd = 2, xlab = "", ylab = "", 
           cex.axis = 0.75, xaxt = 'n', yaxt = "n", bty = "L")
      mtext(side = 2, "Probability density", cex = 0.75, line = 2.5)
      mtext(side = 1, "Parameter value", cex = 0.75, line = 2)
      text(0.0095, 85, cex = 1.25, adj = 1,
           expression(tau[kappa]))
      axis(1, cex.axis = 0.75, at = seq(0, 0.010, 0.001), labels = FALSE, 
           tck = -0.01)
      axis(1, cex.axis = 0.75, at = seq(0, 0.010, 0.005), labels = FALSE, 
           tck = -0.02)
      mtext(side = 1, las = 1, seq(0, 0.010, 0.005), cex = 0.6, 
            at = seq(0, 0.01, 0.005), line = 0.5)
      axis(2, cex.axis = 0.75, at = seq(0, 80, 10), labels = FALSE, 
           tck = -0.01)
      axis(2, cex.axis = 0.75, at = seq(0, 80, 20), labels = FALSE, 
           tck = -0.025)
      mtext(side = 2, las = 1, seq(0, 80, 20), cex = 0.6, at = seq(0, 80, 20),
            line = 0.5)


      xx <- seq(-4, 4, 0.01)
      yy <- dnorm(xx, 0, 0.5)
      plot(xx, yy, type = 'l', las = 1, lwd = 2, xlab = "", ylab = "", 
           cex.axis = 0.75, ylim = c(0, 1), xaxt = 'n', yaxt = "n", 
           bty = "L")
      mtext(side = 2, "Probability density", cex = 0.75, line = 2.5)
      mtext(side = 1, "Parameter value", cex = 0.75, line = 2)
      text(3.95, 0.95, expression(chi), cex = 1.25, adj = 1)
      axis(1, cex.axis = 0.75, at = seq(-4, 4, 1), labels = FALSE, 
           tck = -0.015)
      axis(1, cex.axis = 0.75, at = seq(-4, 4, 2), labels = FALSE, 
           tck = -0.025)
      mtext(side = 1, las = 1, seq(-4, 4, 2), 
            cex = 0.6, at = seq(-4, 4, 2), line = 0.5)
      axis(2, cex.axis = 0.75, at = seq(0, 1, 0.1), labels = FALSE, 
           tck = -0.01)
      axis(2, cex.axis = 0.75, at = seq(0, 10, 0.5), labels = FALSE, 
           tck = -0.025)
      mtext(side = 2, las = 1, seq(0, 1.0, 0.5), cex = 0.6, 
            at = seq(0, 1.0, 0.5), line = 0.5)



      xx <- seq(0.001, 1, .001)
      yy <- dgamma(xx, shape = 1, rate = 10)
      plot(xx, yy, type = 'l', las = 1, lwd = 2, xlab = "", ylab = "", 
           cex.axis = 0.75, xaxt = 'n', yaxt = "n", bty = "L")
      mtext(side = 2, "Probability density", cex = 0.75, line = 2.5)
      mtext(side = 1, "Parameter value", cex = 0.75, line = 2)
      text(0.95, 9.5, expression(phi), cex = 1.25, adj = 1)
      axis(1, cex.axis = 0.75, at = seq(0, 1, 0.1), labels = FALSE, 
           tck = -0.015)
      axis(1, cex.axis = 0.75, at = seq(0, 1, 0.5), labels = FALSE, 
           tck = -0.025)
      mtext(side = 1, las = 1, c("0.0", "0.5", "1.0"), 
            cex = 0.6, at = seq(0, 1, 0.5), line = 0.5)
      axis(2, cex.axis = 0.75, at = seq(0, 10, 1), labels = FALSE, 
           tck = -0.01)
      axis(2, cex.axis = 0.75, at = seq(0, 10, 5), labels = FALSE, 
           tck = -0.025)
      mtext(side = 2, las = 1, seq(0, 10, 5), cex = 0.6, at = seq(0, 10, 5),
            line = 0.5)



      xx <- seq(-.5, 1.5, 0.001)
      yy <- dunif(xx, 0, 1)
      plot(xx, yy, type = 'l', las = 1, lwd = 2, xlab = "", ylab = "", 
           cex.axis = 0.75, ylim = c(0, 1.2), xaxt = 'n', yaxt = "n", 
           bty = "L")
      mtext(side = 2, "Probability density", cex = 0.75, line = 2.5)
      mtext(side = 1, "Parameter value", cex = 0.75, line = 2)
      text(1.4, 1.14, adj = 1, expression(lambda), cex = 1.25)
      axis(1, cex.axis = 0.75, at = seq(-0.5, 1.5, 0.5), labels = FALSE, 
           tck = -0.015)
      mtext(side = 1, las = 1, c("-0.5", "0.0", "0.5", "1.0", "1.5"), 
            cex = 0.6, at = seq(-0.5, 1.5, 0.5), line = 0.5)
      axis(2, cex.axis = 0.75, at = seq(0, 1.2, 0.1), labels = FALSE, 
           tck = -0.01)
      axis(2, cex.axis = 0.75, at = seq(0, 1.1, 0.5), labels = FALSE, 
           tck = -0.025)
      mtext(side = 2, las = 1, c("0.0", "0.5", "1.0"), cex = 0.6, 
            at = seq(0, 1, 0.5), line = 0.5)

      xx <- seq(-40, 40, 0.01)
      yy <- dnorm(xx, -0.122, 4)
      plot(xx, yy, type = 'l', las = 1, lwd = 2, xlab = "", ylab = "", 
           cex.axis = 0.75, ylim = c(0, 0.11), xaxt = 'n', yaxt = "n", 
           bty = "L")
      mtext(side = 2, "Probability density", cex = 0.75, line = 2.5)
      mtext(side = 1, "Parameter value", cex = 0.75, line = 2)
      text(36, 0.1045, adj = 1, expression(beta[1]), cex = 1.25)
      axis(1, cex.axis = 0.75, at = seq(-40, 40, 40), labels = FALSE, 
           tck = -0.03)
      axis(1, cex.axis = 0.75, at = seq(-40, 40, 10), labels = FALSE, 
           tck = -0.015)
      mtext(side = 1, las = 1, c("-40", "0", "40"), 
            cex = 0.6, at = seq(-40, 40, 40), line = 0.5)
      axis(2, cex.axis = 0.75, at = seq(0, 0.11, 0.01), labels = FALSE, 
           tck = -0.015)
      axis(2, cex.axis = 0.75, at = seq(0, 0.1, 0.05), labels = FALSE, 
           tck = -0.025)
      mtext(side = 2, las = 1, c("0.00", "0.05", "0.1"), cex = 0.6, 
            at = seq(0, 0.1, 0.05), line = 0.5)



      xx <- seq(-15, 15, 0.01)
      yy <- dnorm(xx, 0, 2.5)
      plot(xx, yy, type = 'l', las = 1, lwd = 2, xlab = "", ylab = "", 
           cex.axis = 0.75, ylim = c(0, 0.2), xaxt = 'n', yaxt = "n", 
           bty = "L")
      mtext(side = 2, "Probability density", cex = 0.75, line = 2.5)
      mtext(side = 1, "Parameter value", cex = 0.75, line = 2)
      text(13.5, 0.19, expression(paste(beta[2], ",...,", beta[13])), 
           adj = 1, cex = 1.25)
      axis(1, cex.axis = 0.75, at = seq(-10, 10, 10), labels = FALSE, 
           tck = -0.03)
      axis(1, cex.axis = 0.75, at = seq(-15, 15, 5), labels = FALSE, 
           tck = -0.015)
      mtext(side = 1, las = 1, c("-10", "0", "10"), 
            cex = 0.6, at = seq(-10, 10, 10), line = 0.5)
      axis(2, cex.axis = 0.75, at = seq(0, 0.2, 0.05), labels = FALSE, 
           tck = -0.015)
      axis(2, cex.axis = 0.75, at = seq(0, 0.2, 0.1), labels = FALSE, 
           tck = -0.025)
      mtext(side = 2, las = 1, c("0.00", "0.10", "0.20"), cex = 0.6, 
            at = seq(0, 0.2, 0.1), line = 0.5)


      xx <- seq(-10, 4.75, 0.01)
      yy <- dnorm(xx, -0.122, 4)
      exx <- exp(xx)
      plot(exx, yy, type = 'l', las = 1, lwd = 2, xlab = "", ylab = "", 
           cex.axis = 0.75, ylim = c(0, 0.11), xaxt = 'n', yaxt = "n", 
           bty = "L")
      mtext(side = 2, "Probability density", cex = 0.75, line = 2.5)
      mtext(side = 1, "Parameter value", cex = 0.75, line = 2)
      text(.9*exp(4.75), 0.1045, adj = 1, expression(bar(delta)), cex = 1.25)
      axis(1, cex.axis = 0.75, 
           at = c(seq(0, 10, 1), seq(10, 100, 10), seq(10, 1000, 100)), 
           labels = FALSE, tck = -0.01)
      axis(1, cex.axis = 0.75, at = c(0, 1e1, 1e2), labels = FALSE, 
           tck = -0.03)
      mtext(side = 1, las = 1, c(0, 10, 100), 
            cex = 0.6, at = c(0, 1e1, 1e2), line = 0.5)
      axis(2, cex.axis = 0.75, at = seq(0, 0.11, 0.01), labels = FALSE, 
           tck = -0.015)
      axis(2, cex.axis = 0.75, at = seq(0, 0.1, 0.05), labels = FALSE, 
           tck = -0.025)
      mtext(side = 2, las = 1, c("0.00", "0.05", "0.1"), cex = 0.6, 
            at = seq(0, 0.1, 0.05), line = 0.5)


    dev.off()

}



##############################################################################
#
# salvage_hists
#
# creates Figure 2
#
# Inputs: processed salvage data
#
# Outputs: tiff of Fig 2 saved out
#
salvage_time <- function(salvage_p){
  DAF <- salvage_p
  DAF <- DAF[DAF$year %in% 1995:2014, ]
  meanAF_SWP <- rep(NA, 366)
  meanAF_CVP <- rep(NA, 366)
  meantm3_SWP <- rep(NA, 366)
  meantm3_CVP <- rep(NA, 366)
  meanDS_SWP <- rep(NA, 366)
  meanDS_CVP <- rep(NA, 366)
  meanDens_SWP <- rep(NA, 366)
  meanDens_CVP <- rep(NA, 366)
  convf <- 1233.48 / 1e3
  for(i in 1:366){
    meanAF_SWP[i] <- mean(DAF[DAF[ , 3] == i, 4], na.rm = TRUE)
    meanAF_CVP[i] <- mean(DAF[DAF[ , 3] == i, 5], na.rm = TRUE)
    meantm3_SWP[i] <- mean(DAF[DAF[ , 3] == i, 4], na.rm = TRUE) * convf
    meantm3_CVP[i] <- mean(DAF[DAF[ , 3] == i, 5], na.rm = TRUE) * convf
    meanDS_SWP[i] <- mean(DAF[DAF[ , 3] == i, 6], na.rm = TRUE)
    meanDS_CVP[i] <- mean(DAF[DAF[ , 3] == i, 7], na.rm = TRUE)
    meanDens_SWP[i] <- 
       mean(DAF[DAF[ , 3] == i, 6]/(DAF[DAF[ , 3] == i, 4] * convf),
            na.rm = TRUE)
    meanDens_CVP[i] <- 
       mean(DAF[DAF[ , 3] == i, 7]/(DAF[DAF[ , 3] == i, 5] * convf),
            na.rm = TRUE)
  }

  tiff("output/Fig2.tif", height = 4, width = 6, units = "in",
       res = 600, compression = "lzw")

  par(fig = c(0.035, 0.535, 0.85, 1))
  par(mar = c(1.5, 1, 0.5, 1))
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", 
       ylab = "", xlim = c(0, 1), ylim = c(0, 1))
  text(x = 0.5, y = 1, cex = 0.75, font = 2, xpd = TRUE,
       "State Water Project")
  par(fig = c(0.5, 1, 0.85, 1), new = TRUE)
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", 
       ylab = "", xlim = c(0, 1), ylim = c(0, 1))
  text(x = 0.5, y = 1, cex = 0.75, font = 2, xpd = TRUE,
       "Central Valley Project")

  par(mar = c(1.25, 2, 1, 1))

  par(fig = c(0.035, 0.535, 0.64, 0.96), new = TRUE)
  plot(meantm3_SWP, xlim = c(0, 370), ylim = c(0, 16000),
       main = "", ylab = "", xlab = "", xaxt = "n", yaxt = "n", type = "l",
       bty = "L")
  rect(70, -1e5, 220, 16500, border = NA, 
       col = rgb(0.8, 0.8, 0.8))
  box("plot", bty = "L")
  abline(v = 70, lty = 2, col = rgb(0.2, 0.2, 0.2))
  abline(v = 220, lty = 2, col = rgb(0.2, 0.2, 0.2))
  points(meantm3_SWP, type = "l")
  axis(1, at = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365),
       labels = FALSE, tck = -0.05)
  axis(1, at = c(1, 91, 182, 274), labels = FALSE, tck = -0.075)
  mtext(side = 1, at = c(1, 91, 182, 274), c("1/1", "4/1", "7/1", "10/1"), 
       cex = 0.5)
  axis(2, at = seq(0, 15000, 5000), tck = -0.05, labels = FALSE)
  axis(2, at = seq(0, 16000, 1000), tck = -0.02, labels = FALSE)
  mtext(side = 2, at = seq(0, 15000, 5000),
        seq(0, 15000, 5000), las = 1, cex = 0.5, line = 0.3)
  mtext(side = 2, line = 2.25, cex = 0.6, "Water exports")
  mtext(side = 2, line = 1.5, cex = 0.6, 
        expression(paste("(1000 ", "m"^"3", ")")))
  text(-60, 19250, "a", xpd = TRUE)

  par(fig = c(0.5, 1, 0.64, 0.96), new = TRUE)
  plot(meantm3_CVP, xlim = c(0, 370), ylim = c(0, 16000),
       main = "", ylab = "", xlab = "", xaxt = "n", yaxt = "n", type = "l", 
       bty = "L")
  rect(70, -1e5, 220, 16500, border = NA, 
       col = rgb(0.8, 0.8, 0.8))
  box("plot", bty = "L")
  abline(v = 70, lty = 2, col = rgb(0.2, 0.2, 0.2))
  abline(v = 220, lty = 2, col = rgb(0.2, 0.2, 0.2))
  points(meantm3_CVP, type = "l")
  axis(1, at = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365),
       labels = FALSE, tck = -0.05)
  axis(1, at = c(1, 91, 182, 274), labels = FALSE, tck = -0.075)
  mtext(side = 1, at = c(1, 91, 182, 274), c("1/1", "4/1", "7/1", "10/1"), 
       cex = 0.5)
  axis(2, at = seq(0, 15000, 5000), tck = -0.05, labels = FALSE)
  axis(2, at = seq(0, 16000, 1000), tck = -0.02, labels = FALSE)
  mtext(side = 2, at = seq(0, 15000, 5000),
        seq(0, 15000, 5000), las = 1, cex = 0.5, line = 0.3)
  text(-60, 19250, "b", xpd = TRUE)

  par(fig = c(0.035, 0.535, 0.32, 0.64), new = TRUE)
  plot(meanDS_SWP, xlim = c(0, 370), ylim = c(0, 6),
       main = "", ylab = "", xlab = "", xaxt = "n", yaxt = "n", type = "l", 
       bty = "L")
  rect(70, -1e5, 220, 6.6, border = NA, 
       col = rgb(0.8, 0.8, 0.8))
  box("plot", bty = "L")
  abline(v = 70, lty = 2, col = rgb(0.2, 0.2, 0.2))
  abline(v = 220, lty = 2, col = rgb(0.2, 0.2, 0.2))
  points(meanDS_SWP, type = "l")
  axis(1, at = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365),
       labels = FALSE, tck = -0.05)
  axis(1, at = c(1, 91, 182, 274), labels = FALSE, tck = -0.075)
  mtext(side = 1, at = c(1, 91, 182, 274), c("1/1", "4/1", "7/1", "10/1"), 
       cex = 0.5)
  axis(2, at = seq(0, 6, 2), tck = -0.05, labels = FALSE)
  axis(2, at = seq(0, 6, 1), tck = -0.02, labels = FALSE)
  mtext(side = 2, at = seq(0, 6, 2),
        seq(0, 6, 2), las = 1, cex = 0.5, line = 0.3)
  mtext(side = 2, line = 2.2, cex = 0.6, "Delta smelt")
  mtext(side = 2, line = 1.6, cex = 0.6, "subsample")
  mtext(side = 2, line = 1.0, cex = 0.6, "counts")
  text(-60, 7, "c", xpd = TRUE)

  par(fig = c(0.5, 1, 0.32, 0.64), new = TRUE)
  plot(meanDS_CVP, xlim = c(0, 370), ylim = c(0, 6),
       main = "", ylab = "", xlab = "", xaxt = "n", yaxt = "n", type = "l", 
       bty = "L")
  rect(70, -1e5, 220, 6.6, border = NA, 
       col = rgb(0.8, 0.8, 0.8))
  box("plot", bty = "L")
  abline(v = 70, lty = 2, col = rgb(0.2, 0.2, 0.2))
  abline(v = 220, lty = 2, col = rgb(0.2, 0.2, 0.2))
  points(meanDS_CVP, type = "l")
  axis(1, at = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365),
       labels = FALSE, tck = -0.05)
  axis(1, at = c(1, 91, 182, 274), labels = FALSE, tck = -0.075)
  mtext(side = 1, at = c(1, 91, 182, 274), c("1/1", "4/1", "7/1", "10/1"), 
       cex = 0.5)
  axis(2, at = seq(0, 6, 2), tck = -0.05, labels = FALSE)
  axis(2, at = seq(0, 6, 1), tck = -0.02, labels = FALSE)
  mtext(side = 2, at = seq(0, 6, 2),
        seq(0, 6, 2), las = 1, cex = 0.5, line = 0.3)
  text(-60, 7, "d", xpd = TRUE)

  par(fig = c(0.035, 0.535, 0, 0.32), new = TRUE)
  plot(meanDens_SWP, xlim = c(0, 370), ylim = c(0, 0.003),
       main = "", ylab = "", xlab = "", xaxt = "n", yaxt = "n", type = "l", 
       bty = "L")
  rect(70, -1e3, 220, 0.004, border = NA, 
       col = rgb(0.8, 0.8, 0.8))
  box("plot", bty = "L")
  abline(v = 70, lty = 2, col = rgb(0.2, 0.2, 0.2))
  abline(v = 220, lty = 2, col = rgb(0.2, 0.2, 0.2))
  points(meanDens_SWP, type = "l")
  axis(1, at = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365),
       labels = FALSE, tck = -0.05)
  axis(1, at = c(1, 91, 182, 274), labels = FALSE, tck = -0.075)
  mtext(side = 1, at = c(1, 91, 182, 274), c("1/1", "4/1", "7/1", "10/1"), 
       cex = 0.5)
  axis(2, at = seq(0, 0.003, 0.001), tck = -0.05, labels = FALSE)
  axis(2, at = seq(0, 0.003, 0.0005), tck = -0.02, labels = FALSE)
  mtext(side = 2, at = seq(0, 0.003, 0.001),
        seq(0, 0.003, 0.001), las = 1, cex = 0.5, line = 0.3)
  mtext(side = 2, line = 2.25, cex = 0.6, "Delta smelt per")
  mtext(side = 2, line = 1.5, cex = 0.6, 
        expression(paste("1000 ", "m"^"3", "exported")))
  text(-60, 0.00365, "e", xpd = TRUE)

  par(fig = c(0.5, 1, 0, 0.32), new = TRUE)
  plot(meanDens_CVP, xlim = c(0, 370), ylim = c(0, 0.003),
       main = "", ylab = "", xlab = "", xaxt = "n", yaxt = "n", type = "l", 
       bty = "L")
  rect(70, -1e3, 220, 0.004, border = NA, 
       col = rgb(0.8, 0.8, 0.8))
  box("plot", bty = "L")
  abline(v = 70, lty = 2, col = rgb(0.2, 0.2, 0.2))
  abline(v = 220, lty = 2, col = rgb(0.2, 0.2, 0.2))
  points(meanDens_CVP, type = "l")
  axis(1, at = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365),
       labels = FALSE, tck = -0.05)
  axis(1, at = c(1, 91, 182, 274), labels = FALSE, tck = -0.075)
  mtext(side = 1, at = c(1, 91, 182, 274), c("1/1", "4/1", "7/1", "10/1"), 
       cex = 0.5)
  axis(2, at = seq(0, 0.003, 0.001), tck = -0.05, labels = FALSE)
  axis(2, at = seq(0, 0.003, 0.0005), tck = -0.02, labels = FALSE)
  mtext(side = 2, at = seq(0, 0.003, 0.001),
        seq(0, 0.003, 0.001), las = 1, cex = 0.5, line = 0.3)
  text(-60, 0.00365, "f", xpd = TRUE)

  dev.off()
}



##############################################################################
#
# delta_map
#
# creates Figure 1
#
# Inputs: processed spatial data
#
# Outputs: tiff of Fig 1 saved out
#
delta_map <- function(spatial_p){

      tiff("output/Fig1.tif", height = 6, width = 6, units = "in",
          res = 600, compression = "lzw")

      rp0 <- spatial_p$rp0
      par(mar=c(1, 1, 1, 0.5), pty = "s")
      plot(1, 1, xlim = c(570000, 650000), ylim = c(4180000, 4260000), 
           bty = 'n', xlab = "", ylab = "", xaxt = "n", yaxt = "n")

      xmi <- 547560.1 
      xma <- 652095.8 
      ymi <- 4181379 
      yma <- 4293880 

      xvs <- xmi + (xma-xmi)/rp0@ncols * (0:(rp0@ncols))
      yvs <- yma - (yma-ymi)/rp0@nrows * (0:(rp0@nrows))
      cc <- rp0@data@values
      ccm <- matrix(rp0@data@values, nrow = rp0@nrows, ncol = rp0@ncols, 
                    byrow = T)

      ccm[ccm == 1] <- rgb(0.00, 0.47, 0.70)
      ccm[ccm == 0] <- rgb(1, 1, 1)

      for(i in 1:rp0@nrows){
        for(j in 1:rp0@ncols){
          rect(xvs[j], yvs[i], xvs[j+1], yvs[i+1], col = ccm[i,j], 
               border = NA)
        }
      }

    points(spatial_p$stalocs, pch = 15, cex = 0.8, col = "white")
    points(spatial_p$stalocs, pch = 0, cex = 0.8, lwd = 1)

    text(574545, 4203000, "Carquinez Strait", cex = 0.5)
    arrows(574545, 4204000, 574545, 4207500, length = 0.05, angle = 30)
    text(577955, 4216000, "Suisun Bay", cex = 0.5, srt = 50)
    text(609045, 4218500, "Sacramento River", cex = 0.5, srt = 40)
    text(612545, 4206000, "San Joaquin River", cex = 0.5, srt = -7)
    text(570000, 4218600, "To San Pablo and", cex = 0.5, xpd = T)
    text(570000, 4216800, "San Francisco Bays", cex = 0.5, xpd = T)
    arrows(570000, 4215000, 565000, 4215000, xpd = TRUE,
           length = 0.05, angle = 30)

    arrows(623680, 4186500, 623680, 4182500, xpd = TRUE,
           length = 0.05, angle = 30)
    arrows(626750, 4185500, 626750, 4182500, xpd = TRUE,
           length = 0.05, angle = 30)
    text(625250, 4181000, "To California Aqueduct", cex = 0.5, xpd = TRUE)

    arrows(592000, 4204500, 593800, 4212300, length = 0.05, angle = 30)
    text(592000, 4203500, "Chipps Island", cex = 0.5, xpd = TRUE)


    # compass

      points(c(645000, 645000), c(4213500, 4218500), type = 'l', lwd = 2)
      points(c(642500, 647500), c(4216000, 4216000), type = 'l', lwd = 2)
      text(c(645000, 648750, 645000, 641250), 
           c(4220500, 4216000, 4212500, 4216000),
           c("N", "E", "S", "W"), xpd = TRUE, 
           cex = c(0.75, 0.5, 0.5, 0.5))

    # scale bar


      points(c(570000, 590000), c(4177500, 4177500), type = 'l', lwd = 2)
      points(c(570000, 570000), c(4176500, 4178500), type = 'l', lwd = 2)
      points(c(590000, 590000), c(4176500, 4178500), type = 'l', lwd = 2)
      points(c(580000, 580000), c(4176500, 4178500), type = 'l', lwd = 2)

      points(c(575000, 575000), c(4177000, 4178000), type = 'l', lwd = 2)
      points(c(585000, 585000), c(4177000, 4178000), type = 'l', lwd = 2)

      for(i in 1:20){
        x <- 570000 + 1000 * i
        points(c(x, x), c(4177250, 4177750), type = 'l', lwd = 1)
      }

      text(c(570000, 580000, 590000), rep(4175500, 3), 
           c("0 km", "10 km", "20 km"), 
           cex = 0.6, xpd = T)

    # inset


      gDist <- dist(spatial_p$stalocs)/1000
      wDist <- (spatial_p$waterDist)/1000

        par(fig = c(0.0, 0.4, 0.6, 1.0), 
            mar = c(3, 3.5, 1, 1), 
            new = TRUE)


        plot(gDist, wDist, xlim = c(0, 90), ylim = c(0, 90), las = 1, 
             xlab = "", ylab = "", type = 'n', 
             xaxt = 'n', yaxt = 'n', bty = "L")
        mtext(side = 1, line = 0.35, cex = 0.45, "Crow-flies distance (km)")
        mtext(side = 2, line = 0.75, cex = 0.5, "In-water distance (km)")
        axis(1, at = seq(0, 80, 20), labels = F, tck = -0.02)
        axis(1, at = seq(0, 90, 10), labels = F, tck = -0.01)
        mtext(side = 1, text = seq(0, 80, 20), at = seq(0, 80, 20), 
              line = -0.2, cex = 0.4)
        axis(2, at = seq(0, 80, 20), labels = F, tck = -0.02)
        axis(2, at = seq(0, 90, 10), labels = F, tck = -0.01)
        mtext(side = 2, text = seq(0, 80, 20), at = seq(0, 80, 20), 
              line = 0.25, cex = 0.4, las = 1)
        points(c(0, 90), c(0, 90), type = 'l', lwd = 2, lty = 3)
        points(gDist, wDist, cex = 0.5, lwd = 1, col = rgb(0, 0, 0, 0.1))


   # CA

        par(fig = c(0.0, 1, 0.0, 1), 
            mar = c(0, 0, 0, 0), 
            new = TRUE)


        plot(1, 1, xlim = c(0, 1), ylim = c(0, 1), las = 1, 
             xlab = "", ylab = "", type = 'n',   
             xaxt = 'n', yaxt = 'n', bty = "n")

    CA <- readPNG("data/CA.png")
    rasterImage(CA, 0.8, 0.8, 1.0, 1.0)
    rect(0.838, 0.91, 0.855, 0.925)

    dev.off()


}

##############################################################################
#
# prepare_model_data
#
# prepares the data for inputting into the models
#
# Inputs: the processed 20mm data 
#         the processed spatial data
#         the processed velocity data
#         the model number
#			
# Outputs: list of inputs formatted for the model of interest
#
# current components of the "all" list:
#
# Y: observed counts of delta smelt [16587]
# V: tow volume [16587]
# NT: number of tows [1]: 16587
# X: visit covariates [5571 x 13]
# NC: number of visit covariates [1]: 13
# V: tow volume [16587]
# DSid: Date Station id [16587]
# NDS: number of date station combinations [1]: 16587 
# TO: (within visit) tow order [16587]
# W: tow inst vel [16587]
# DM: between-station distance matrix [34 x 34]
# Sid: station ID of each date-station combination [5571]
# NS: number of stations [1]: 34
# Yrid: year ID of each date-station combination [5571]
# NYr: number of years [1]: 20
# MSPY: max number of surveys in a year [1]: 11
# week: among-year week index for potential visits within each year [20 x 11]
# SO: (within year) survey order [5571]
# MSSPY: max number of survey-station combos in a year [1]: 374
# SSid: within year survey-station identifier for a visit
# Z: tow covariates [16587 x 4]
# NCT: number of tow covariates [1]: 4
# msd: means and sds of pre-transformed variables
# outflow: outflow_p table

prepare_data <- function(mm20_p, spatial_p, vel_p, model, outflow_p = NULL){

# tow-level data

  stations_in <- mm20_p$tows20mm$Station %in% rownames(spatial_p$stalocs)
  dates_in <- as.Date(mm20_p$tows20mm$Date) < as.Date("2015-01-01")
  vol_in <- !is.na(mm20_p$tows20mm$V)
  tows_in <- which(stations_in & dates_in & vol_in)
  tows20mmR <- mm20_p$tows20mm[tows_in, ]   

# visit-level data

  stations_in <- mm20_p$visits20mm$Station %in% rownames(spatial_p$stalocs)
  dates_in <- as.Date(mm20_p$visits20mm$Date) < as.Date("2015-01-01")
  visits_in <- which(stations_in & dates_in)
  visits20mmR <- mm20_p$visits20mm[visits_in, ]

  # average EC

  ECs <- cbind(visits20mmR$TopEC, visits20mmR$BottomEC)
  visits20mmR$ECav <- apply(ECs, 1, mean, na.rm = T)

  # add velocity

  dt <- difftime(as.Date(visits20mmR$Date), as.Date("1995-01-01"))
  visits20mmR$Week <- floor(as.numeric(dt)/ 7 ) + 1
  visits20mmR$WeeklyAverageVelocity <- NA

  for(i in 1:nrow(visits20mmR)){
    week_match <- which(vel_p$warvt$week == visits20mmR$Week[i])
    station_match <- which(colnames(vel_p$warvt) == visits20mmR$Station[i])
    wav <- vel_p$warvt[week_match, station_match]
    visits20mmR$WeeklyAverageVelocity[i] <- wav
  }   

  # remove visits with any missing covariates

  cols <- c("ECav", "WeeklyAverageVelocity", "TotalCalanoidDENSITY", "Secchi")
  NAcovs <- which(apply(is.na(visits20mmR[ , cols]), 1, sum) > 0)
  visits20mmRNAomit <- visits20mmR[-NAcovs, ]

# remove tows with any missing covariates

  cols <- c("TimeFOD", "Bottom.Depth", "InstVel")
  NAcovs <- which(apply(is.na(tows20mmR[ , cols]), 1, sum) > 0)
  tows20mmRNAomit <- tows20mmR[-NAcovs, ]

# remove visits without associated tows

  visits_with_tows <- rep(NA, nrow(visits20mmRNAomit))
  for(i in 1:nrow(visits20mmRNAomit)){
    date_in <- tows20mmRNAomit$Date == visits20mmRNAomit$Date[i]
    station_in <- tows20mmRNAomit$Station == visits20mmRNAomit$Station[i]
    visits_with_tows[i] <- any(date_in & station_in)
  }
  visits20mmRNAomit <- visits20mmRNAomit[visits_with_tows, ]

# remove tows without associated visits

  tows_in_visits <- rep(NA, nrow(tows20mmRNAomit))
  for(i in 1:nrow(tows20mmRNAomit)){
    date_in <- visits20mmRNAomit$Date == tows20mmRNAomit$Date[i]
    station_in <- visits20mmRNAomit$Station == tows20mmRNAomit$Station[i]
    tows_in_visits[i] <- any(date_in & station_in)
  }
  tows20mmRNAomit <- tows20mmRNAomit[tows_in_visits,]

# prep visit covariate table

  # fraction of the year

  jd <- as.numeric(format(visits20mmRNAomit$Date, "%j"))
  yr <- as.numeric(format(visits20mmRNAomit$Date, "%Y"))
  nye <- as.Date(paste(yr, "-12-31", sep =""))
  jdnye <- as.numeric(format(nye, "%j"))
  foy <- jd / jdnye

  FOYm <- mean(foy)
  FOYsd <- sd(foy)
  FOY <- (foy - FOYm) / FOYsd  

  # year

  YEAR <- yr - 1994
  YEARm <- mean(YEAR)
  YEARsd <- sd(YEAR)
  YEARS <- (YEAR - YEARm) / YEARsd  

  # EC

  lECav <- log(visits20mmRNAomit$ECav)
  lECavm <- mean(lECav, na.rm = T)
  lECavsd <- sd(lECav, na.rm = T)
  lECavS <- (lECav - lECavm) / lECavsd

  # Secchi

  lSE <- log(visits20mmRNAomit$Secchi)
  lSEm <- mean(lSE, na.rm = T)
  lSEsd <- sd(lSE, na.rm = T)
  lSES <- (lSE - lSEm) / lSEsd     

  # Calanoid Density

  lCAL <- log(visits20mmRNAomit$TotalCalanoidDENSITY + 0.1)
  lCALm <- mean(lCAL, na.rm = T)
  lCALsd <- sd(lCAL, na.rm = T)
  lCALS <- (lCAL- lCALm) / lCALsd      

  # Average Velocity

  VEL <- ((visits20mmRNAomit$WeeklyAverageVelocity))
  VELm <- mean(VEL, na.rm = T)
  VELsd <- sd(VEL, na.rm = T)
  VELS <- (VEL - VELm) / VELsd 

  X <- data.frame(int = 1, sFOY = FOY, sFOY2 = FOY^2,
                  yr = YEARS, yr2 = (YEARS)^2,
                  ec = lECavS, ec2 = (lECavS)^2, 
                  sec = lSES, sec2 = (lSES)^2, 
                  cal = lCALS, cal2 = (lCALS)^2, 
                  vel = VELS, vel2 = (VELS)^2) 
  X <- as.matrix(X) 
  NX <- ncol(X)

# means and sds of variables

  msd <- c(FOYsd = FOYsd, FOYm = FOYm, YEARsd = YEARsd, YEARm = YEARm, 
           lECavsd = lECavsd, lECavm = lECavm, lSEsd = lSEsd, lSEm = lSEm,
           lCALsd = lCALsd, lCALm = lCALm, VELsd = VELsd, VELm = VELm)

# prep tow covariate table

  # instantaneous velocity

  ivel <- tows20mmRNAomit$InstVel
  ivelm <- mean(ivel, na.rm = T)
  ivelsd <- sd(ivel, na.rm = T)
  ivels <- (ivel - ivelm) / ivelsd

  # depth

  lbd <- log(tows20mmRNAomit$Bottom.Depth)
  lbdm <- mean(lbd, na.rm = T)
  lbdsd <- sd(lbd, na.rm = T)
  lbds <- (lbd - lbdm) / lbdsd


  tod <- tows20mmRNAomit$TimeFOD
  todm <- mean(tod, na.rm = T)
  todsd <- sd(tod, na.rm = T)
  tods <- (tod - todm) / todsd

  Z <- data.frame(int = 1, ivel = ivels, depth = lbds, tod = tods) 
  Z <- as.matrix(Z) 
  NZ <- ncol(Z)

# instantaneous velocity: made absolute and scaled to [0-1]

  W <- abs(tows20mmRNAomit$InstVel)
  W <- W / max(W)

# Observations
  
  Y <- tows20mmRNAomit$LCSmeltA
  V <- tows20mmRNAomit$V / 1000
  NT <- length(Y)


# associate visits and tows via DS (DateStation)

  DS <- paste0(tows20mmRNAomit$Date, "_", tows20mmRNAomit$Station)
  DSid <- as.numeric(as.factor(DS))
  UDS <- unique(DSid)
  NDS <- length(UDS)

# order the tows within visits

  dtimes <- paste(tows20mmRNAomit$Date, tows20mmRNAomit$Time, sep = " ")
  TO <- rep(NA, NDS)
  for(i in 1:NDS){
    matches <- which(DSid == UDS[i])
    TO[matches] <- order(dtimes[matches])
  }

# distance matrix

  DM <- as.matrix((spatial_p$waterDist)/1000)

# associate each visit with a station id to access the distance matrix

  S <- visits20mmRNAomit$Station
  Sid <- rep(NA, length(S))
  for (i in 1:length(S)){
    Sid[i] <- which(rownames(DM) == S[i])
  }
  NS <- length(unique(S))

# associate each visit with a year id to block VCV matrices

  Yr <- format(visits20mmRNAomit$Date, "%Y")
  Yrid <- as.numeric(as.factor(Yr))
  Yr <- as.numeric(Yr)
  UYr <- unique(Yrid)
  NYr <- length(UYr)

# survey number within the year and max number of surveys in a year
#   survey number isn't necessarily in order and they skip, so create own
#   WYSurvey based on the first week of each

  visits20mmRNAomit$WYSurvey <- NA

  for(i in 1:NYr){

    SNV <- visits20mmRNAomit$SurveyNumber[Yrid == UYr[i]]
    weekses <- visits20mmRNAomit$Week[Yrid == UYr[i]]
    SNV2 <- rep(NA, length(SNV))
    weekses2 <- rep(NA, length(weekses))
    for(j in 1:length(weekses2)){
      weekses2[j] <- min(weekses[SNV == SNV[j]])
    }
    uweekses2 <- unique(sort(weekses2))
    for(j in 1:length(weekses2)){
      SNV2[j] <- which(uweekses2 == weekses2[j])
    }
    visits20mmRNAomit$WYSurvey[Yrid == UYr[i]] <- SNV2
  }

  SO <- visits20mmRNAomit$WYSurvey
  MSPY <- max(visits20mmRNAomit$WYSurvey)

# week of each survey for each year
#   using the most common week for the surveys that span a week break
#   (only happened 15 of 176 times, tended to involve <= 15% of visits in the
#    survey and no surveys ever spanned three calendar weeks


  week <- matrix(NA, nrow = NYr, ncol = MSPY)
  for(i in 1:NYr){
    for(j in 1:MSPY){
      matched <- Yrid == UYr[i] & visits20mmRNAomit$WYSurvey == j
      wk <- visits20mmRNAomit$Week[matched]
      if (length(wk) == 0){
        week[i, j] <- week[i, j - 1] + 1
      } else {
        week[i, j] <- as.numeric(names(table(wk))[which.max(table(wk))])
      }
    }
  }

# station-survey index for each visit and max number of stations-survey per 
#   year

  posSS <- paste0(rep(1:NS, MSPY), "_", rep(1:MSPY, each = NS))
  SS <- paste0(Sid, "_", SO)
  SSid <- rep(NA, length(SS))
  for(i in 1:length(SS)){
    SSid[i] <- which(posSS == SS[i])
  }
  MSSPY <- MSPY * NS


# create model-specific outputs

  # model 1 is a single average density

  if (model == 1){
    out <- list(Y = Y, V = V, NT = NT)
  }

  # model2 draws a visit-specific density from the global and
  #  groups the data by DS (DateStation)

  if (model == 2){
    out <- list(Y = Y, V = V, NT = NT, DSid = DSid, NDS = NDS)
  }

  # model3 adds in the linear predictors 

  if (model == 3){
    out <- list(Y = Y, V = V, NT = NT, X = X, NX = NX, DSid = DSid, NDS = NDS)
  }

  # model4 adds in tow-level impact of instantaneous velocity with a 
  #  deterministic forced negative exponential

  if (model == 4){
    out <- list(Y = Y, V = V, NT = NT, X = X, NX = NX, DSid = DSid, NDS = NDS,
                W = W)
  }

  # model5 adds in tow-level impact of instantaneous velocity with a 
  #  exponential that is not forced negative and is noisy 

  if (model == 5){
    out <- list(Y = Y, V = V, NT = NT, X = X, NX = NX, DSid = DSid, NDS = NDS,
                W = W)
  }

  # model6 adds spatial autocorrelation to model 5

  if (model == 6){
    out <- list(Y = Y, V = V, NT = NT, X = X, NX = NX, DSid = DSid, NDS = NDS,
                W = W, DM = DM, Sid = Sid, NS = NS, Yrid = Yrid, NYr = NYr)
  }

  # model7 adds temporal autocorrelation to model 5

  if (model == 7){
    out <- list(Y = Y, V = V, NT = NT, X = X, NX = NX, DSid = DSid, NDS = NDS,
                W = W, week = week, MSPY = MSPY, SO = SO, 
                Yrid = Yrid, NYr = NYr)
  }

  # model8 adds spatial and temporal autocorrelation to model 5

  if (model == 8){
    out <- list(Y = Y, V = V, NT = NT, X = X, NX = NX, DSid = DSid, NDS = NDS,
                W = W, week = week, MSPY = MSPY, SO = SO, 
                DM = DM, Sid = Sid, NS = NS, Yrid = Yrid, NYr = NYr)
  }

  # model9 adds spatioemporal autocorrelation to model 5

  if (model == 9){
    out <- list(Y = Y, V = V, NT = NT, X = X, NX = NX, DSid = DSid, NDS = NDS,
                W = W, week = week, MSPY = MSPY, NS = NS, MSSPY = MSSPY,
                DM = DM, SSid = SSid, Yrid = Yrid, NYr = NYr)
  }

  # everything that could be used

  if (model == "all"){
    out <- list(Y = Y, V = V, NT = NT, X = X, NX = NX, DSid = DSid, NDS = NDS,
                DM = DM, Sid = Sid, NS = NS, Yrid = Yrid, NYr = NYr,
                week = week, MSPY = MSPY, SO = SO, TO = TO, W = W,
                MSSPY = MSSPY, SSid = SSid, Z = Z, NZ = NZ, msd = msd,
                outflow = outflow_p)
  }

  # just the distance matrix among Y samples

  if (model == "dmy"){

    DMY <- matrix(NA, NT, NT)
    for(i in 1:NT){
      for(j in 1:NT){
        DMY[i,j] <- DM[Sid[DSid[i]], Sid[DSid[j]]]
      }
    }
    DMY_inv <- 1 / DMY
    DMY_inv[which(DMY_inv == Inf)] <- 0
    out <- DMY_inv
  }

  out
}



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