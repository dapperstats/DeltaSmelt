
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