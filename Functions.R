
##############################################################################
#
# prepare_model_input
#
# prepares the data for inputting into the pomp models
#
# Inputs: the processed 20mm data 
#         the processed spatial data
#         the processed velocity data
#			
# Outputs: list of inputs formatted for the model
#
#


prepare_model_input <- function(mm20_p, spatial_p, vel_p){

    # restrict to the stations and years of interest

      visits20mmR <- mm20_p$visits20mm[which(
                                        mm20_p$visits20mm$Station %in% 
                                            rownames(spatial_p$stalocs) & 
                                        as.Date(mm20_p$visits20mm$Date) < 
                                            as.Date("2015-01-01")),]

      tows20mmR <- mm20_p$tows20mm[which(
                                    mm20_p$tows20mm$Station %in% 
                                         rownames(spatial_p$stalocs) & 
                                    as.Date(mm20_p$tows20mm$Date) < 
                                         as.Date("2015-01-01")),]    

    # average EC

      visits20mmR$ECav <- apply(
                             cbind(visits20mmR$TopEC, visits20mmR$BottomEC), 
                             1, mean, na.rm = T)

    # add the velocity data on to the visits table 

      visits20mmR$Week <- floor(as.numeric(
                                difftime(as.Date(visits20mmR$Date),
                                as.Date("1995-01-01", format= "%Y-%m-%d"))) / 
                                7 ) + 1
      visits20mmR$WeeklyAverageVelocity <- NA

      for(i in 1:nrow(visits20mmR)){

        visits20mmR$WeeklyAverageVelocity[i] <- vel_p$warvt[
                      which(vel_p$warvt$week == visits20mmR$Week[i]),  
                      which(colnames(vel_p$warvt) == visits20mmR$Station[i])]
      }      

    #
    # Removing data points with any missing covariate values
    #  (EC, Secchi, Calanoids, Velocity)
    #

      NAcovs <- which(
                 apply(is.na(visits20mmR[, c(25, 12, 21, 27)]), 1, sum) > 0)

      visits20mmRNAomit <- visits20mmR[-NAcovs,]


    # determine the survey across all surveys and the survey across the 
    #   surveys within the year

      YR <- format(visits20mmRNAomit$Date, "%Y")
      visits20mmRNAomit$AYSurvey <- NA
      visits20mmRNAomit$WYSurvey <- NA
      adder <- 0

      for(i in 1:length(unique(YR))){

        # need to account for potential skipped survey numbers, etc

          SNV <- visits20mmRNAomit$SurveyNumber[YR == unique(YR)[i]]
          uSNV <- unique(SNV)
          ouSNV <- order(uSNV)
          
          for(j in 1:length(uSNV)){
            visits20mmRNAomit$AYSurvey[YR == unique(YR)[i] & 
               visits20mmRNAomit$SurveyNumber == uSNV[j]] <- ouSNV[j] + adder
            visits20mmRNAomit$WYSurvey[YR == unique(YR)[i] & 
               visits20mmRNAomit$SurveyNumber == uSNV[j]] <- ouSNV[j]
          }
        adder <- adder + max(ouSNV)
      }

    # sort visit data based on station within survey

      visits20mmRNAomit <- visits20mmRNAomit[
                                       order(visits20mmRNAomit$AYSurvey, 
                                             visits20mmRNAomit$Station),]

    # survey level covariates ("X")
    #  as a table that aligns with the 5898 station-survey combos as sorted 
    #  columns: intercept, cos of time, sin of time, year, year2, ec, ec2, 
    #  s, s2, c, c2, v, v2
    #

      # calculate the fraction of the year
    
        jd <- as.numeric(format(visits20mmRNAomit$Date, "%j"))
        jdT <- as.numeric(format(as.Date(paste(
                   format(visits20mmRNAomit$Date, "%Y"), "-12-31", sep ="")),
                 "%j"))
        foy <- jd / jdT

      # standardize the fraction of the year

        FOYm <- mean(foy, na.rm = T)
        FOYsd <- sd(foy, na.rm = T)
        FOY <- (foy - FOYm) / FOYsd  

      # year

        YEAR <- as.numeric(format(visits20mmRNAomit$Date, "%Y")) - 1994

        # standardize

          YEARm <- mean(YEAR, na.rm = T)
          YEARsd <- sd(YEAR, na.rm = T)
          YEARS <- (YEAR - YEARm) / YEARsd  

      # EC

        # log transform

          lECav <- log(visits20mmRNAomit$ECav)

        # standardize

          lECavm <- mean(lECav, na.rm = T)
          lECavsd <- sd(lECav, na.rm = T)
          lECavS <- (lECav - lECavm) / lECavsd

      # Secchi

        # log transform

          lSE <- log(visits20mmRNAomit$Secchi)

        # standardize

          lSEm <- mean(lSE, na.rm = T)
          lSEsd <- sd(lSE, na.rm = T)
          lSES <- (lSE - lSEm) / lSEsd      

      # Calanoid Density

        # log transform

          lCAL <- log(visits20mmRNAomit$TotalCalanoidDENSITY + 0.1)

        # standardize

          lCALm <- mean(lCAL, na.rm = T)
          lCALsd <- sd(lCAL, na.rm = T)
          lCALS <- (lCAL- lCALm) / lCALsd      

      # Average Velocity

        # pull out for use (no transformation happening) 

          VEL <- ((visits20mmRNAomit$WeeklyAverageVelocity))

        # standardize

          VELm <- mean(VEL, na.rm = T)
          VELsd <- sd(VEL, na.rm = T)
          VELS <- (VEL - VELm) / VELsd      

      # put together into a data frame

        Xtab <- data.frame(int = 1, foy = foy, foy2 = foy^2, 
                           sFOY = FOY, sFOY2 = FOY^2,
                           costime = round(cos(2 * pi * foy),5), 
                           sintime = round(sin(2 * pi * foy),5),
                           yr = YEARS, yr2 = (YEARS)^2,
                           ec = lECavS, ec2 = (lECavS)^2, 
                           sec = lSES, sec2 = (lSES)^2, 
                           cal = lCALS, cal2 = (lCALS)^2, 
                           vel = VELS, vel2 = (VELS)^2)  

      # expand to include all possible station surveys across 34 x 10 x 20

        X <- data.frame(matrix(NA, nrow= 34 * 10 * 20, ncol = ncol(Xtab)))
        colnames(X) <- c("int", "foy", "foy2", "sFOY", "sFOY2",  
                         "costime", "sintime",
                         "yr", "yr2", "ec", "ec2", "sec", "sec2",
                         "cal", "cal2", "vel", "vel2")
        iyear <- rep(1995:2014, each = 10 * 34)
        isurvey <- rep(rep(1:10, each = 34), 20)
        istation <- rep(unique(visits20mmRNAomit$Station), 10 * 20)

        spot2 <- 1 

        for(i in 1:20){
          for(j in 1:10){
            for(k in 1:34){
              spot1 <- which(visits20mmRNAomit$Year == iyear[spot2] & 
                             visits20mmRNAomit$WYSurvey == isurvey[spot2] &  
                             visits20mmRNAomit$Station == istation[spot2])
              if(length(spot1)>0){
                X[spot2,1:17] <- (Xtab[spot1,1:17])
              }
              spot2 <- spot2 + 1
            }
          }
        }

        FILLINS <- which(apply(is.na(X), 1, sum) > 0)
        for(i in 1:length(FILLINS))
          X[FILLINS[i],] <- c(1, rep(0, 16))

    # tow-specific data

      # trimming down the tow data to just tows associated with remaining 
      #  visits

        stdate_tows <- paste(tows20mmR$Station, tows20mmR$Date, sep = " ")
        stdate_visits <- paste(visits20mmRNAomit$Station, 
                               visits20mmRNAomit$Date, sep = " ")
        tows20mmT <- tows20mmR[which(stdate_tows %in% stdate_visits), ]

      # add year

        tows20mmT$Year <- format(tows20mmT$Date, "%Y")

      # add the survey number to the tow data

        tows20mmT$Survey <- NA      
        for(i in 1:nrow(tows20mmT)){
          tows20mmT$Survey[i] <- visits20mmRNAomit$Survey[which(
                          visits20mmRNAomit$Station == tows20mmT$Station[i] & 
                          visits20mmRNAomit$Date == tows20mmT$Date[i])]
        }

      # sort the tow data by survey then station then tow

        tows20mmT <- tows20mmT[order(tows20mmT$Survey, tows20mmT$Station, 
                                     tows20mmT$Tow),]
        
      # remove the NA volumes

        tows20mmT <- tows20mmT[-which(is.na(tows20mmT$V)),]

      # inputs
      #   Y = counts, W = instantaneous velocity, V = sample volume/1000

        Y <- tows20mmT$LCSmeltA
        V <- tows20mmT$V/1000
        W <- tows20mmT$InstVel


    # distance matrix (in km)

      DM <- as.matrix((spatial_p$waterDist)/1000)

    # references

      # station of each possible station-survey combo

        station <- rep(1:34, 10 * 20)

      # year of each possible station-survey combo

        year <- iyear - 1994

      # week of each year-survey combo, filled in with dummy value if not used

        week <- matrix(NA, nrow = 20, ncol = 10)
        for(i in 1:20){
          for(j in 1:10){
            week[i,j] <- visits20mmRNAomit$Week[
                            which(visits20mmRNAomit$Year == (1995:2014)[i] & 
                                  visits20mmRNAomit$WYSurvey == j)][1]
            week[i,j] <- ifelse(is.na(week[i,j]), week[i,j-1] + 1, week[i,j])
          }
        }

      # station-survey of each tow

        stationsurvey <- rep(NA, length(Y))
        for(i in 1:length(stationsurvey)){

          spot <- which(istation == tows20mmT$Station[i] & 
                        isurvey == tows20mmT$Survey[i] & 
                        iyear == tows20mmT$Year[i] )
          if(length(spot) > 0){
            stationsurvey[i] <- spot
          }
        }

      # within year survey index across all survey-station combos

        swinyear <- rep(rep(1:10, each = 34), 20)

      # within year survey-station index across all survey-station combos

        ssinyear <- rep(1:(34 * 10), 20)

      # drop off the observations that don't have covariates attached 
      #  (NA station-survey, all 0 covariates)

        naSS <- which(is.na(stationsurvey))

        pp <- X[(stationsurvey),] 
        all0cv <- which(as.numeric(apply(pp[ , 2:17], 1, sum)) == 0)

        todrop <- unique(c(naSS, all0cv))

        Y <- Y[-todrop]
        W <- W[-todrop]
        V <- V[-todrop]
        stationsurvey <- stationsurvey[-todrop]

    # Restrict the covariate set

      X <- as.matrix(X[,c(1, 4:5,8:17)])

    # Take the absolute value of velocity

      W <- abs(W)

    # data descriptors
    #  NT = # observed tows, NY = # years, M = # stations, 
    #  sinyear = # potential surveys in a year
    #  MN = potential # of survey-station combos across all years
    #  NC = # covariates

      NT <- length(Y)
      NY <- 20
      M <- 34
      sinyear <- 10
      MN <- NY * M * sinyear
      NC <- ncol(X)

  # prep output

    output <- list(Y = Y, V = V, W = W, X = X, NT = NT, MN = MN, NY = NY, 
                   NC = NC, M = M, DM = DM, year = year, 
                   stationsurvey = stationsurvey, station = station, 
                   sinyear = sinyear, week = week, swinyear = swinyear, 
                   ssinyear = ssinyear)

  # return

    return(output)

}


##############################################################################
#
# inits_fnc
#
# initializer functions for the JAGS model
#
# Inputs: the chain
#
# Outputs: initialization list for the chain
#

inits_fnc_delta_mu <- function(chain){
      gen_list <- function(chain = chain){
        list(.RNG.name = sample(c(
                  "base::Wichmann-Hill",
                  "base::Marsaglia-Multicarry",
                  "base::Super-Duper",
                  "base::Mersenne-Twister"),
                                                1),
          .RNG.seed = sample(1:1e+06, 1),
          mu_delta = rnorm(1, -0.144, 0.7)
        )
      }

    # return  

      return(gen_list(chain))
}




inits_fnc_toy <- function(chain){
      gen_list <- function(chain = chain){
        list(.RNG.name = sample(c(
                  "base::Wichmann-Hill",
                  "base::Marsaglia-Multicarry",
                  "base::Super-Duper",
                  "base::Mersenne-Twister"),
                                                1),
          .RNG.seed = sample(1:1e+06, 1),
          mu = rnorm(1, 0, 2), phi = runif(1, 0.001, 0.999)
        )
      }

    # return  

      return(gen_list(chain))
}
