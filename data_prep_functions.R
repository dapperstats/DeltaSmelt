
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


prepare_data <- function(mm20_p, spatial_p, vel_p, model){

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

# Just absolute instantaneous velocity 

  W <- abs(tows20mmRNAomit$InstVel)


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

  SO <- visits20mmRNAomit$SurveyNumber
  MSPY <- max(visits20mmRNAomit$SurveyNumber)

# week of each survey for each year
#   using the most common week for the surveys that span a week break
#   (only happened 15 of 176 times, tended to involve <= 15% of visits in the
#    survey and no surveys ever spanned three calendar weeks


  week <- matrix(NA, nrow = NYr, ncol = MSPY)
  for(i in 1:NYr){
    for(j in 1:MSPY){
      matched <- Yrid == UYr[i] & visits20mmRNAomit$SurveyNumber == j
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

  # everything that could be used

  if (model == "all"){
    out <- list(Y = Y, V = V, NT = NT, X = X, NX = NX, DSid = DSid, NDS = NDS,
                DM = DM, Sid = Sid, NS = NS, Yrid = Yrid, NYr = NYr,
                week = week, MSPY = MSPY, SO = SO, TO = TO, W = W,
                MSSPY = MSSPY, SSid = SSid, Z = Z, NZ = NZ)
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