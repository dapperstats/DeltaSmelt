# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# This script contains function code underlying raw_data_processing.R
#   script, which processes the raw data files for the 20mm and
#   salvage databases, shapefile of the Delta, summarized Delta outflow,
#   and summarized Delta velocity files.
# 
# This script's main purpose is to produce reasonable-sized data files for
#   sharing associated with the manuscript. Most users will not need to 
#'  re-process the raw data, so should not need to use this code block.
#
# Because of the size of the associated raw data files, these scripts and the 
#   raw data are not presently distributed alongside the downstream code
#   and data.


read_in_salvage <- function(filepath){
  conn <- odbcConnectAccess2007(filepath)
  Sample<-sqlQuery(conn, "SELECT * FROM Sample")
  Catch<-sqlQuery(conn, "SELECT * FROM Catch")
  Building<-sqlQuery(conn, "SELECT * FROM Building")
  Length <-sqlQuery(conn, "SELECT * FROM Length")
  close(conn)
  list(Sample = Sample, Catch = Catch, Building = Building, Length = Length)
}

process_salvage <- function(salvage){
  catches <- Catch[which(Catch$OrganismCode == 26), "BuildingRowID"]
  samplesC <- Building[
                which(Building$BuildingRowID %in% catches & 
                      Building$BuildingCode == "F"), "SampleRowID"]
  samplesS <- Building[
                which(Building$BuildingRowID %in% catches & 
                      Building$BuildingCode %in% c("NS", "OS")),
                "SampleRowID"]
  list(CVP = samplesC, SWP = samplesS)
}



##############################################################################
#
# read_in_20mm
#
# reads in the 20mm database
#
#
# Inputs: file location of database
#			
# Outputs: database as a list
#			

read_in_20mm <- function(filepath){  

    con <- odbcConnectAccess2007(filepath)
    mm20 <- list(Stations = sqlQuery(con, "SELECT * FROM 20mmStations"),
                 Catch = sqlQuery(con, "SELECT * FROM Catch"),
                 FishCodes = sqlQuery(con, "SELECT * FROM FishCodes"),
                 Lengths = sqlQuery(con, "SELECT * FROM Lengths"),
                 MeterCorrections = sqlQuery(con, 
                                           "SELECT * FROM MeterCorrections"),
                 TowInfo = sqlQuery(con, "SELECT * FROM TowInfo"),
                 WaterInfo = sqlQuery(con, "SELECT * FROM WaterInfo"),
                 WtFactors = sqlQuery(con, "SELECT * FROM Wt_factors"),
                 ZooCatch = sqlQuery(con, "SELECT * FROM ZooCatch"),
                 ZooCodes = sqlQuery(con, "SELECT * FROM ZooCodes"),
                 Zooplankton = sqlQuery(con, "SELECT * FROM Zooplankton"))
    odbcCloseAll() 

  # return

    return(mm20)
}


##############################################################################
#
# process_20mm_data
#
# processes the full 20mm database into what's needed for analyses
#
# Inputs: the list that's the read-in 20mm database
#			
# Outputs: list of 20mm lengths, 20mm tows, 20mm visits

process_20mm_data <- function(data){

  #  each station on each visit gets up to 3 net tows 
  #    one major set of data is centered around the tow  
  #  and during each visit covariates are collected
  #    one major set of data is centered around the visit
  #  and for every fish collected there is a length
  #    one major set of data is centered around the fish


    # lengths of smelt sampled

      lengths20mm <- data$Length[which(data$Length$"Fish Code" == 3 ),
                                 c(1:3, 5)]

    # tow 
  
      # grab the data we want from the tow table

        tows20mm <- data.frame(data$TowInfo[,c(1:12)])
    
      # Interpolate a few missing time values
      #  just doing manually now for the ones that we need
      #  on average tows come 14 minutes after the previous one at the station
      #  adding in 14 minutes after the previous tow

        spot <- which(tows20mm$Date == "2000-05-24" & 
                      tows20mm$Station == 606 &
                      tows20mm$Tow == 2)
        tows20mm$Time[spot] <- "1899-12-30 08:53:00 PST"
        spot <- which(tows20mm$Date == "2000-06-13" & 
                      tows20mm$Station == 815 &
                      tows20mm$Tow == 2)
        tows20mm$Time[spot] <- "1899-12-30 07:06:00 PST"
        spot <- which(tows20mm$Date == "2000-06-13" & 
                      tows20mm$Station == 815 &
                      tows20mm$Tow == 3)
        tows20mm$Time[spot] <- "1899-12-30 07:20:00 PST"
        spot <- which(tows20mm$Date == "2005-03-14" & 
                      tows20mm$Station == 910 &
                      tows20mm$Tow == 3)
        tows20mm$Time[spot] <- "1899-12-30 15:51:00 PST"
        spot <- which(tows20mm$Date == "2005-05-25" & 
                      tows20mm$Station == 704 &
                      tows20mm$Tow == 3)
        tows20mm$Time[spot] <- "1899-12-30 07:45:00 PST"
        spot <- which(tows20mm$Date == "2006-04-04" & 
                      tows20mm$Station == 906 &
                      tows20mm$Tow == 3)
        tows20mm$Time[spot] <- "1899-12-30 08:37:00 PST"
        spot <- which(tows20mm$Date == "2006-04-05" & 
                      tows20mm$Station == 706 &
                      tows20mm$Tow == 3)
        tows20mm$Time[spot] <- "1899-12-30 11:41:00 PST"

      # fix an obvious typo on a time entry
      #  2006-04-08 520 3 should be 12:37 not 21:37

        spot <- which(tows20mm$Date == "2006-04-08" & 
                      tows20mm$Station == 520 &
                      tows20mm$Tow == 3)
        tows20mm$Time[spot] <- "1899-12-30 12:37:00 PST"

      # create the fraction of day time column

        tows20mm$TimeFOD <- (as.numeric(format(tows20mm$Time, "%H")) * 60 +
                             as.numeric(format(tows20mm$Time, "%M"))) /
                            (24 * 60)

      # edit the time column

        tows20mm$Time <- format(tows20mm$Time, "%H:%M")
  


      # determine the volume for each tow
      #
      #  V = A * K * D
      #    V is volume (m3) of the tow
      #    A is area (m2) of the mouth opening of the net
      #      constant for the net at 1.51 m2
      #    K is the calibration factor for the flow meter
      #      (to correct "m3" to m3)
      #    D is the difference in the flow meter counts from start to 
      #      finish (m?)
      #      "Net.Meter.Check"
    
        # K depends on the net used and the year of the survey
      
          tYear <- as.numeric(format(tows20mm$Date, "%Y"))

          tows20mm$K <- NA

          for(i in 1:nrow(tows20mm)){
            tows20mm$K[i] <- data$MeterCorrections$"k factor"[
                              which(data$MeterCorrections$"Meter Serial" == 
                                         tows20mm$Net.Meter.Serial[i] & 
                                    data$MeterCorrections$"Study Year" == 
                                         tYear[i])]
          }
    
        # V is then calculated

          tows20mm$V <- 1.51 * tows20mm$K * tows20mm$Net.Meter.Check

      # smelt sampled (ugh slow, but it works)
      #  total (by the counts and by the lengths) and large (over 15 mm) and 
      #  large but in the cohort (smaller than 0.35 * JDay)

        tows20mm$TSmelt <- 0
        tows20mm$TSmeltL <- 0
        tows20mm$LSmelt <- 0
        tows20mm$LCSmelt <- 0


        for(i in 1:nrow(tows20mm)){

          ts <- data$Catch$Catch[which(data$Catch$"Fish Code" == 3 & 
                                       data$Catch$Date == tows20mm$Date[i] & 
                                       data$Catch$Tow == tows20mm$Tow[i] & 
                                       data$Catch$Station ==
                                                       tows20mm$Station[i])]
          ts[length(ts)==0] <- 0
          tows20mm$TSmelt[i] <- ts  

          tl <- data$Lengths[which(data$Lengths$"Fish Code" == 3 & 
                                   data$Lengths$Date == tows20mm$Date[i] & 
                                   data$Lengths$Tow == tows20mm$Tow[i] & 
                                   data$Lengths$Station ==  
                                                    tows20mm$Station[i]), ]

          JD <- as.numeric(format(tows20mm$Date[i], "%j"))

          tows20mm$TSmeltL[i] <- length(tl$Length)
          tows20mm$LSmelt[i] <- length(which(tl$Length >= 15))
          tows20mm$LCSmelt[i] <- length(which(tl$Length >= 15 & 
                                              tl$Length < 0.35*JD))

        }

      # account for the few smelts without lengths

        spots <- which(tows20mm$TSmelt != tows20mm$TSmeltL)
        nvc <- tows20mm$TSmelt[which(tows20mm$TSmelt != tows20mm$TSmeltL)]
        nvl <- tows20mm$TSmeltL[which(tows20mm$TSmelt != tows20mm$TSmeltL)]

        # dates of those samplings

          dnl <- tows20mm$Date[which(tows20mm$TSmelt != tows20mm$TSmeltL)]

          # associated lengths, and how many over 15 mm and in the cohort

          dnlt <- rep(NA, length(dnl))
          dnllc <- rep(NA, length(dnl))

          for(i in 1:length(dnl)){
            tt <- lengths20mm$Length[which(lengths20mm$Date == dnl[i])]
            jd <- as.numeric(format(dnl[i], "%j"))
            dnlt[i] <- length(tt)
            dnllc[i] <- length(tt[tt >= 15 & tt < 0.35 * jd])
          }

        # fraction of smelts without lengths expected to be over 15 mm and in 
        #  the cohort

          frl <- dnllc / dnlt

        # number of smelts without lengths expected to be over 15 mm and in 
        #  the cohort

          nl <- round((nvc - nvl) * frl)

        # add them to the total counts

          tows20mm$LCSmeltA <- tows20mm$LCSmelt
          tows20mm$LCSmeltA[spots] <- tows20mm$LCSmelt[spots] + nl  

    # visit: requires grabbing data from multiple tables in the database
  
      # grab the data we want from the tow table, but just for the first tow 
      #  of each visit
      #  note that bottom depth is in feet 

        visits20mm <- data.frame(data$TowInfo[which(data$TowInfo$Tow == 1),
                                              c(1:2, 5:6, 13:16)])

      # water parameters and survey numbers from the water info table

        visits20mm$Temp <- NA
        visits20mm$TopEC <- NA
        visits20mm$BottomEC <- NA
        visits20mm$Secchi <- NA
        visits20mm$Turbidity <- NA
        visits20mm$SurveyNumber <- NA
      

        for(i in 1:nrow(visits20mm)){

          xx<-which(data$WaterInfo$Date == visits20mm$Date[i] & 
                    data$WaterInfo$Station ==  visits20mm$Station[i] )
  
          visits20mm$Temp[i] <- data$WaterInfo$Temp[xx] 
          visits20mm$TopEC[i] <- data$WaterInfo$"Top EC"[xx]
          visits20mm$BottomEC[i] <- data$WaterInfo$"Bottom EC"[xx] 
          visits20mm$Secchi[i] <- data$WaterInfo$Secchi[xx] 
          visits20mm$Turbidity[i] <- data$WaterInfo$Turbidity[xx]
          visits20mm$SurveyNumber[i] <- data$WaterInfo$Survey[xx]

        }


      # zooplankton
    
        # sample volume, calculated from the tow info 
        #
        #  ZV = ZA * ZK * ZD
        #    ZV is volume (m3) of the tow
        #    ZA is area (m2) of the mouth opening of the net
        #       ZA is a constant for the net at 0.0101
        #    ZK is the calibration factor for the flow meter 
        #      (to correct "m3" to m3)
        #    ZD is the difference in the flow meter counts from start to
        #      finish (m?)
        #      ZD is the difference in the flow meter counts, aka the
        #      "net meter check" value
  
  
          # ZK depends on the net used and the year of the survey
        
            visits20mm$Year <- as.numeric(format(visits20mm$Date, "%Y"))
  
            visits20mm$ZK <- NA
  
            for(i in (1:nrow(visits20mm))){
              cf <- data$MeterCorrections$"k factor"[
                       which(data$MeterCorrections$"Meter Serial" == 
                                            visits20mm$CB.Meter.Serial[i] & 
                             data$MeterCorrections$"Study Year" == 
                                            visits20mm$Year[i])]
              cf[length(cf) == 0] <- 0
              visits20mm$ZK[i] <- cf
            }

          # XV is then calculated
  
            visits20mm$ZV <- 0.0101 * visits20mm$ZK * 
                               visits20mm$CB.Meter.Check
  
          
        # zooplankton sample processing info, from the zooplankton table
        #
        #  sample "dilution": sample volume
        #  cell count: the number of cells counted
        #  

          visits20mm$Zsampdil <- NA
          visits20mm$Zcellcount <- NA

          for(i in 1:nrow(visits20mm)){
            sdt <- data$Zooplankton$Dilution[which(
                           data$Zooplankton$Date == visits20mm$Date[i] & 
                           data$Zooplankton$Station == visits20mm$Station[i])]
            sdt[length(sdt) == 0] <- 0
            visits20mm$Zsampdil[i] <- sdt[1]
          }


          for(i in 1:nrow(visits20mm)){
            cct <- data$Zooplankton$CellNumber[which(
                           data$Zooplankton$Date == visits20mm$Date[i] & 
                           data$Zooplankton$Station == visits20mm$Station[i])]
            cct[length(cct) == 0] <- 0    
            visits20mm$Zcellcount[i] <- max((cct))
          }


        # total calanoid counts from the zoo catch table
        #
        #  counts are recorded by cell, so we'll sum across cells for an 
        #    overall value
        #
        #  all calanoids codes: 31-38, 43, 47-50, 86, 87
        #

          visits20mm$TotalCalanoidCOUNT <- NA

          for(i in 1:nrow(visits20mm)){

            tss <- data$ZooCatch[which(
                          data$ZooCatch$Date == visits20mm$Date[i] & 
                           data$ZooCatch$Station == visits20mm$Station[i]),]
            visits20mm$TotalCalanoidCOUNT[i]<- sum(tss$Count[tss$ZooCode %in%
                                                 c(31:38, 43, 47:50, 86, 87)])
          }
      
        # total calanoid densities
        #
        #  (count * sample dilution)/# cells/total volume
        #
      
          visits20mm$TotalCalanoidDENSITY <-  round(
              (visits20mm$TotalCalanoidCOUNT * visits20mm$Zsampdil) / 
                visits20mm$Zcellcount / visits20mm$ZV, 3)


      # for each visit, summarize the total long fish in the cohort, volume, 
      #  and number of tows across all tows

        visits20mm$VLCSmelt <- NA
        visits20mm$VTV <- NA
        visits20mm$Ntows <- NA

        for(i in 1:nrow(visits20mm)){

          vts <- tows20mm[which(tows20mm$Date == visits20mm$Date[i] & 
                                tows20mm$Station == visits20mm$Station[i]),]
          visits20mm$VLCSmelt[i] <- sum(vts$LCSmeltA)
          visits20mm$VTV[i] <- sum(vts$V)
          visits20mm$Ntows[i] <- nrow(vts)
        }

  # prep output 

    output <- list(lengths20mm, tows20mm, visits20mm) 
    names(output) <- c("lengths20mm", "tows20mm", "visits20mm") 

  # return

    return(output)
}


##############################################################################
#
# process_outflow_data
#
#

process_outflow_data <- function(outflow){
  outflow$Date <- yyyymmdd(outflow$Date)
  outflow$Outflow[outflow$Outflow < 1] <- 1
  outflow
}



##############################################################################
#
# process_spatial_data
#
# processes the map and spatial information into what's needed for analyses
#
# Inputs: the map, 
#         the list that's the read-in 20mm database, the processed 20mm data
#			
# Outputs: list of raster components, distances, and station locations



process_spatial_data <- function(map, data, processed_data){

  # spatial locations
  #
  #  we need to verify that all of the station locations we'll be using lie 
  #    within the polygon provided
  #    -using the stations regularly sampled since 1995 
  #      (all of those have > 100 visits)
  #    -and included within the DSM2 model coverage

    # create utm locations for the stations

      Lat <- rep(NA, nrow(data$Stations))
      Lon <- rep(NA, nrow(data$Stations))
    
      for(i in 1:nrow(data$Stations)){
        Lat[i] <- data$Stations$LatD[i] + data$Stations$LatM[i] / 60 + 
                  data$Stations$LatS[i] / 3600
        Lon[i] <- -1 * (data$Stations$LonD[i] + data$Stations$LonM[i] / 60 + 
                        data$Stations$LonS[i] / 3600)
      }

      xy <- cbind(Lon, Lat)
      stationlocs <- project(xy, "+proj=utm +zone=10 ellps=WGS84")
      Easting <- stationlocs[,1]
      Northing <- stationlocs[,2]

    # count how many times each station was visited, restrict to those with 
    #   at least 100 visits

      visits <- table(processed_data$visits20mm$Station)
      visits <- (visits[which(visits >= 100)])

    # remove the stations out of the DSM2 coverage  

      outofdsm2 <- c(323, 340, 342:346)
      visits <- visits[-which(outofdsm2 %in% names(visits))]

    # check the spatial locations of the stations against the delta polygon

      IPTF <- pointInSpatialPolygons(stationlocs[,1], stationlocs[,2], map)

    # there is one station (918) that actually doesn't fall within the polygon 
    # moving the location slightly to accommodate
    #  original      new
    #  625898    4191170    625978    4191170

      stationlocsA <- stationlocs
      stationlocsA[which(
       data$Stations$Station %in% names(visits) & IPTF == F)[1],1] <- 
       (stationlocsA[which(
        data$Stations$Station %in% names(visits) & IPTF == FALSE)[1], 1]) + 80

    # create the final for use station location table

      stalocs <- stationlocsA[which(
                   data$Stations$Station %in% names(visits)), ]
      rownames(stalocs) <- data$Stations$Station[which(
                              data$Stations$Station %in% names(visits))]

  # measure distances between stations
  #  
  #  we use a raster approach to do this
  #

    # rasterize the polygon and designate within water with = 1

      r <- raster(ncol = 1394, nrow = 1500)
      extent(r) <- extent(map)
      rp <- rasterize(map, r, 1)

    # measure distances between points within the polygon 
    #  (i.e. water distances)

      rp0 <- rp
      rp0[is.na(rp0)] <- 0
      tp0 <- transition(rp0, mean, 16)
      tpc0 <- geoCorrection(tp0, "c", scl = F)
      waterDist <- costDistance(tpc0, stalocs)

      summary(waterDist)

    # measure distances between points as the crow flies 
    #  (i.e. geographic distances)

      gDist <- dist(stalocs)


  # prep output

    output <- list(r, rp, rp0, tp0, tpc0, stalocs, waterDist, gDist)
    names(output) <- c("r", "rp", "rp0", "tp0", "tpc0",
                       "stalocs", "waterDist", "gDist")

  # return

    return(output)

}


##############################################################################
#
# process_velocity_data
#
# processes the velocity into what's needed for analyses
#
# Inputs: the read-in velocity data, 
#         the processed 20mm data
#			
# Outputs: list of
#            table of weekly average velocit for each station for each week
#            updated tow data



process_velocity_data <- function(data, mm20_p){

  # make a table of the weekly average (sunday 00:00 through saturday 23:45) 
  #  for each station each week
  
    weekstart <- as.Date("1995-01-01") + 7 * (1:1123)
    wks <- 1:(length(weekstart)-1)
    warvt <- data.frame(matrix(NA, nrow = length(wks), ncol = ncol(data)))
    warvt[,1] <- wks
    colnames(warvt) <- c("week", colnames(data)[2:ncol(data)])  
  
    vds <- as.Date(strptime(data[,1], "%Y-%m-%d"))

    for(i in 1:1096){

      dois <- weekstart[i] + (0:6)
      tmr <- which(vds %in% dois)
      tm <- data[tmr, 2:35]
      warvt[i, 2:35] <- apply(tm, 2, mean)

    }

    # sort the columns to be in order

      colnames(warvt)[1] <- 1
      warvt <- warvt[,order(colnames(warvt))]
      colnames(warvt)[1] <- "week"



    # append the instantanteous velocitiy to the tow table

      mm20_p$tows20mm$InstVel <- 0  

      for(i in 1:nrow(mm20_p$tows20mm)){

        samps <- mm20_p$tows20mm$Station[i]
        sampd <- as.POSIXlt(paste(mm20_p$tows20mm$Date[i], 
                                  mm20_p$tows20mm$Time[i],
                                  sep = " "))
        cc <- which(colnames(data)==samps)
        rr <- which.min(abs(data$DateTime - sampd))
        vv <- as.numeric(data[rr, cc])
        vv[length(vv) == 0] <- NA
        mm20_p$tows20mm$InstVel[i] <- vv
      }

  # prep output

    output <- list(warvt, mm20_p$tows20mm)
    names(output) <- c("warvt", "mm20_p$tows20mm") 

  # return

    return(output)

}


##############################################################################
#
# pointInSpatialPolygons
#
# checks if a point is in a spatial polygon
#
# cribbed from the point.in.SpatialPolygons() function from the prevR package,
#  but improved speed
#
# Inputs: the x and y coordinates of the point (point.x, point.y) and the 
#   shapte file (SpP)
#			
# Outputs: logical regarding the location of the point inside (T) or not (F) 
#   the polygon
#			

pointInSpatialPolygons <- function(point.x, point.y, SpP){  

  X <- slot(SpP,"polygons")
  is.inside <- FALSE
  is.outsideHole <- TRUE

  for(i in 1:length(X)){
        
    PS <- slot(X[[i]],"Polygons")
      
    for(j in 1:length(PS)){

      pointsPosition <- point.in.polygon(point.x, point.y, 
                                         slot(PS[[j]], "coords")[,1], 
                                         slot(PS[[j]], "coords")[,2])
      if(!slot(PS[[j]],"hole")) {
        is.inside <- is.inside | pointsPosition != 0
      } else{
        is.outsideHole <- is.outsideHole & 
                           (pointsPosition == 0 |  pointsPosition == 3)
      }
    }
  }

  is.inside & is.outsideHole
}


##############################################################################
#
# yyyymmdd 
#
# converts dates from excel style to standard yyyymmdd
#

yyyymmdd <- function(x){
 
  slashCheck <- all(grepl("\\/", x))
  numerals <- strsplit(as.character(x), "/")[[1]]
  numeralCheck1 <- nchar(numerals[1]) %in% 1:2 & nchar(numerals[2]) %in% 1:2 & 
                     nchar(numerals[3]) == 4  
  numeralCheck2 <- nchar(numerals[1]) %in% 1:2 & nchar(numerals[2]) %in% 1:2 & 
                     nchar(numerals[3]) == 2  
  if (slashCheck & numeralCheck1){
    x <- as.Date(as.character(x), format = "%m/%d/%Y")
  }
  if (slashCheck & numeralCheck2){
    x <- as.Date(as.character(x), format = "%m/%d/%y")
  }
  return(x)
}


