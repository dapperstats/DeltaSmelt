# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# This script contains code to process the raw data files for the 20mm and
#   salvage databases, shapefile of the Delta, summarized Delta outflow,
#   and summarized Delta velocity files.
# 
# This script's main purpose is to produce reasonable-sized data files for
#   sharing associated with the manuscript. Most users will not need to 
#   re-process the raw data, so should not need to use this code block.
#
# The novel functions used in this script are housed in 
#   raw_data_processing_functions.R.
#
# Because of the size of the associated raw data files, these scripts and the 
#   raw data are not presently distributed alongside the downstream code
#   and data.

library(RODBC); library(maptools); library(rgdal); library(gdistance);
source("raw_data_processing_functions.R")

mm20 <- read_in_20mm("data/20-mm.mdb")
map <- readShapePoly("data/Map/DeltaMap.shp", delete_null_obj = TRUE,
                     proj4string = CRS("+proj=utm +zone=10 +datum=NAD83"))
load("data/DeltaVelocities.RData")
outflow <- read.csv("data/deltaoutflow.csv")
salvage <- read_in_salvage("data/Salvage_data_FTP.mdb")

mm20_p <- process_20mm_data(data = mm20)
spatial_p <- process_spatial_data(map, data = mm20, processed_data = mm20_p)
vel_p <- process_velocity_data(data = vels, mm20_p = mm20_p)
mm20_p$tows20mm <- vel_p$"mm20_p$tows20mm"
outflow_p <- process_outflow_data(outflow)
salvage_p <- process_salvage(salvage)

save(mm20_p, file = "data/processed_20mm.RData")
save(vel_p, file = "data/processed_velocity.RData")
save(spatial_p, file = "data/processed_map.RData")
save(outflow_p, file = "data/processed_outflow.RData")
save(salvage_p, file = "data/processed_salvage.RData")