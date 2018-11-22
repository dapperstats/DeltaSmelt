#
# This is the main script for Simonis and Merz delta smelt manuscript. 
#
# Because of the size of the raw data files, this script begins with the
# pre-processed data.
#
# mm20_p: 20 mm survey data structured by fish length, tow, and visit
# vel_p: weekly average velocity (and a table that is also now in mm20_p)
# spatial_p: list of raster components, distances, and stration locations
# outflow_p: daily outflow and x2
#

library(gdistance); library(maptools); library(rjags); library(runjags)
source("Functions.R")

load("data/processed_20mm_files.RData")
load("data/processed_velocity_files.RData")
load("data/processed_spatial_files.RData")
load("data/processed_outflow_files.RData")

prepped_data <- prepare_model_input(mm20_p, spatial_p, vel_p)

# to add to prepped_data
prepped_data$NSS <- length(unique(prepped_data$stationsurvey))

# delta_mu model: only a single overall intercept

tdelta_mu <- system.time(
delta_mu_run <- run.jags(model = "delta_mu.R", modules = "glm", 
                      monitor = c("delta_mu"),
                      data = prepped_data, inits = inits_fnc_delta_mu,
                      n.chains = 4,
                      adapt = 1000, burnin = 2000, sample = 2000, thin = 1,
                      summarise = FALSE, plots = FALSE, method = "parallel",
                      keep.jags.files = "output/model_output/delta_mu")
)

tdelta_mu
plot(delta_mu_run)
summary(delta_mu_run)


