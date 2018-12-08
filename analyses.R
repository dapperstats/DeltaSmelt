# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# This script runs the main analyses
#
# The data functions used in this script are housed in data_prep_functions.R.
# The model functions used in this script are in model_running_functions.R.
# The model scripts used in this script are in the models/ subfolder.
#

# to do:
#  keep adding models

library(runjags); library(rjags); library(ape)
source("model_running_functions.R")
source("data_prep_functions.R")

load("data/processed_20mm.RData")
load("data/processed_velocity.RData")
load("data/processed_spatial.RData")
load("data/processed_outflow.RData")
DMY_inv <- prepare_data(mm20_p, spatial_p, vel_p, "dmy")

data_m1 <- prepare_data(mm20_p, spatial_p, vel_p, 1)
m1 <- run_mod(1, data_m1, mcmc = c(8, 1000, 1000, 5000, 1))
m1_eval <- eval_mod(1, data_m1, m1, DMY_inv)

data_m2 <- prepare_data(mm20_p, spatial_p, vel_p, 2)
m2 <- run_mod(2, data_m2, mcmc = c(8, 1000, 1000, 5000, 1))
m2_eval <- eval_mod(2, data_m2, m2, DMY_inv)

data_m3 <- prepare_data(mm20_p, spatial_p, vel_p, 3)
m3 <- run_mod(3, data_m3, mcmc = c(8, 1000, 1000, 5000, 1))
m3_eval <- eval_mod(3, data_m3, m3, DMY_inv)


