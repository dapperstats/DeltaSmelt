# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# This script runs the main analyses
#
# The data functions used in this script are housed in data_prep_functions.R.
# The model functions used in this script are in model_running_functions.R.
# The summary functions used in this script are in summary_functions.R.
# The model scripts used in this script are in the models/ subfolder.
#

library(runjags); library(rjags); library(ape); library(mvtnorm)
source("model_running_functions.R")
source("data_prep_functions.R")
source("summary_functions.R")

load("data/processed_20mm.RData")
load("data/processed_velocity.RData")
load("data/processed_map.RData")
load("data/processed_outflow.RData")
DMY_inv <- prepare_data(mm20_p, spatial_p, vel_p, "dmy")

data_m1 <- prepare_data(mm20_p, spatial_p, vel_p, 1)
m1 <- run_mod(1, data_m1, mcmc = c(8, 2000, 5000, 500, 40))
m1_eval <- eval_mod(1, data_m1)

data_m2 <- prepare_data(mm20_p, spatial_p, vel_p, 2)
m2 <- run_mod(2, data_m2, mcmc = c(8, 2000, 5000, 500, 40))
m2_eval <- eval_mod(2, data_m2)

data_m3 <- prepare_data(mm20_p, spatial_p, vel_p, 3)
m3 <- run_mod(3, data_m3, mcmc = c(8, 2000, 5000, 500, 40))
m3_eval <- eval_mod(3, data_m3)

data_m4 <- prepare_data(mm20_p, spatial_p, vel_p, 4)
m4 <- run_mod(4, data_m4, mcmc = c(8, 2000, 5000, 500, 40))
m4_eval <- eval_mod(4, data_m4)

data_m5 <- prepare_data(mm20_p, spatial_p, vel_p, 5)
m5 <- run_mod(5, data_m5, mcmc = c(8, 2000, 5000, 500, 40))
m5_eval <- eval_mod(5, data_m5)

data_m6 <- prepare_data(mm20_p, spatial_p, vel_p, 6)
m6 <- run_mod(6, data_m6, mcmc = c(8, 2000, 5000, 500, 40))
m6_eval <- eval_mod(6, data_m6)

data_m7 <- prepare_data(mm20_p, spatial_p, vel_p, 7)
m7 <- run_mod(7, data_m7, mcmc = c(8, 2000, 5000, 500, 40))
m7_eval <- eval_mod(7, data_m7)

data_m8 <- prepare_data(mm20_p, spatial_p, vel_p, 8)
m8 <- run_mod(8, data_m8, mcmc = c(8, 2000, 5000, 500, 40))
m8_eval <- eval_mod(8, data_m8)

data_m9 <- prepare_data(mm20_p, spatial_p, vel_p, 9)
m9 <- run_mod(9, data_m9, mcmc = c(8, 2000, 5000, 500, 40))
m9_eval <- eval_mod(9, data_m9)




