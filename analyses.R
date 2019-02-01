# Juvenile delta smelt distribution analysis
# JL Simonis and JE Merz
#
# This script runs the main analyses.
#
# In addition to R, it is required that you install JAGS to re-run
#   any analyses: http://mcmc-jags.sourceforge.net/
# Note, however, that analysis output is provided, and JAGS is
#   not required to evaluate the outputs.
#
# The functions used in this script are housed in functions.R.
# The data files used in this script are housed in the data/ subfolder. 
# The model scripts used in this script are in the models/ subfolder, as is
#   a descriptor file detailing what each model number corresponds to.
# The manuscript focuses on model 9.
# Note that model 9 takes a substantial amount of time to run.
# Copies of the model MCMC output files are saved in the output/model_output
#   subfolder, including the output from model 9 used in the manuscript

pkgs <- c("runjags", "rjags", "ape", "mvtnorm", "png", "ape", "mgcv")
install.packages(pkgs)
library(runjags); library(rjags); library(ape); library(mvtnorm);
library(png); library(ape); library(mgcv)
source("functions.R")

load("data/processed_20mm.RData")
load("data/processed_velocity.RData")
load("data/processed_map.RData")
load("data/processed_outflow.RData")
load("data/processed_salvage.RData")

DMY_inv <- prepare_data(mm20_p, spatial_p, vel_p, "dmy")
ds_data <- prepare_data(mm20_p, spatial_p, vel_p, "all", outflow_p)
data_m1 <- prepare_data(mm20_p, spatial_p, vel_p, 1)
data_m2 <- prepare_data(mm20_p, spatial_p, vel_p, 2)
data_m3 <- prepare_data(mm20_p, spatial_p, vel_p, 3)
data_m4 <- prepare_data(mm20_p, spatial_p, vel_p, 4)
data_m5 <- prepare_data(mm20_p, spatial_p, vel_p, 5)
data_m6 <- prepare_data(mm20_p, spatial_p, vel_p, 6)
data_m7 <- prepare_data(mm20_p, spatial_p, vel_p, 7)
data_m8 <- prepare_data(mm20_p, spatial_p, vel_p, 8)
data_m9 <- prepare_data(mm20_p, spatial_p, vel_p, 9)

#model running code can be skipped to use existing model MCMC files
#m1 <- run_mod(1, data_m1, mcmc = c(8, 2000, 5000, 500, 40))
#m2 <- run_mod(2, data_m2, mcmc = c(8, 2000, 5000, 500, 40))
#m3 <- run_mod(3, data_m3, mcmc = c(8, 2000, 5000, 500, 40))
#m4 <- run_mod(4, data_m4, mcmc = c(8, 2000, 5000, 500, 40))
#m5 <- run_mod(5, data_m5, mcmc = c(8, 2000, 5000, 500, 40))
#m6 <- run_mod(6, data_m6, mcmc = c(8, 2000, 5000, 500, 40))
#m7 <- run_mod(7, data_m7, mcmc = c(8, 2000, 5000, 500, 40))
#m8 <- run_mod(8, data_m8, mcmc = c(8, 2000, 5000, 500, 40))
#m9 <- run_mod(9, data_m9, mcmc = c(8, 2000, 5000, 500, 40))

m1_eval <- eval_mod(1, data_m1, DMY_inv)
m2_eval <- eval_mod(2, data_m2, DMY_inv)
m3_eval <- eval_mod(3, data_m3, DMY_inv)
m4_eval <- eval_mod(4, data_m4, DMY_inv)
m5_eval <- eval_mod(5, data_m5, DMY_inv)
m6_eval <- eval_mod(6, data_m6, DMY_inv)
m7_eval <- eval_mod(7, data_m7, DMY_inv)
m8_eval <- eval_mod(8, data_m8, DMY_inv)
m9_eval <- eval_mod(9, data_m9, DMY_inv)

ds_data$NT
ds_data$NDS
summaryY(ds_data$Y)
summaryV(ds_data$V)
summaryD(ds_data$Y, ds_data$V)
summaryTows(ds_data$Y, ds_data$V, ds_data$DSid, ds_data$TO, ds_data$W)
MI_obs <- Moran.I(ds_data$Y, DMY_inv)


delta_map(spatial_p)
salvage_time(salvage_p)
priors_fig()
covariates_fig(ds_data, 9)
ac_fig(9)
date_hmap(ds_data, 9)
flow_hmap(ds_data, 9)
ppc_fig(ds_data, 9, MI_obs)

m9tab <- mcmc_table(9)

peak_descrips(ds_data, 9)
obs_pred_cor(ds_data, 9)
r2(ds_data, 9)
