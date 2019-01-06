# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# This script prepares the tables, figures, and summary statistics
#
# The data functions used in this script are housed in data_prep_functions.R.
# The figure functions used in this script are in figure_functions.R.
# The summary functions used in this script are in summary_functions.R.
# The model outputs called in this script are in output/model_output
#

library(png); library(ape); library(mgcv)
source("figure_functions.R")
source("data_prep_functions.R")
source("summary_functions.R")

load("data/processed_20mm.RData")
load("data/processed_velocity.RData")
load("data/processed_map.RData")
load("data/processed_outflow.RData")

# full data summary

ds_data <- prepare_data(mm20_p, spatial_p, vel_p, "all", outflow_p)
DMY_inv <- prepare_data(mm20_p, spatial_p, vel_p, "dmy")
ds_data$NT
ds_data$NDS
summaryY(ds_data$Y)
summaryV(ds_data$V)
summaryD(ds_data$Y, ds_data$V)
summaryTows(ds_data$Y, ds_data$V, ds_data$DSid, ds_data$TO, ds_data$W)
MI_dens <- Moran.I(ds_data$Y/ds_data$V, DMY_inv)


# figures

delta_map(spatial_p)
salvage_time(salvage_p)
priors_fig()
covariates_fig(ds_data, 9)
ac_fig(9)
date_hmap(ds_data, 9)
flow_hmap(ds_data, 9)
ppc_fig(ds_data, 9)

# table

m9tab <- mcmc_table(9)

# text 

peak_descrips(ds_data, 9)
obs_pred_cor(ds_data, 9)
r2(ds_data, 9)

 