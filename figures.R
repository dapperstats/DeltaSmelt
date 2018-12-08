# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# This script prepares the figures
#
# The data functions used in this script are housed in data_prep_functions.R.
# The figure functions used in this script are in figure_functions.R.
# The model outputs called in this script are in output/model_output
#

library(png)
source("figure_functions.R")

load("data/processed_spatial.RData")

delta_map(spatial_p) 