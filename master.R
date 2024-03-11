# code works with R version 4.1.3
rm(list=ls())
source("functions.R")

# (installing and) loading required packages:
packages <- c("RODBC", "move", "dplyr", "lubridate", "devtools", "plotrix", "sp", "suntools", "gdata", 
"randomForest", "plyr", "moments", "MuMIn", "lme4", "DHARMa", "emmeans", "glmmTMB", "RMark", "mgcv",  
"gamm4", "mapview", "grDevices", "nlme", "OpenStreetMap", "ggplot2", "png", "DescTools")
ipak(packages)

set.seed(3) # to be able to reproduce random processes
options(digits=10)

# PROCESS DATA
source("1_data_preparation/1_load_and_process_data_from_movebank.R")
source("1_data_preparation/2_determine_breeding_phases_and_link_habitat_and_diel_tidal_cycle.R")

# ANALYSE DATA
source("2_analyses/1_time_allocation_analysis.R") 
source("2_analyses/2_foraging_trip_analysis.R")
source("2_analyses/3_performance_tagged_vs_untagged_birds.R")

# VISUALIZE RESULTS 
setEPS()
source("3_visualisation/1_plot_data_on_breeding_phases_and_habitat_map.R") 
source("3_visualisation/2_time_allocation_plots.R") 
source("3_visualisation/3_foraging_trip_plots.R") 