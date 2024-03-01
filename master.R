# code works with R version 4.1.3
rm(list=ls()) # clear workspace

# usage
packages <- c("RODBC", "move", "dplyr", "lubridate", "devtools", "plotrix", "sp", "suntools", "gdata", 
"randomForest", "plyr", "moments", "MuMIn", "lme4", "DHARMa", "emmeans", "glmmTMB", "RMark", "mgcv",  
"gamm4", "mapview", "sp", "grDevices", "nlme", "OpenStreetMap", "ggplot2", "png", "DescTools")
ipak(packages)

source("functions.R")
set.seed(3) # to be able to reproduce random processes (though the order of running the different codes should then be exactly the same)

# data preparation and visualization
#source("0_load_tracking_data_from_uvabits_database_and_export_to_movebank.R") # original downloading script; for this, the time zone should still be set at GMT/UTC
#source("0_retrieve_tracking_data_from_already_downloaded_files.R") # retrieves the same data from previously downloaded files and arranges it in the same order
#save.image("data/raw/gps.acc.data.2012-2019.RData")
load("data/raw/gps.acc.data.2012-2019.RData")

# PROCESS DATA
source("1_data_preparation/1_load_and_process_data_from_movebank.R")
source("1_data_preparation/2_determine_breeding_phases_and_link_habitat_and_diel_tidal_cycle.R")
#save.image("data/tmp/gps.data.behav.final.1129.RData")
load("data/tmp/gps.data.behav.final.1129.RData")

# ANALYSE DATA
source("2_analyses/1_time_allocation_analysis.R") 
# save.image("data/tmp/time.allocation.results.0206.RData")
load("data/tmp/time.allocation.results.0206.RData")
source("2_analyses/2_foraging_trip_analysis.R")
source("3_analyses/3_performance_tagged_vs_untagged_birds.R")

# VISUALIZE RESULTS 
setEPS()
source("3_visualisation/1_plot_data_on_breeding_phases_and_habitat_map.R") 
source("3_visualisation/2_time_allocation_plots.R") 
source("3_visualisation/3_foraging_trip_plots.R") 