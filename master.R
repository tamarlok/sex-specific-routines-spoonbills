# code works with R version 4.1.3
rm(list=ls()) # clear workspace

# import and export tracking data:
library(RODBC) 
library(move)

library(dplyr)
library(lubridate)
library(devtools)
library(plotrix)

library(sp)
library(suntools)
library(gdata)

# libraries to run the RF models:
library(randomForest)
library(plyr)
library(moments)

# statistical analysis
library(MuMIn)
library(lme4)
library(DHARMa)
library(emmeans)
library(glmmTMB)
library(RMark)
library(mgcv) 
library(gamm4)

# visualisation
library(mapview)
library(sp)

library(nlme)
library(OpenStreetMap)
library(ggplot2)
library(png)
library(DescTools)

### Set system time zone 
Sys.setenv(TZ="GMT")
###

source("functions.R")
set.seed(3) # to be able to reproduce random processes (though the order of running the different codes should then be exactly the same)

# data preparation and visualization
#source("0_load_tracking_data_from_uvabits_database_and_export_to_movebank.R") # original downloading script; for this, the time zone should still be set at GMT/UTC
#source("0_retrieve_tracking_data_from_already_downloaded_files.R") # retrieves the same data from previously downloaded files and arranges it in the same order
#save.image("data/raw/gps.acc.data.2012-2019.RData")
load("data/raw/gps.acc.data.2012-2019.RData")

# change system time zone to CEST (local summer time = GMT/UTC + 2)
Sys.setenv(TZ='Europe/Amsterdam')

# PROCESS DATA
source("1_data_preparation/1_predicting_behaviour_from_acc_data.R")
source("1_data_preparation/2_determine_breeding_phases_and_link_habitat.R")
source("1_data_preparation/3_link_gps_(pre)breeding_data_with_predicted_behaviours_and_continuous_tide.R") 
source("1_data_preparation/4_further_data_selection_procedures.R") # steps 3 and 4 should ideally be combined, but seperating them makes things more flexible and easy to adjust with respect to data selection decisions at this stage.
#save.image("data/tmp/gps.data.behav.final.1129.RData")
load("data/tmp/gps.data.behav.final.1129.RData")

# ANALYSE DATA
source("2_analyses/1_time_allocation_analysis.R") 
#save.image("data/tmp/time.allocation.results.0206.RData")
load("data/tmp/time.allocation.results.0206.RData")
source("2_analyses/2_foraging_trip_analysis.R")
source("3_analyses/3_performance_tagged_vs_untagged_birds.R")

# VISUALIZE RESULTS 
source("3_visualisation/1_plot_data_on_breeding_phases_and_habitat_map.R") 
source("3_visualisation/2_time_allocation_plots.R") 
source("3_visualisation/3_foraging_trip_plots.R") 