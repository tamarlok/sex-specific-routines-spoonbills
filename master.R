# code works with R version 4.1.3
rm(list=ls()) # clear workspace

library(RODBC) 
library(maptools)
library(shapefiles)
library(rgdal)
library(PBSmapping)
library(sp)
library(plotrix)
library(adehabitatHR)
library(lubridate)
library(raster)
library(gdata)
library(gplots)
library(rgeos)
library(fossil)
library(RNCEP)
library(geosphere)
# libraries to run the RF models:
library(randomForest)
library(plyr)
library(moments)
# statistical analysis
library(nlme)
library(lme4)
library(MuMIn)
library(emmeans)

# to work on the NIOZ server, set wd:
#setwd("/export/data01/user/tlok/Tamar/")

# in case of different functions with the same name used in different packages, call the function as package::function 

### Set system time zone 
Sys.setenv(TZ="GMT")
#Sys.setenv(TZ='Europe/Amsterdam') # use this to plot in GMT+2 = NL summer time start=  [1] "2013-05-01 06:14:10 CEST"
###

source("functions.R")
set.seed(3) # to be able to reproduce random processes (though the order of running the different codes should then be exactly the same)

# data preparation and visualization
#source("0_load_tracking_data_from_uvabits_database.R") # original downloading script; for this, the time zone should still be set at GMT/UTC
#source("0_retrieve_tracking_data_from_already_downloaded_files.R") # retrieves the same data from previously downloaded files and arranges it in the same order
#save.image("data/raw/gps.acc.data.2012-2019.RData")
load("data/raw/gps.acc.data.2012-2019.RData") # the correct bird.data is not yet saved in this file (on 8.4.2022)

# change system time zone to CEST (local summer time = GMT/UTC + 2)
Sys.setenv(TZ='Europe/Amsterdam')

source("1_data_preparation/1_predicting_behaviour_from_acc_data.R")
#save.image("data/processed/gps.acc.data.predicted.behaviour.2104.RData")
load("data/raw/gps.acc.data.2012-2019.RData")
source("1_data_preparation/2_determine_breeding_phases_and_link_habitat.R")
#save.image("data/processed/gps.breeding.data.0530.RData")
load("data/processed/gps.breeding.data.0530.RData")
load("data/processed/gps.acc.data.predicted.behaviour.2104.RData")
source("1_data_preparation/3_link_gps_breeding_data_with_predicted_behaviours.R") 
#save.image("data/processed/gps.breeding.data.behav.0530.RData")
load("data/processed/gps.breeding.data.behav.0530.RData")
# analyse the data
source("2_analyses/1_time_allocation_analysis.R") 
source("2_analyses/2_foraging_trip_analysis.R") 

# visualize the results 
source("3_visualisation/1_plot_data_on_breeding_phases_and_habitat_map.R") 
