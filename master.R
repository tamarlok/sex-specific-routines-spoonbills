# code works with R version 4.1.3
rm(list=ls()) # clear workspace

# install.packages(c("mongolite", "RODBC", "rgdal", "raster", "rgeos", "RNCEP", "lme4"))
# install.packages(c("maptools","shapefiles","PBSmapping","sp","plotrix","adehabitatHR","lubridate","gdata","gplots","fossil","geosphere","randomForest","plyr","moments","nlme","MuMIn","emmeans"))


library(RODBC)  # installation on ymga fails
library(maptools)
library(shapefiles)
library(rgdal) # installation on ymga fails
library(PBSmapping)
library(sp)
library(plotrix)
library(adehabitatHR)
library(lubridate)
library(raster) # installation on ymga fails
library(gdata)
library(gplots)
library(rgeos) # installation on ymga fails
library(fossil)
library(RNCEP) # installation on ymga fails
library(geosphere)
# libraries to run the RF models:
library(randomForest)
library(plyr)
library(moments)
# statistical analysis
library(nlme)
library(lme4)# installation on ymga fails
library(car)
library(lmerTest)
library(MuMIn)
library(emmeans)
library(OpenStreetMap)
library(ggplot2)

# to work on the NIOZ server, set wd:
#setwd("/export/data01/user/tlok/Tamar/") # for ymga server
#setwd("/export/lv6/user/tlok/Tamar/") # for ada server
# remote session called "sexfor"

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

# PROCESS DATA
source("1_data_preparation/1_predicting_behaviour_from_acc_data.R") # in this script, a tmp file is saved with the summary statistics per 0.4 s ACC-segment still in it.
#save.image("data/processed/gps.acc.data.predicted.behaviour.1116.RData")
load("data/raw/gps.acc.data.2012-2019.RData")
source("1_data_preparation/2_determine_breeding_phases_and_link_habitat.R")
#save.image("data/processed/gps.breeding.data.0531.RData")
load("data/processed/gps.breeding.data.0531.RData")
load("data/processed/gps.acc.data.predicted.behaviour.1116.RData")
source("1_data_preparation/3_link_gps_breeding_data_with_predicted_behaviours.R") 
#save.image("data/processed/gps.breeding.data.behav.1116.RData")
# load("data/processed/gps.breeding.data.behav.0720.RData") # tables in the MS versions of <16 nov are still based on this datafile, in which the data of 656 in 2015 and 2016 is missing
load("data/processed/gps.breeding.data.behav.1116.RData")

# ANALYSE DATA
source("2_analyses/1_time_allocation_analysis_AIC_0719.R") 
#save.image("data/tmp/time_allocation_analysis_results_1116.RData")
load("data/tmp/time_allocation_analysis_results_1116.RData")
source("2_analyses/2_foraging_trip_analysis.R") # can only be run after time allocation script, as there, nest coordinates are determined. 

# VISUALIZE RESULTS 
source("3_visualisation/1_plot_data_on_breeding_phases_and_habitat_map.R") 
