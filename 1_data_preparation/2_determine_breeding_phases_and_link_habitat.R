rm(acc.data.list, seg.df.list.fixed.0.4) # to save memory
source("functions.R") # to have the most updated version of the functions

# TMP for NIOZ cluster
breeding.data <- read.csv("breeding.data.csv")
load("ShapeFileSchier.RData")
# END for NIOZ cluster

### link the gps data to habitat and breeding data, and determine nest and breeding phases ###
breeding.data <- read.csv("data/raw/breeding.data.csv")
breeding.data <- breeding.data[breeding.data$used==1,] # only select years with suitable data
breeding.data$hatchdate[breeding.data$hatchdate==""] <- NA
breeding.data$hatchday[is.na(breeding.data$hatchdate)==F]<-yday(dmy(breeding.data$hatchdate[is.na(breeding.data$hatchdate)==F]))
# link the gps.acc.data to habitat and calculate other variables. 
# load the habitat shapefile of Schiermonnikoog and surroundings, made by the RUG Geodienst 
load("data/raw/ShapeFileSchier.RData")
schier_new84_sel$habitat <- as.character(schier_new84_sel$Habitat)
schier_new84_sel$habitat[is.na(schier_new84_sel$habitat)] <- "unknown"
schier_new84_sel$habitat[schier_new84_sel$habitat=="Wadgeulen_Diep"|schier_new84_sel$habitat=="Wadgeulen_Ondiep"|schier_new84_sel$habitat=="Wadplaten"|schier_new84_sel$habitat=="Wad_Kweldergeul_Brak"]="waddenzee" 
schier_new84_sel$habitat[schier_new84_sel$habitat=="Schier_Kweldergeul_Brak"|schier_new84_sel$habitat=="Schier_Brak_Rest"]="Schier_brak"
schier_new84_sel$habitat[schier_new84_sel$habitat=="Wal_Zoet_Ondiep"|schier_new84_sel$habitat=="Wal_Zoet_Diep"]="wal_rest_zoet"
schier_new84_sel$habitat[schier_new84_sel$habitat=="Wal_Kwelder"|schier_new84_sel$habitat=="Wal_Land_Rest"|schier_new84_sel$habitat=="Wal_Moeras"]="wal_rest_land"
schier_new84_sel$habitat[schier_new84_sel$habitat=="LG_Land_Rest"|schier_new84_sel$habitat=="LG_Moeras"]="LM_land"
schier_new84_sel$habitat[schier_new84_sel$habitat=="LG_Zoet_Ondiep"|schier_new84_sel$habitat=="LG_Zoet_Diep"]="LM_zoet"

# other categorizations of habitat:
# categorize habitats as schier, mainland, wadden, or rest:
schier_new84_sel$habitat_type = 'schier'
schier_new84_sel$habitat_type[schier_new84_sel$habitat=='LM_zoet'|schier_new84_sel$habitat=='LM_land'|schier_new84_sel$habitat=='wal_rest_zoet'|schier_new84_sel$habitat=='wal_rest_land'] ='mainland'
schier_new84_sel$habitat_type[schier_new84_sel$habitat=='waddenzee'|schier_new84_sel$habitat=='Wad_Kweldergeul_Brak'] ='wadden'
schier_new84_sel$habitat_type[schier_new84_sel$habitat=='Eilanden_Rest'|schier_new84_sel$habitat=='Noordzee'] ='rest'
schier_new84_sel$habitat_type[is.na(schier_new84_sel$habitat)] = NA

# categorize habitats as marine, brackish, freshwater, land, or NA
schier_new84_sel$habitat_salinity = NA
schier_new84_sel$habitat_salinity[schier_new84_sel$habitat=='Schier_Kwelder'|schier_new84_sel$habitat=='wal_rest_land'|schier_new84_sel$habitat=='LM_land'|schier_new84_sel$habitat=='Schier_Land_Rest'|schier_new84_sel$habitat=='Eilanden_Rest'] = 'land'
schier_new84_sel$habitat_salinity[schier_new84_sel$habitat=='waddenzee'|schier_new84_sel$habitat=="Noordzee"] = 'marine'
schier_new84_sel$habitat_salinity[schier_new84_sel$habitat=='Schier_brak'|schier_new84_sel$habitat=='Wad_Kweldergeul_Brak'] = 'brackish'
schier_new84_sel$habitat_salinity[schier_new84_sel$habitat=='LM_zoet'|schier_new84_sel$habitat=='wal_rest_zoet'|schier_new84_sel$habitat=='Schier_Zoet'] = 'freshwater'

# categorize as land versus water
schier_new84_sel$land_water <- "land"
schier_new84_sel$land_water[schier_new84_sel$habitat%in%c("LM_zoet","Noordzee","Schier_brak","Schier_Zoet","waddenzee","wal_rest_zoet")] <- "water"

# determine nest location and breeding phases (distinguishing successful and unsuccessful post-breeding phases):
Sys.setenv(TZ='Europe/Amsterdam') # make sure that time zone is set to local time

gps.breeding.data.list <- list()

for (i in 1:length(gps.data.list)) {
  df <- gps.data.list[[i]]
  birdID <- names(gps.data.list)[i]
  print(birdID)
  df <- na.omit(df[df$latitude>53,])   # only select latitude > 53 (north of Sneek, the breeding area) and remove missing values
  df$birdID <- birdID
  df <- merge(df, unique(bird.data[,c("birdID","sex")]))   # the unique() is needed as 656 exists 3 times in the bird.data
  df$year <- year(df$date_time)
  df$yday_CEST <- yday(df$date_time)
  df$hour_CEST <- hour(df$date_time)  
  df$month <- month(df$date_time)
  df$week <- week(df$date_time)
  df$freq <- 1
  coordinates(df)  <- ~longitude + latitude  # to make it a spatialpoints dataframe
  proj4string(df) <- CRS(proj4string(schier_new84_sel))  # to give the same projection to df as the habitat file schier_new84_sel has
  # add dusk, dawn, day and night (which is location-specific, but varies very little between locations as they are very close):
  # Make df with dawn/sunrise and dusk/sunset for each day of the year at Schier
  df$sunset=sunriset(coordinates(df), df$date_time, POSIXct.out=TRUE, direction="sunset")[,2]
  df$sunrise=sunriset(coordinates(df), df$date_time, POSIXct.out=TRUE, direction="sunrise")[,2]
  df$daynight = ifelse(df$date_time<df$sunrise|df$date_time>df$sunset,"night","day")
  df$dusk=crepuscule(coordinates(df), df$date_time, solarDep = 6,POSIXct.out=TRUE, direction="dusk")[,2]
  df$dawn=crepuscule(coordinates(df), df$date_time, solarDep = 6, POSIXct.out=TRUE, direction="dawn")[,2]
  df$twilight_day_night = ifelse(df$date_time<df$dawn|df$date_time>df$dusk,"night","day")
  # add habitat
  habitats.to.add <- over(df, schier_new84_sel) 
  df <- data.frame(df, habitats.to.add[,c('habitat','habitat_type','habitat_salinity','land_water')]) # this function does not yet remove GPS points outside the habitat map, but gives NA's for the habitat columns.  
  # calculate duration of GPS-fixes, but before doing so, first order the df according to date_time (if this was not already done)
  df[is.na(df$habitat),c('habitat','habitat_type','habitat_salinity','land_water')] <- "unknown" # replace habitat of locations outside habitat with "unknown" (so they are not deleted within the determine.breeding.phase function)
  df <- as.data.frame(df)
  df <- df[order(df$date_time),]
  df$time_to_previous <- c(NA, interval(df$date_time[1:(dim(df)[1]-1)], df$date_time[2:dim(df)[1]])%/%seconds(1))
  df$time_to_next <- c(df$time_to_previous[2:dim(df)[1]], NA)
  df$duration = round((df$time_to_previous+df$time_to_next)/2,2)/60 # to get the duration in decimal minutes
  dur.day.year <- aggregate(duration/60~yday_CEST+year, df, sum)
  df <- df[df$duration<61,] # this is also done in the determine.breeding.phases function, but here we can more easily check what the consequences are
  dur.day.year <- aggregate(duration/60~yday_CEST+year, df, sum)
  table(round(df$duration,0))
  n_hour <- aggregate(freq~year+month+yday_CEST+hour_CEST, df, sum)
  names(n_hour)[5] <- "fixes_per_hr"
  df<-merge(df, n_hour)
  df$speed_km=(df$speed_2d*60*60)/1000
  # only use data from the day after the catch date onward
  catch.date <- bird.data[bird.data$birdID==birdID,c('logger','catch.date')]
  df <- merge(df, catch.date, by.x='device_info_serial', by.y='logger', all.x=T)
  df$catch.date <- dmy(df$catch.date)
  df <- df[df$date_time>df$catch.date,]
  breeding.data.bird <- merge(unique(df[,c('birdID','year')]), breeding.data)
  # determine nest location and breeding phases for each year seperately:
  df<-df[order(df$date_time),]
  # determine breeding phases per bird per year (only if there is at least one record in breeding.data.bird):
  df.list.breeding.year <- list()
  df.all=df # as a back-up to check the determine.breeding.phases function, can be removed later
  if (dim(breeding.data.bird)[1]>0) for (j in 1:dim(breeding.data.bird)[1]) df.list.breeding.year[[j]] <- determine.breeding.phases(df=df[df$year==breeding.data.bird$year[j],], day_hatched=breeding.data.bird$hatchday[j], day_hatched2=breeding.data.bird$hatchday2[j], successful=breeding.data.bird$successful[j]) 
  if (length(df.list.breeding.year)>0) {
    df.breeding <- from.list.to.df(df.list.breeding.year)
    gps.breeding.data.list[[i]] <- df.breeding
    names(gps.breeding.data.list)[i]<-birdID
  }
}

save.image("data/processed/gps.breeding.data.tmp.RData") # to have this saved in case R freezes...
load("data/processed/gps.breeding.data.tmp.RData")

# data check:
names(gps.breeding.data.list)
missing.data.656.2013 <- gps.breeding.data.list[[12]][gps.breeding.data.list[[12]]$birdID==656&gps.breeding.data.list[[12]]$year==2013&gps.breeding.data.list[[12]]$yday_CEST>135&gps.breeding.data.list[[12]]$yday_CEST<140,]


names(gps.breeding.data.list)
table(gps.breeding.data.list[[17]]$nest1, gps.breeding.data.list[[17]]$year)
# 656 and 1609 and 6282 are one and the same bird. These data will come together when changing from list to dataframe. However, as trackers were used in different years, it doesn't matter for determining nest locations and breeding phases
gps.breeding.data<-from.list.to.df(gps.breeding.data.list)

# additional calculations on gps.breeding.data
# breeding.phase 'breeding' was determined in a separate column. 
# replace NA values in the breeding.phase column with the value in the breeding column
gps.breeding.data$breeding.phase[is.na(gps.breeding.data$breeding.phase)]<-gps.breeding.data$breeding[is.na(gps.breeding.data$breeding.phase)]
table(gps.breeding.data$birdID, gps.breeding.data$year)
table(gps.breeding.data$breeding.phase) # somehow, no 'breeding' was classified... 

## numbering breeding phases:
gps.breeding.data$breeding.phase.nr = 1 # pre-breeding phase
gps.breeding.data$breeding.phase.nr[gps.breeding.data$breeding.phase=='breeding'] = 2
gps.breeding.data$breeding.phase.nr[gps.breeding.data$breeding.phase=='eggs'] = 3
gps.breeding.data$breeding.phase.nr[gps.breeding.data$breeding.phase=='chicks'] = 4
gps.breeding.data$breeding.phase.nr[gps.breeding.data$breeding.phase=='post.breeding.successful'] = 5
gps.breeding.data$breeding.phase.nr[gps.breeding.data$breeding.phase=='post.breeding.unsuccessful'] = 6
gps.breeding.data$breeding.phase = paste(gps.breeding.data$breeding.phase.nr, gps.breeding.data$breeding.phase, sep='.')

## add columns with week relative to the hatching day.
## change the attempt number of the pre-breeding phase, to make it the same attempt as the subsequent breeding attempt:
gps.breeding.data$attempt[gps.breeding.data$breeding.phase=='1.pre-breeding'] = gps.breeding.data$attempt[gps.breeding.data$breeding.phase=='1.pre-breeding']+1
gps.breeding.data = gps.breeding.data[order(gps.breeding.data$year, gps.breeding.data$birdID, gps.breeding.data$yday_CEST),]
ydays.year.bird.phase.all = unique(gps.breeding.data[,c('year','birdID','sex','breeding.phase.nr','attempt','yday_CEST','week')])
dim(ydays.year.bird.phase.all) 

yday.min.year.bird.phase = aggregate(yday_CEST~birdID+sex+breeding.phase.nr+year+attempt, data=ydays.year.bird.phase.all, min)
hatch_day_bird_year = yday.min.year.bird.phase[yday.min.year.bird.phase$breeding.phase.nr==4,c('birdID','year','attempt','yday_CEST')]
names(hatch_day_bird_year)[4]='hatch_day'
gps.breeding.data = merge(gps.breeding.data, hatch_day_bird_year, all.x=T)
unique(gps.breeding.data$hatch_day)
gps.breeding.data$day_rel_hatching = gps.breeding.data$yday_CEST-gps.breeding.data$hatch_day
gps.breeding.data$week_rel_hatching = floor(gps.breeding.data$day_rel_hatching/7)
table(gps.breeding.data$birdID, gps.breeding.data$year)

rm(gps.data.list, gps.breeding.data.list, schier_new84_sel)
