rm(acc.data.list, seg.df.list.fixed.0.4) # to save memory
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
  df <- merge(df, bird.data[,c("birdID","sex")])   # add sex to the data
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
  df <- data.frame(df, habitats.to.add[,c('habitat','habitat_type','habitat_salinity','land_water')])
  # calculate duration of GPS-fixes, but before doing so, first order the df according to date_time (if this was not already done)
  df <- df[order(df$date_time),]
  df$time_to_previous <- c(NA, interval(df$date_time[1:(dim(df)[1]-1)], df$date_time[2:dim(df)[1]])%/%seconds(1))
  df$time_to_next <- c(df$time_to_previous[2:dim(df)[1]], NA)
  df$duration = round((df$time_to_previous+df$time_to_next)/2,2)/60 # to get the duration in decimal minutes
  n_hour <- aggregate(freq~year+month+yday_CEST+hour_CEST, df, sum)
  names(n_hour)[5] <- "fixes_per_hr"
  df<-merge(df, n_hour)
  df$speed_km=(df$speed_2d*60*60)/1000
  df <- as.data.frame(df)
  # only use data from the day after the catch date onward
  catch.date <- dmy(bird.data$catch.date[bird.data$birdID==birdID])
  df <- df[df$date_time>catch.date,]
  breeding.data.bird <- merge(unique(df[,c('birdID','year')]), breeding.data)
  # determine nest location and breeding phases for each year seperately:
  df$lat_rnd=round(df$latitude, digit=5) 
  df$lon_rnd=round(df$longitude, digit=6) 
  df<-df[order(df$date_time),]
  # determine breeding phases per bird per year (only if there is at least one record in breeding.data.bird):
  df.list.breeding.year <- list()
  if (dim(breeding.data.bird)[1]>0) for (j in 1:dim(breeding.data.bird)[1]) df.list.breeding.year[[j]] <- determine.breeding.phases(df=df[df$year==breeding.data.bird$year[j],], day_hatched=breeding.data.bird$hatchday[j], day_hatched2=breeding.data.bird$hatchday2[j], successful=breeding.data.bird$successful[j]) 
  if (length(df.list.breeding.year)>0) {
    df.breeding <- from.list.to.df(df.list.breeding.year)
    gps.breeding.data.list[[i]] <- df.breeding
  }
}

table(round(df$duration,1))
df.breeding.yday.durations <- aggregate(duration/60~yday_CEST+year, df.breeding, sum)
df.2013 <- df[df$year==2013,]
df.2013[1:20,c('date_time','time_to_previous','time_to_next','duration')]
table(round(df.2013$duration,1))
table(round(df.breeding.yday.durations$'duration/60',1))

# 656 and 1609 and 6282 are one and the same bird. These data will come together when changing from list to dataframe. However, as trackers were used in different years, it doesn't matter for determining nest locations and breeding phases
gps.breeding.data<-from.list.to.df(gps.breeding.data.list)