### link the gps data to habitat and breeding data, and determine nest and breeding phases ###
breeding.data <- read.csv("data/raw/breeding.data.csv") # file deposited on Dryad
breeding.data <- breeding.data[breeding.data$used==1,] # only select birdyears with suitable and reliable data

# link the gps.acc.data to habitat and calculate other variables. 
# load the habitat shapefile of Schiermonnikoog and surroundings, made by the RUG Geodienst:
schier_new84_sel <- readOGR(dsn = "data/raw/study_area_shapefile/study_area.shp") # files deposited on Dryad
schier_new84_sel$habitat <- as.character(schier_new84_sel$Habitat)
schier_new84_sel$habitat[is.na(schier_new84_sel$habitat)] <- "unknown"
schier_new84_sel$habitat[schier_new84_sel$habitat=="Wadgeulen_Diep"|schier_new84_sel$habitat=="Wadgeulen_Ondiep"|schier_new84_sel$habitat=="Wadplaten"|schier_new84_sel$habitat=="Wad_Kweldergeul_Brak"]="waddenzee" 
schier_new84_sel$habitat[schier_new84_sel$habitat=="Schier_Kweldergeul_Brak"|schier_new84_sel$habitat=="Schier_Brak_Rest"]="Schier_brak"
schier_new84_sel$habitat[schier_new84_sel$habitat=="Wal_Zoet_Ondiep"|schier_new84_sel$habitat=="Wal_Zoet_Diep"]="wal_rest_zoet"
schier_new84_sel$habitat[schier_new84_sel$habitat=="Wal_Kwelder"|schier_new84_sel$habitat=="Wal_Land_Rest"|schier_new84_sel$habitat=="Wal_Moeras"]="wal_rest_land"
schier_new84_sel$habitat[schier_new84_sel$habitat=="LG_Land_Rest"|schier_new84_sel$habitat=="LG_Moeras"]="LM_land"
schier_new84_sel$habitat[schier_new84_sel$habitat=="LG_Zoet_Ondiep"|schier_new84_sel$habitat=="LG_Zoet_Diep"]="LM_zoet"

# determine nest location and breeding phases (distinguishing successful and unsuccessful post-breeding phases):
Sys.setenv(TZ='Europe/Amsterdam') # make sure that time zone is set to local time

gps.breeding.data.behav.list <- list()

for (i in 1:length(gps.behav.data.list)) {
  df <- na.omit(gps.behav.data.list[[i]]) # remove missing values
  df$date_time_CEST <- with_tz(df$date_time, tzone="Europe/Amsterdam") 
  birdID <- df$birdID[1]
  df <- merge(df, unique(bird.data[,c("birdID","sex")])) # the unique() is needed as 656 exists 3 times in the bird.data
  df$year <- year(df$date_time_CEST)
  df$yday_CEST <- yday(df$date_time_CEST)
  df$hour_CEST <- hour(df$date_time_CEST)  
  df$month <- month(df$date_time_CEST)
  df$freq <- 1
  coordinates(df)  <- ~longitude + latitude  # to make it a spatialpoints dataframe
  proj4string(df) <- CRS(proj4string(schier_new84_sel))  # to give the same projection to df as the habitat file schier_new84_sel has
  # add habitat
  habitats.to.add <- over(df, schier_new84_sel) 
  df <- data.frame(df, habitats.to.add[,c('habitat')]) # this function does not yet remove GPS points outside the habitat map, but gives NA's for the habitat columns in those cases  
  df$habitat[is.na(df$habitat)] <- "unknown" # replace habitat of locations outside habitat with "unknown"
  # calculate duration of GPS-fixes, but before doing so, first order the df according to date_time (if this was not already done)
  df <- as.data.frame(df)
  df <- df[order(df$date_time_CEST),]
  df$time_to_previous <- c(NA, interval(df$date_time_CEST[1:(dim(df)[1]-1)], df$date_time_CEST[2:dim(df)[1]])%/%seconds(1))
  df$time_to_next <- c(df$time_to_previous[2:dim(df)[1]], NA)
  df$duration = round((df$time_to_previous+df$time_to_next)/2,2)/60 # to get the duration in decimal minutes
  df <- df[df$duration<61,] # select durations of GPS fixes of max 60 minutes to determine breeding phases.  
  # only use data from the day after the catch date onward
  catch.date <- bird.data[bird.data$birdID==birdID,c('birdID','tagID','start_deployment')]
  df <- merge(df, catch.date, by=c('birdID','tagID'), all.x=T)
  df <- df[floor_date(df$date_time_CEST, 'days')>floor_date(df$start_deployment, 'days'),] 
  breeding.data.bird <- merge(unique(df[,c('birdID','year')]), breeding.data)
  # determine nest location and breeding phases for each year seperately:
  df<-df[order(df$date_time_CEST),]
  # determine breeding phases per bird per year (only if there is at least one record in breeding.data.bird):
  df.list.breeding.year <- list()

  if (dim(breeding.data.bird)[1]>0) for (j in 1:dim(breeding.data.bird)[1]) df.list.breeding.year[[j]] <- determine.breeding.phases(df=df[df$year==breeding.data.bird$year[j],], day_hatched=breeding.data.bird$hatchday[j], day_hatched2=breeding.data.bird$hatchday2[j], successful=breeding.data.bird$successful[j]) 
  
  # combine data from different years from the same bird into a single list item
  if (length(df.list.breeding.year)>0) {
    df.breeding <- from.list.to.df(df.list.breeding.year)
    gps.breeding.data.behav.list[[i]] <- df.breeding
  }
}

# change from list to df:
gps.breeding.data.behav<-from.list.to.df(gps.breeding.data.behav.list)

# additional calculations on gps.breeding.data.behav
# replace NA values in the breeding.phase column with the value in the breeding column
gps.breeding.data.behav$breeding.phase[is.na(gps.breeding.data.behav$breeding.phase)]<-gps.breeding.data.behav$breeding[is.na(gps.breeding.data.behav$breeding.phase)]

## numbering breeding phases:
gps.breeding.data.behav$breeding.phase.nr = 1 # pre-breeding phase
gps.breeding.data.behav$breeding.phase.nr[gps.breeding.data.behav$breeding.phase=='breeding'] = 2
gps.breeding.data.behav$breeding.phase.nr[gps.breeding.data.behav$breeding.phase=='eggs'] = 3
gps.breeding.data.behav$breeding.phase.nr[gps.breeding.data.behav$breeding.phase=='chicks'] = 4
gps.breeding.data.behav$breeding.phase.nr[gps.breeding.data.behav$breeding.phase=='post.breeding.successful'] = 5
gps.breeding.data.behav$breeding.phase.nr[gps.breeding.data.behav$breeding.phase=='post.breeding.unsuccessful'] = 6
gps.breeding.data.behav$breeding.phase = paste(gps.breeding.data.behav$breeding.phase.nr, gps.breeding.data.behav$breeding.phase, sep='.')

gps.breeding.data.behav = gps.breeding.data.behav[order(gps.breeding.data.behav$year, gps.breeding.data.behav$birdID, gps.breeding.data.behav$yday_CEST),]
ydays.year.bird.phase.all = unique(gps.breeding.data.behav[,c('year','birdID','sex','breeding.phase','breeding.phase.nr','attempt','yday_CEST','week')])
dim(ydays.year.bird.phase.all) 

yday.min.year.bird.phase = aggregate(yday_CEST~year+attempt+birdID+sex+breeding.phase+breeding.phase.nr, data=ydays.year.bird.phase.all, min)
yday.min.year.bird.phase = yday.min.year.bird.phase[order(yday.min.year.bird.phase$year, yday.min.year.bird.phase$birdID, yday.min.year.bird.phase$attempt, yday.min.year.bird.phase$breeding.phase.nr),]

# change NA into unknown behaviour:
gps.breeding.data.behav$behaviour[is.na(gps.breeding.data.behav$behaviour)] <- "unknown"
gps.breeding.data.behav$speed_2d = round(gps.breeding.data.behav$speed_2d,2)

## Determine the total duration recorded per day, and remove the days with less than 23.1 hours of data (this can happen, as in a previous step we deleted points with a duration of more than 60 minutes, and in some cases, behaviour was not predicted as no acc-data were collected)
duration_yday_birdID_year_all <- aggregate(duration/60~year+birdID+sex+yday_CEST+week+breeding.phase+breeding.phase.nr, gps.breeding.data.behav, sum) 
dim(duration_yday_birdID_year_all) # 5663 birddays
names(duration_yday_birdID_year_all)[8] = 'dur_per_yday'
distr.duration <- table(round(duration_yday_birdID_year_all$dur_per_yday,0), duration_yday_birdID_year_all$birdID)
distr.duration # when linked with behaviour estimated from acc-data, total duration per day is often less than 23.8 hours, as regularly, an acc-sample is not measured. 

# the nest attempt determination went wrong for 6289 in 2017, as the second breeding attempt was not monitored and therefore, hatching date was unknown. Therefore, the 'pre-breeding phase' of the second breeding attempt is too long, as the bird is actually breeding again during part of this period. And the post-fledging phase is therefore defined as the 3rd attempt. Remove this pre-breeding phase from the analysis:
gps.breeding.data.behav$breeding.phase[gps.breeding.data.behav$birdID==6289 & gps.breeding.data.behav$year==2017 & gps.breeding.data.behav$attempt==2 & gps.breeding.data.behav$breeding.phase=='1.pre-breeding'] = NA
# and make the 3rd attempt the 2nd:
gps.breeding.data.behav$attempt[gps.breeding.data.behav$birdID==6289 & gps.breeding.data.behav$year==2017 & gps.breeding.data.behav$attempt==3] = 2

# assign nest locations to attempts: 
nest.locations.bird.year <- aggregate(cbind(nest1,nest2)~year+birdID+attempt+lat.nest1+lon.nest1+lat.nest2+lon.nest2+nest1.real+nest2.real,gps.breeding.data.behav,sum)
nest.locations.bird.year = nest.locations.bird.year[order(nest.locations.bird.year$year, nest.locations.bird.year$birdID, nest.locations.bird.year$attempt),]
# when nest1>nest2 (in this case, nest1 is always a real nesting attempt)
nest.locations.bird.year[nest.locations.bird.year$nest1>nest.locations.bird.year$nest2,c('lat.nest','lon.nest')] <- nest.locations.bird.year[nest.locations.bird.year$nest1>nest.locations.bird.year$nest2,c('lat.nest1','lon.nest1')]
# when nest2>nest1 and nest2 was a real nesting attempt: 
nest.locations.bird.year[nest.locations.bird.year$nest2>nest.locations.bird.year$nest1 & nest.locations.bird.year$nest2.real==1,c('lat.nest','lon.nest')] <- nest.locations.bird.year[nest.locations.bird.year$nest2>nest.locations.bird.year$nest1 & nest.locations.bird.year$nest2.real==1,c('lat.nest2','lon.nest2')]

# during the time allocation analysis, it turned out that the nest (attempt) assignment went wrong for 6288 and 6289 in 2017. The reason for 6288 was that the transmitter failed shortly after the real breeding attempt, therefore producing more locations at an earlier unsuccessful settlement. The reason for 6289 was that she had two breeding attempts with similar amounts of data, and with unknown hatchdate for the 2nd breeding attempt. Moreover, as 656 had two trackers in 2015, during the same breeding attempt, only one row (of the two) for 656 in 2015 should be selected. 
nest.locations.bird.year[nest.locations.bird.year$year==2017 & nest.locations.bird.year$birdID==6288,c('lat.nest','lon.nest')] <- nest.locations.bird.year[nest.locations.bird.year$year==2017 & nest.locations.bird.year$birdID==6288,c('lat.nest2','lon.nest2')] 
nest.locations.bird.year[nest.locations.bird.year$year==2017 & nest.locations.bird.year$birdID==6289,c('lat.nest','lon.nest')] <- nest.locations.bird.year[nest.locations.bird.year$year==2017 & nest.locations.bird.year$birdID==6289,c('lat.nest1','lon.nest1')] 

# add lat.nest and lon.nest to gps.breeding.data.behav
gps.breeding.data.behav <- merge(gps.breeding.data.behav, nest.locations.bird.year[,c('birdID','year','attempt','lat.nest','lon.nest')])

gps.breeding.data.behav = gps.breeding.data.behav[order(gps.breeding.data.behav$year, gps.breeding.data.behav$birdID, gps.breeding.data.behav$attempt, gps.breeding.data.behav$breeding.phase.nr),]
unique(gps.breeding.data.behav[,c('year','birdID','attempt','breeding.phase','lat.nest','lon.nest')])

# calculate distance to nest:
gps.breeding.data.behav$distance.to.nest <- NA
gps.breeding.data.behav$distance.to.nest[is.na(gps.breeding.data.behav$lat.nest)==F] <- round(distCosine(gps.breeding.data.behav[is.na(gps.breeding.data.behav$lat.nest)==F,c('longitude','latitude')], gps.breeding.data.behav[is.na(gps.breeding.data.behav$lat.nest)==F,c('lon.nest','lat.nest')], r=6378137),3) # gives the distance in meters between nest location and actual position of the bird; can only be calculated when lat.nest (and lon.nest) is known.

# link to tide data:
tide.data <- read.table("data/raw/getij_schiermonnikoog_2010-2019.txt", skip=4)
names(tide.data) <- c("date","time","lowhigh", "waterheight")
tide.data$date_time <- dmy_hm(paste(tide.data$date, tide.data$time, sep=" "))
tide.data$date_time_wintertime <- force_tz(tide.data$date_time, tzone="Etc/GMT-1")
tide.data$date_time_UTC <- with_tz(tide.data$date_time_wintertime, tzone="UTC")
tide.data$date_time_CEST <- with_tz(tide.data$date_time_wintertime, tzone="Europe/Amsterdam")
tide.data$date_hour_CEST <- round_date(tide.data$date_time_CEST, "hour")
tide.data$yday_CEST <- yday(tide.data$date_time_CEST)
tide.data$hour_CEST <- hour(tide.data$date_time_CEST)
tide.data$min_CEST <- minute(tide.data$date_time_CEST)
tide.data$year <- year(tide.data$date_time_CEST)
tide.data$day_min <- tide.data$hour_CEST*60+tide.data$min_CEST
tide.data$lowhigh[tide.data$lowhigh==1]<-"high"
tide.data$lowhigh[tide.data$lowhigh==2]<-"low"
high.tides <- tide.data[tide.data$lowhigh=="high",c("year","yday_CEST", "waterheight","date_hour_CEST","day_min")]
names(high.tides)[4]="date_hour_high"
low.tides <- tide.data[tide.data$lowhigh=="low",c("year","yday_CEST", "waterheight","date_hour_CEST","day_min")]
names(low.tides)[4]="date_hour_low"
tide.data <- tide.data[tide.data$year>2011,]

# determine time until high tide per unique date_hour_30min_CEST:
all.dates <- seq(ymd_h("2013-01-01 00"), ymd_h("2019-12-31 00"), by = "days")
df_date_time_30min <- expand.grid(date=all.dates, hour=0:23, minute=c(0,30))
df_date_time_30min$date_time_30min <- df_date_time_30min$date+3600*df_date_time_30min$hour+60*df_date_time_30min$minute
df_date_time_30min <- df_date_time_30min[order(df_date_time_30min$date_time_30min),]

tide.data$date_time_30min <- floor_date(tide.data$date_time_UTC, unit = "30 mins")

# make sure tide data is ordered before using the findInterval function:
tide.data <- tide.data[order(tide.data$date_time_30min),]
tide.data$time_to_next_tide<-NA
tide.data$time_to_next_tide[1:(dim(tide.data)[1]-1)] <- tide.data$date_time_30min[2:dim(tide.data)[1]]-tide.data$date_time_30min[1:(dim(tide.data)[1]-1)]
tide.data$time_to_previous_tide[2:dim(tide.data)[1]] <- tide.data$time_to_next_tide[1:(dim(tide.data)[1]-1)]

df_date_time_30min = merge(df_date_time_30min[,c('date','date_time_30min')], tide.data[,c('date_time_30min','lowhigh','time_to_next_tide','time_to_previous_tide')], by="date_time_30min", all.x=T)

# determine tidal stage in radians, where 0/2pi is high tide and pi is low tide.
df_date_time_30min$tidal_stage_rad <- NA
start.row <- min(which(is.na(df_date_time_30min$time_to_next_tide)==F))
last.row <- max(which(is.na(df_date_time_30min$time_to_next_tide)==F))
for(i in start.row:last.row) {
  if (is.na(df_date_time_30min$lowhigh[i])==F & df_date_time_30min$lowhigh[i]=='high') df_date_time_30min$tidal_stage_rad[i] = 0
  if (is.na(df_date_time_30min$lowhigh[i])==F & df_date_time_30min$lowhigh[i]=='low') df_date_time_30min$tidal_stage_rad[i] = pi
  if (is.na(df_date_time_30min$lowhigh[i]) & df_date_time_30min$tidal_stage_rad[i-1]<2*pi) {
    df_date_time_30min$time_to_next_tide[i] = df_date_time_30min$time_to_next_tide[i-1]
    df_date_time_30min$tidal_stage_rad[i] = df_date_time_30min$tidal_stage_rad[i-1]+(0.5/df_date_time_30min$time_to_next_tide[i])*pi
  }
}

df_date_time_30min = df_date_time_30min[,c('date','date_time_30min','lowhigh','tidal_stage_rad')]

# first change UTC into CEST before linking to gps data:
df_date_time_30min$date_time_30min_CEST <- with_tz(df_date_time_30min$date_time_30min, tz="Europe/Amsterdam")
df_date_time_30min$diel_rad = (hour(df_date_time_30min$date_time_30min_CEST)*60+minute(df_date_time_30min$date_time_30min_CEST))/1440*2*pi
gps.breeding.data.behav$date_time_30min_CEST <- floor_date(gps.breeding.data.behav$date_time_CEST, '30min')
gps.breeding.data.behav = merge(gps.breeding.data.behav, df_date_time_30min, by='date_time_30min_CEST')
gps.breeding.data.behav = gps.breeding.data.behav[order(gps.breeding.data.behav$birdID, gps.breeding.data.behav$date_time_CEST),]

# determine tidal_phase based on tidal_stage_rad, equally dividing the cycle into four phases, where 0 +/- 1/4pi = high, 1/4pi - 3/4pi = outgoing, 3/4pi-1.25pi = low, 1.25-1.75pi=incoming:
gps.breeding.data.behav$tidal_stage_cat = 'high'
gps.breeding.data.behav$tidal_stage_cat[gps.breeding.data.behav$tidal_stage_rad>=0.25*pi & gps.breeding.data.behav$tidal_stage_rad<0.75*pi] = 'outgoing'
gps.breeding.data.behav$tidal_stage_cat[gps.breeding.data.behav$tidal_stage_rad>=0.75*pi & gps.breeding.data.behav$tidal_stage_rad<1.25*pi] = 'low'
gps.breeding.data.behav$tidal_stage_cat[gps.breeding.data.behav$tidal_stage_rad>=1.25*pi & gps.breeding.data.behav$tidal_stage_rad<1.75*pi] = 'incoming'

# check the attempts and nest locations per bird per year:
nest.location.bird.year.attempt = unique(gps.breeding.data.behav[,c('year','birdID','attempt','lat.nest','lon.nest')])
nest.location.bird.year.attempt = nest.location.bird.year.attempt[order(nest.location.bird.year.attempt$year, nest.location.bird.year.attempt$birdID, nest.location.bird.year.attempt$attempt),]

# data of 763 are removed from 2015 onward, as GPS locations are very inaccurate (which is visible from the wide spread of locations around the nest location, and corresponding low nest attendance)
gps.breeding.data.behav$exclude <- 0
gps.breeding.data.behav$exclude[gps.breeding.data.behav$birdID==763&(gps.breeding.data.behav$year>=2015)] <- 1
gps.breeding.data.behav <- gps.breeding.data.behav[gps.breeding.data.behav$exclude==0,]
# check breeding phases: only include pre-breeding from the day the bird first visited the Oosterkwelder in a particular year. This means that the column 'breeding' says 'breeding' (and not 'pre-breeding'; which is a different definition from pre-breeding than in the category breeding.phase).
gps.breeding.data.behav$breeding.phase[gps.breeding.data.behav$breeding=='pre-breeding'] = NA

# remove cases where breeding.phase=NA
gps.breeding.data.behav = gps.breeding.data.behav[!is.na(gps.breeding.data.behav$breeding.phase),]
# remove cases where behaviour was not predicted (no acceleration data available):
gps.breeding.data.behav = gps.breeding.data.behav[gps.breeding.data.behav$behaviour!='unknown',]
# remove cases where habitat was unknown (because outside habitat map):
gps.breeding.data.behav = gps.breeding.data.behav[gps.breeding.data.behav$habitat!='unknown',]

# Include Noordzee-locations as marine habitat when foraging, and Schier/island when resting.  
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$habitat=='Noordzee' & gps.breeding.data.behav$behaviour2 =='resting_rest']<-'resting_schier'

# check where the locations on Eilanden_Rest are:
gps.breeding.data.rest = gps.breeding.data.behav[gps.breeding.data.behav$habitat=='Eilanden_Rest',]
gps.breeding.data.rest$behav.nr <- 1
gps.breeding.data.rest$behav.nr[gps.breeding.data.rest$behaviour=='foraging'] <- 2
gps.breeding.data.rest$behav.nr[gps.breeding.data.rest$behaviour=='flying'] <- 3
coordinates(gps.breeding.data.rest) <- c("longitude","latitude")
proj4string(gps.breeding.data.rest) <- CRS("+proj=longlat +datum=WGS84")
mapview(gps.breeding.data.rest, zcol="behaviour", col.regions=c('red','blue','grey','green')) # locations on saltmarsh of Ameland and in gully on Rottumerplaat 
# include Eilanden_Rest as 'foraging_brackish' when foraging, and as 'resting_wadden' when resting
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$habitat=='Eilanden_Rest' & gps.breeding.data.behav$behaviour =='resting']<-'resting_wadden'
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$habitat=='Eilanden_Rest' & gps.breeding.data.behav$behaviour =='foraging']<-'foraging_brackish'
table(gps.breeding.data.behav$behaviour2, gps.breeding.data.behav$habitat)

# check the sampling frequency per bird per year per yday
data.overview = table(gps.breeding.data.behav$yday_CEST, gps.breeding.data.behav$birdID, gps.breeding.data.behav$year)
data.overview = as.data.frame(data.overview)
names(data.overview) = c('yday','birdID','year','freq')
data.overview = data.overview[data.overview$freq>0,]
write.csv(data.overview, 'data/processed/data.overview.csv')

# subsample data so that one sample per half hour is retained:
# to make sure that the first sample per half hour is retained, first order data by date_time
gps.breeding.data.behav <- gps.breeding.data.behav[order(gps.breeding.data.behav$birdID, gps.breeding.data.behav$date_time_CEST),]
gps.breeding.data.behav <- gps.breeding.data.behav %>%
  mutate(time_interval = floor_date(date_time_CEST, unit = "30 mins"))

gps.breeding.data.behav.30min <- gps.breeding.data.behav %>%
  group_by(time_interval, year, birdID, yday_CEST) %>%
  slice(1)

# how often are different behaviours assigned:
sample.sizes.behaviours = table(gps.breeding.data.behav.30min$behaviour)
sample.sizes.behaviours['other']/sum(sample.sizes.behaviours) # 5.0% was estimated as 'other' behaviour.

# number of samples per individual per day after down-sampling to 30 minute interval:
N_yday_birdID_year <- aggregate(freq~year+birdID+sex+yday_CEST+week+breeding.phase+breeding.phase.nr+attempt+day_rel_hatching+week_rel_hatching, gps.breeding.data.behav.30min[gps.breeding.data.behav.30min$breeding.phase.nr!=2,], sum) 

# only use the last 30 days of the pre-breeding phase. For this, we first determine the last day of pre-breeding per bird/year/attempt: 
last.prebreeding.day = aggregate(yday_CEST~birdID+year+attempt, N_yday_birdID_year[N_yday_birdID_year$breeding.phase=='1.pre-breeding',], max)
names(last.prebreeding.day)[4]='last.prebreeding.day'
N_yday_birdID_year = merge(N_yday_birdID_year, last.prebreeding.day, all.x=T)
N_yday_birdID_year$days_since_end_prebreeding = N_yday_birdID_year$yday_CEST-N_yday_birdID_year$last.prebreeding.day

# only use the first 30 days of the post-breeding phase. For this, we first determine the first day of post-breeding per bird/year/attempt: 
first.postbreeding.day = aggregate(yday_CEST~birdID+year+attempt, N_yday_birdID_year[N_yday_birdID_year$breeding.phase%in%c('5.post.breeding.successful','6.post.breeding.unsuccessful'),], min)
names(first.postbreeding.day)[4]='first.postbreeding.day'
N_yday_birdID_year = merge(N_yday_birdID_year, first.postbreeding.day, all.x=T)
N_yday_birdID_year$days_since_start_postbreeding = N_yday_birdID_year$yday_CEST-N_yday_birdID_year$first.postbreeding.day+1

# only use nearly complete data & only the last 30 days of pre-breeding and first 30 days of post-breeding:
# there is not always a pre-breeding phase, so also the NA's of days_since_end_prebreeding should be kept:
N_yday_birdID_year_sel = N_yday_birdID_year[N_yday_birdID_year$freq>46 & N_yday_birdID_year$days_since_start_postbreeding<=30 & (is.na(N_yday_birdID_year$days_since_end_prebreeding) | N_yday_birdID_year$days_since_end_prebreeding>-30),] 
table(N_yday_birdID_year$freq>46 & N_yday_birdID_year$days_since_start_postbreeding<=30 & (is.na(N_yday_birdID_year$days_since_end_prebreeding) | N_yday_birdID_year$days_since_end_prebreeding>-30)) # 1741 bird days are removed.

# remove data of 6118 in 2014 (only post-breeding data) and of 6288 in 2017 (only pre-breeding data):
N_yday_birdID_year_sel <- N_yday_birdID_year_sel[!(N_yday_birdID_year_sel$birdID==6118 & N_yday_birdID_year_sel$year==2014) & !(N_yday_birdID_year_sel$birdID==6288 & N_yday_birdID_year_sel$year==2017),]

# determine number of days per bird per year per breeding phase, and only include when at least 5 days with near-complete data:
N_yday_birdID_year_sel$n_days=1
ndays.bird.phase.year = aggregate(n_days~birdID+year+breeding.phase, N_yday_birdID_year_sel, sum)
N_yday_birdID_year_sel <- merge(N_yday_birdID_year_sel, ndays.bird.phase.year[ndays.bird.phase.year$n_days>=5,c('birdID','year','breeding.phase')]) # from 3259 to 3115 bird days; 144 bird days removed.
N_yday_birdID_year_sel <- N_yday_birdID_year_sel[order(N_yday_birdID_year_sel$birdID, N_yday_birdID_year_sel$year, N_yday_birdID_year_sel$yday_CEST),]

# link to gps.behav data so that only nearly complete days are kept:
gps.breeding.data.behav = merge(gps.breeding.data.behav.30min, N_yday_birdID_year_sel[,c('birdID','year','yday_CEST')])
gps.breeding.data.behav <- gps.breeding.data.behav[order(gps.breeding.data.behav$birdID, gps.breeding.data.behav$date_time_CEST),]

gps.breeding.data.behav = gps.breeding.data.behav[order(gps.breeding.data.behav$birdID, gps.breeding.data.behav$date_time_CEST),]

# remove NA's: (this was already done, as no rows are removed after running the below line)
gps.breeding.data.behav <- na.omit(gps.breeding.data.behav[,c('birdID','year','date_time_CEST','longitude','latitude','habitat','breeding.phase','sex','behaviour','lat.nest','lon.nest','distance.to.nest','diel_rad','tidal_stage_rad')])

write.csv(gps.breeding.data.behav, "data/processed/gps.breeding.data.behav.csv") # file deposited on Dryad

keep(gps.breeding.data.behav, bird.data, breeding.data, sure=T)