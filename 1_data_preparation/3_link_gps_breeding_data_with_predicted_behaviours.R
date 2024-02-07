# duration of gps data, before it was linked to behaviour (as acceleration data were not always collected)
duration_yday_birdID_year_all <- aggregate(duration/60~year+birdID+sex+yday_CEST+week+breeding.phase+breeding.phase.nr, gps.breeding.data, sum) 
dim(duration_yday_birdID_year_all) # 5534 birddays with data on incubation, chick-rearing or post-breeding phase; 6371 birddays when also including pre-breeding.
names(duration_yday_birdID_year_all)[8] = 'dur_per_yday'
distr.duration <- table(round(duration_yday_birdID_year_all$dur_per_yday,0), duration_yday_birdID_year_all$birdID)
distr.duration # by far most gps data is with daily durations of around 24 hours
rowSums(distr.duration)

# link gps.breeding.data with predicted behaviour data:
gps.breeding.data.behav <- merge(gps.breeding.data, birdID.datetime.behavsel, all.x=T)
rm(gps.breeding.data, birdID.datetime.behavsel, df.pred.behav.sel) # to save memory
gps.breeding.data.behav$behaviour[is.na(gps.breeding.data.behav$behaviour)] <- "unknown"
table(gps.breeding.data.behav$birdID, gps.breeding.data.behav$behaviour)
gps.breeding.data.behav$speed_2d = round(gps.breeding.data.behav$speed_2d,2)

# check when and why ACC data are sometimes missing:
data.no.behaviour <- gps.breeding.data.behav[gps.breeding.data.behav$behaviour=="unknown",c('birdID','date_time')]
# check a sample of the missing data:
data.no.behaviour[80000,]
#df.all.fixed.0.4[df.all.fixed.0.4$birdID==6284 & df.all.fixed.0.4$date_time=="2016-06-14 20:20:18",] # does not exist. 
#acc.data.list[["6284"]][185000:185100,] # It is also not in the imported acc.data.list, suggesting that erroneously, not acc data was collected during this GPS fix. 
data.no.behaviour[85000,]
#df.all.fixed.0.4[df.all.fixed.0.4$birdID==6287 & df.all.fixed.0.4$date_time=="2016-05-11 07:50:48",] # does not exist. 
#acc.data.list[["6287"]][1,] # This is before the first ACC measurement was taken... Checking the UvA-BiTS database, this is true. 
#acc.data.list[["6282"]][12095:12105,c(2:6)] 

# remove data of duration of more than an hour (but check the durations for transmitters set at a GPS fix every hour, as this is often slightly more than an hour; these data should not be removed)
table(round(gps.breeding.data.behav$duration,1)) # only durations of <61 minutes are included, which happened within the determine.breeding.phases function
## Determine the total duration recorded per day, and remove the days with less than 23.1 hours of data (this can happen, as in a previous step we deleted points with a duration of more than 60 minutes, and in some cases, behaviour was not predicted as no acc-data were collected)
duration_yday_birdID_year_all <- aggregate(duration/60~year+birdID+sex+yday_CEST+week+breeding.phase+breeding.phase.nr, gps.breeding.data.behav, sum) 
dim(duration_yday_birdID_year_all) # 5663 birddays
names(duration_yday_birdID_year_all)[8] = 'dur_per_yday'
distr.duration <- table(round(duration_yday_birdID_year_all$dur_per_yday,0), duration_yday_birdID_year_all$birdID)
distr.duration # when linked with behaviour estimated from acc-data, total duration per day is often less than 23.8 hours, as regularly, an acc-sample is not measured. Here, it seems more appropriate to include days with at least 20 hours of data. 

# What if we first re-calculate the durations, so that measurements that are missed are assumed to be the same behaviour as the measurement before:
#gps.breeding.data.behav.known <- gps.breeding.data.behav.known[order(gps.breeding.data.behav.known$birdID, gps.breeding.data.behav.known$date_time),]
#gps.breeding.data.behav.known$time_to_previous_behav <- c(NA, interval(gps.breeding.data.behav.known$date_time[1:(dim(gps.breeding.data.behav.known)[1]-1)], gps.breeding.data.behav.known$date_time[2:dim(gps.breeding.data.behav.known)[1]])%/%seconds(1)) 
#gps.breeding.data.behav.known$time_to_next_behav <- c(gps.breeding.data.behav.known$time_to_previous_behav[2:dim(gps.breeding.data.behav.known)[1]], NA)
#gps.breeding.data.behav.known$duration_behav = round((gps.breeding.data.behav.known$time_to_previous_behav+gps.breeding.data.behav.known$time_to_next_behav)/2,2)/60 # to get the duration in decimal minutes
#table(round(gps.breeding.data.behav.known$duration_behav,0))
# (again) select on durations <61 minutes (and >0 minute, to remove the negative numbers between fixes of different birds)
#gps.breeding.data.behav.known <- gps.breeding.data.behav.known[gps.breeding.data.behav.known$duration_behav>0 & gps.breeding.data.behav.known$duration_behav<61,]
#hist(gps.breeding.data.behav.known$duration_behav) # the vast majoirty has a duration of <30 minutes, so that is a better selection.
#table(round(gps.breeding.data.behav.known$duration_behav,0))
#gps.breeding.data.behav.sel <- gps.breeding.data.behav.known[gps.breeding.data.behav.known$duration_behav<=30,]

# check again where to put the criterium of removing data
#duration_yday_birdID_year_all <- aggregate(duration_behav/60~year+birdID+sex+yday_CEST+week+breeding.phase+breeding.phase.nr+attempt+day_rel_hatching+week_rel_hatching, gps.breeding.data.behav.sel[gps.breeding.data.behav.sel$breeding.phase.nr>2,], sum) 
#names(duration_yday_birdID_year_all)[11] = 'dur_per_yday'
#dim(duration_yday_birdID_year_all) # 4757 birddays
#distr.duration <- table(round(duration_yday_birdID_year_all$dur_per_yday,1), duration_yday_birdID_year_all$birdID)
#distr.duration[130:160,]
#duration_yday_birdID_year = duration_yday_birdID_year_all[duration_yday_birdID_year_all$dur_per_yday>23.5,] # remove days with summed durations of intervals with estimated behaviours and known habitats of 23.5 h or less. As duration of 30 minutes and smaller were kept, it means that for an entire day to be covered, the total duration should be >23.5 hours.  
#dim(duration_yday_birdID_year) ## 3981 birddays left (4757-3982 = 776 birddays removed)
#duration_yday_birdID_year_removed = duration_yday_birdID_year_all[duration_yday_birdID_year_all$dur_per_yday<23.5,] # data that were removed
#table(duration_yday_birdID_year_removed$birdID, duration_yday_birdID_year_removed$year) ## this is where the data of 656 in 2013-2015 got removed. probably because this bird often vistsed locations outside the habitat map, and these data were removed within the determine.breeding.phases function. therefore, the total duration per yday left over is often less than 23.5 hour, despite that the GPS interval was usually 1 hour. 

#gps.breeding.data.behav.sel = merge(gps.breeding.data.behav.sel, duration_yday_birdID_year[,c('year','birdID','yday_CEST')]) # remove the days with durations<=23.5 in the gps.breeding.data.behav.sel file, as well as the days in the pre-breeding or (very broad) breeding phase. 
#gps.breeding.data.behav.sel <- gps.breeding.data.behav.sel[order(gps.breeding.data.behav.sel$birdID, gps.breeding.data.behav.sel$date_time),]
#dim(gps.breeding.data.behav.known) # 919630 rows
#dim(gps.breeding.data.behav.sel) # 713608 rows 
#table(gps.breeding.data.behav.sel$birdID, gps.breeding.data.behav.sel$year)

#write.table(table(gps.breeding.data.behav.sel$birdID, gps.breeding.data.behav.sel$year), 'clipboard', sep='\t')

#hist(gps.breeding.data.behav.sel$duration_behav) # the vast majority of data has duration of <20 minutes. I guess it is better to only use these data. 

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

# bij 656 in 2015 zijn er twee nest coordinaten die aan een breeding attempt zijn gekoppeld... omdat de coordinaten bepaald zijn per zender, niet per vogel... Remove the first row of 2015, with the least data:
nest.locations.bird.year = nest.locations.bird.year[rownames(nest.locations.bird.year)!=11,]

# add lat.nest and lon.nest to gps.breeding.data.behav
gps.breeding.data.behav <- merge(gps.breeding.data.behav, nest.locations.bird.year[,c('birdID','year','attempt','lat.nest','lon.nest')])

gps.breeding.data.behav = gps.breeding.data.behav[order(gps.breeding.data.behav$year, gps.breeding.data.behav$birdID, gps.breeding.data.behav$attempt, gps.breeding.data.behav$breeding.phase.nr),]
unique(gps.breeding.data.behav[,c('year','birdID','attempt','breeding.phase','lat.nest','lon.nest')])

# calculate distance to nest:
gps.breeding.data.behav$distance.to.nest <- NA
gps.breeding.data.behav$distance.to.nest[is.na(gps.breeding.data.behav$lat.nest)==F] <- round(distCosine(gps.breeding.data.behav[is.na(gps.breeding.data.behav$lat.nest)==F,c('longitude','latitude')], gps.breeding.data.behav[is.na(gps.breeding.data.behav$lat.nest)==F,c('lon.nest','lat.nest')], r=6378137),3) # gives the distance in meters between nest location and actual position of the bird; can only be calculated when lat.nest (and lon.nest) is known.

# make column "behaviour2" that combines behaviour with habitat:
gps.breeding.data.behav$behaviour2 = gps.breeding.data.behav$behaviour
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour=='foraging'] = paste(gps.breeding.data.behav$behaviour[gps.breeding.data.behav$behaviour=='foraging'], gps.breeding.data.behav$habitat_salinity[gps.breeding.data.behav$behaviour=='foraging'], sep='_')
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour=='resting'] = paste(gps.breeding.data.behav$behaviour[gps.breeding.data.behav$behaviour=='resting'], gps.breeding.data.behav$habitat_type[gps.breeding.data.behav$behaviour=='resting'], sep='_')
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$distance.to.nest<5] = 'at_nest' 
# show that the criterium of <5 meter distance to the nest leads to ca. 100% nest attendance during egg incubation, when summing up the proportion of time spent on the nest by males and females (even though data comes from different nests)
#gps.behav.eggs <- gps.breeding.data.behav[gps.breeding.data.behav$breeding.phase=='3.eggs',]
#duration.behav.sex.eggs <- aggregate(duration_behav~behaviour2+sex, gps.behav.eggs, sum)
#duration.behav.F.eggs <- duration.behav.sex.eggs[duration.behav.sex.eggs$sex=="F",]
#duration.behav.M.eggs <- duration.behav.sex.eggs[duration.behav.sex.eggs$sex=="M",]
#duration.behav.F.eggs$prop.dur <- duration.behav.F.eggs$duration_behav/sum(duration.behav.F.eggs$duration_behav)
#duration.behav.M.eggs$prop.dur <- duration.behav.M.eggs$duration_behav/sum(duration.behav.M.eggs$duration_behav)
#duration.behav.F.eggs$prop.dur[1]+duration.behav.M.eggs$prop.dur[1] # 1.039 (i.e., close to 1); <4 meter gives 0.965.

table(gps.breeding.data.behav$behaviour2)
# turn foraging_land into "other"  
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour2=="foraging_land"] <- "other" 
table(gps.breeding.data.behav$habitat)
gps.breeding.data.behav[gps.breeding.data.behav$habitat=="Eilanden_Rest",]

gps.breeding.data.behav$breeding.phase2 <- as.character(gps.breeding.data.behav$breeding.phase)
gps.breeding.data.behav$breeding.phase2[gps.breeding.data.behav$breeding.phase.nr==5|gps.breeding.data.behav$breeding.phase.nr==6] <- "5.post-breeding"

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
#all.dates <- force_tz(all.dates, tz="Europe/Amsterdam") # let's for now keep it in UTC, as otherwise we get issues with summer and wintertime when hours are skipped...
all.dates
df_date_time_30min <- expand.grid(date=all.dates, hour=0:23, minute=c(0,30))
df_date_time_30min$date_time_30min <- df_date_time_30min$date+3600*df_date_time_30min$hour+60*df_date_time_30min$minute
# order on date_time_30min:
df_date_time_30min <- df_date_time_30min[order(df_date_time_30min$date_time_30min),]

tide.data$date_time_30min <- floor_date(tide.data$date_time_UTC, unit = "30 mins")

# make sure tide data is ordered before using the findInterval function:
tide.data <- tide.data[order(tide.data$date_time_30min),]
tide.data$time_to_next_tide<-NA
tide.data$time_to_next_tide[1:(dim(tide.data)[1]-1)] <- tide.data$date_time_30min[2:dim(tide.data)[1]]-tide.data$date_time_30min[1:(dim(tide.data)[1]-1)]
tide.data$time_to_previous_tide[2:dim(tide.data)[1]] <- tide.data$time_to_next_tide[1:(dim(tide.data)[1]-1)]

df_date_time_30min = merge(df_date_time_30min[,c('date','date_time_30min')], tide.data[,c('date_time_30min','lowhigh','time_to_next_tide','time_to_previous_tide')], by="date_time_30min", all.x=T)

# determine tidal stage in radians, where 0/2pi is high tide and pi is low tide.
head(df_date_time_30min)
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

save.image("tmp.data.20231128.RData")

# do the same with the solar cycle: 
df=df_date_time_30min[,c('date_time_30min','lowhigh','tidal_stage_rad')]
df$date <- floor_date(df$date_time_30min, 'days')
df_sunrise_sunset <- data.frame(date=unique(df[,'date']))
df_sunrise_sunset$latitude <- 53.48
df_sunrise_sunset$longitude <- 6.25
coordinates(df_sunrise_sunset)  <- ~longitude + latitude  # to make it a spatialpoints dataframe
df_sunrise_sunset$sunset=sunriset(coordinates(df_sunrise_sunset), df_sunrise_sunset$date, POSIXct.out=TRUE, direction="sunset")[,2]
# floor to 30 min
df_sunrise_sunset$sunset_30min <- floor_date(df_sunrise_sunset$sunset, '30min')
df_sunrise_sunset$sunrise=sunriset(coordinates(df_sunrise_sunset), df_sunrise_sunset$date, POSIXct.out=TRUE, direction="sunrise")[,2]
df_sunrise_sunset$sunrise_30min <- floor_date(df_sunrise_sunset$sunrise, '30min')
df_sunrise_sunset$time_from_sunrise_to_sunset = as.numeric(df_sunrise_sunset$sunset_30min - df_sunrise_sunset$sunrise_30min)
df_sunrise_sunset$time_from_sunset_to_sunrise_next_day = as.numeric(c(df_sunrise_sunset$sunrise_30min[2:dim(df_sunrise_sunset)[1]]- df_sunrise_sunset$sunset_30min[1:(dim(df_sunrise_sunset)[1]-1)],0))

df = merge(df, as.data.frame(df_sunrise_sunset)[,c('date','sunrise_30min','sunset_30min','time_from_sunrise_to_sunset','time_from_sunset_to_sunrise_next_day')], by='date')

# create column telling the solar stage (night, sunrise, day, sunset) and cycle (in radians)
df$solar_stage = NA
df$solar_stage[df$date_time_30min==df$sunrise_30min] = 'sunrise'
df$solar_stage[df$date_time_30min==df$sunset_30min] = 'sunset'
df$time_to_next_sunriset = NA
df$solar_stage_rad = NA
start.row <- min(which(is.na(df$solar_stage)==F)) # start at first sunrise

#df.test = df
#df = df.test[1:100,]
#df=df.test

for(i in start.row:dim(df)[1]) {
  if (is.na(df$solar_stage[i])==F & df$solar_stage[i]=='sunrise') df$solar_stage_rad[i] = 0
  if (is.na(df$solar_stage[i])==F & df$solar_stage[i]=='sunset') df$solar_stage_rad[i] = pi
  if (is.na(df$solar_stage[i]) & df$solar_stage_rad[i-1]<pi) { # this is between sunrise and sunset
    df$solar_stage_rad[i] = df$solar_stage_rad[i-1]+(0.5/df$time_from_sunrise_to_sunset[i])*pi
  }
  if (is.na(df$solar_stage[i]) & df$solar_stage_rad[i-1]>=pi) { # this is between sunset and sunrise the next day
    df$solar_stage_rad[i] = df$solar_stage_rad[i-1]+(0.5/df$time_from_sunset_to_sunrise_next_day[i])*pi
  }
}

# check if data is still in UTC
head(df)

save.image("tmp.data.20231128.RData")
load("tmp.data.20231128.RData")

df = df[,c('date','date_time_30min','lowhigh','tidal_stage_rad','solar_stage','solar_stage_rad')]

#
df[df$date=='2013-04-01',]
df$date_time_test = df$date_time_30min
# first change UTC into CEST before linking to gps data:
df$date_time_30min <- with_tz(df$date_time_30min, tz="Europe/Amsterdam")
df$diel_rad = (hour(df$date_time_30min)*60+minute(df$date_time_30min))/1440*2*pi # this should only be done after time is set to CEST (to match with hour_CEST)
gps.breeding.data.behav = merge(gps.breeding.data.behav, df, by='date_time_30min')
gps.breeding.data.behav = gps.breeding.data.behav[order(gps.breeding.data.behav$birdID, gps.breeding.data.behav$date_time),]

head(gps.breeding.data.behav)
gps.breeding.data.behav$diel_rad = (hour(gps.breeding.data.behav$date_time_30min)*60+minute(gps.breeding.data.behav$date_time_30min))/1440*2*pi # this should only be done after time is set to CEST (to match with hour_CEST)
head(gps.breeding.data.behav)

# check that this went correctly (date_time_30min should be in CEST, date_time_test in UTC): 
gps.breeding.data.behav$date_time_30min[1] 
gps.breeding.data.behav$date_time_test[1]

# for plotting purposes, also make categorical variables for tide:
gps.breeding.data.behav[1:20,]
# determine tidal_phase based on tidal_stage_rad, equally dividing the cycle into four phases, where 0 +/- 1/4pi = high, 1/4pi - 3/4pi = outgoing, 3/4pi-1.25pi = low, 1.25-1.74pi=incoming:
gps.breeding.data.behav$tidal_stage_cat = 'high'
gps.breeding.data.behav$tidal_stage_cat[gps.breeding.data.behav$tidal_stage_rad>=0.25*pi & gps.breeding.data.behav$tidal_stage_rad<0.75*pi] = 'outgoing'
gps.breeding.data.behav$tidal_stage_cat[gps.breeding.data.behav$tidal_stage_rad>=0.75*pi & gps.breeding.data.behav$tidal_stage_rad<1.25*pi] = 'low'
gps.breeding.data.behav$tidal_stage_cat[gps.breeding.data.behav$tidal_stage_rad>=1.25*pi & gps.breeding.data.behav$tidal_stage_rad<1.75*pi] = 'incoming'
table(gps.breeding.data.behav$tidal_stage_cat)
table(gps.breeding.data.behav$tidal_phase)

# OLD WAY of determining on hour scale whether tide is low, incoming, high or outgoing:
#tidal.phase.hour <- expand.grid(hour=0:23, date=unique(tide.data$date))
#tidal.phase.hour$date_hour_CEST <- dmy_h(paste(tidal.phase.hour$date, tidal.phase.hour$hour, sep=" "))
#tidal.phase.hour$date_hour_CEST <- force_tz(tidal.phase.hour$date_hour_CEST, tzone="Europe/Amsterdam")
#tidal.phase.hour <- merge(tidal.phase.hour, tide.data[,c("date_hour_CEST","lowhigh")], all.x=T)
#tidal.phase.hour <- tidal.phase.hour[order(tidal.phase.hour$date_hour_CEST),]
# make adjacent hour before and after the hour of low and high tide also low and high, resulting in 3 hour periods of low and high tide; then, the hours from low to high are labelled as "incoming" and from high to low outgoing:
#tidal.phase.hour$tide_min1 <- c(tidal.phase.hour$lowhigh[2:dim(tidal.phase.hour)[1]],NA)
#tidal.phase.hour$tide_plus1 <- c(NA,tidal.phase.hour$lowhigh[1:(dim(tidal.phase.hour)[1]-1)])
#tidal.phase.hour$tidal_phase <- tidal.phase.hour$lowhigh
# fill in the spots where tide_min1 is not NA:
#tidal.phase.hour$tidal_phase[is.na(tidal.phase.hour$tide_min1)==F] <- tidal.phase.hour$tide_min1[is.na(tidal.phase.hour$tide_min1)==F]
# fill in the spots where tide_min1 is not NA:
#tidal.phase.hour$tidal_phase[is.na(tidal.phase.hour$tide_plus1)==F] <- tidal.phase.hour$tide_plus1[is.na(tidal.phase.hour$tide_plus1)==F]
# fill in the other NA values with incoming or outgoing
#tidal.phase.hour$tidal_phase[is.na(tidal.phase.hour$tidal_phase)] <- "unknown"
#for (i in 2:dim(tidal.phase.hour)[1]) {
#  if (tidal.phase.hour$tidal_phase[i-1]=='low'&tidal.phase.hour$tidal_phase[i]=="unknown") tidal.phase.hour$tidal_phase[i] <- "incoming"
#  if (tidal.phase.hour$tidal_phase[i-1]=='incoming'&tidal.phase.hour$tidal_phase[i]=="unknown") tidal.phase.hour$tidal_phase[i] <- "incoming"
#  if (tidal.phase.hour$tidal_phase[i-1]=='high'&tidal.phase.hour$tidal_phase[i]=="unknown") tidal.phase.hour$tidal_phase[i] <- "outgoing"
#  if (tidal.phase.hour$tidal_phase[i-1]=='outgoing'&tidal.phase.hour$tidal_phase[i]=="unknown") tidal.phase.hour$tidal_phase[i] <- "outgoing"
#}
# make first three cases manually outgoing:
#tidal.phase.hour$tidal_phase[1:3] <- "outgoing"
#table(tidal.phase.hour$tidal_phase)

#gps.breeding.data.behav$date_hour_CEST <- round_date(gps.breeding.data.behav$date_time, "hour")
#gps.breeding.data.behav <- merge(gps.breeding.data.behav, tidal.phase.hour[,c('date_hour_CEST','tidal_phase')])

keep(ColHabitats, gps.breeding.data.behav, duration_yday_birdID_year_all, bird.data, breeding.data, sure=T)
