# duration of gps data, before linking with behaviour
duration_yday_birdID_year_all <- aggregate(duration/60~year+birdID+sex+yday_CEST+week+breeding.phase+breeding.phase.nr, gps.breeding.data[gps.breeding.data$breeding.phase.nr>2,], sum) 
dim(duration_yday_birdID_year_all) # 5563 birddays
names(duration_yday_birdID_year_all)[8] = 'dur_per_yday'
distr.duration <- table(round(duration_yday_birdID_year_all$dur_per_yday,1), duration_yday_birdID_year_all$birdID)
distr.duration # by far most gps data is with daily durations of 23.8-24.1 hours of data.  
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

# only keep data where behaviour was predicted (i.e. acc-data were collected) and habitat was known
gps.breeding.data.behav.known <- gps.breeding.data.behav[gps.breeding.data.behav$behaviour!="unknown" & gps.breeding.data.behav$habitat!="unknown" & gps.breeding.data.behav$habitat_type!='rest',]

# remove data of duration of more than an hour (but check the durations for transmitters set at a GPS fix every hour, as this is often slightly more than an hour; these data should not be removed)
table(round(gps.breeding.data.behav.known$duration,1)) # only durations of <61 minutes are included, which happened within the determine.breeding.phases function
## Determine the total duration recorded per day, and remove the days with less than 23.1 hours of data (this can happen, as in a previous step we deleted points with a duration of more than 60 minutes, and in some cases, behaviour was not predicted as no acc-data were collected)
# remove breeding phases that we will not use in the analysis (exclude, pre-breeding and breeding, i.e. breeding.phase.nr>2):
duration_yday_birdID_year_all <- aggregate(duration/60~year+birdID+sex+yday_CEST+week+breeding.phase+breeding.phase.nr, gps.breeding.data.behav.known[gps.breeding.data.behav.known$breeding.phase.nr>2,], sum) 
dim(duration_yday_birdID_year_all) # 4761 birddays
names(duration_yday_birdID_year_all)[8] = 'dur_per_yday'
distr.duration <- table(round(duration_yday_birdID_year_all$dur_per_yday,0), duration_yday_birdID_year_all$birdID)
distr.duration # when linked with behaviour estimated from acc-data, total duration per day is often less than 23.8 hours, as regularly, an acc-sample is not measured. Here, it seems more appropriate to include days with at least 20 hours of data. 
# What if we first re-calculate the durations, so that measurements that are missed are assumed to be the same behaviour as the measurement before:
gps.breeding.data.behav.known <- gps.breeding.data.behav.known[order(gps.breeding.data.behav.known$birdID, gps.breeding.data.behav.known$date_time),]
gps.breeding.data.behav.known$time_to_previous_behav <- c(NA, interval(gps.breeding.data.behav.known$date_time[1:(dim(gps.breeding.data.behav.known)[1]-1)], gps.breeding.data.behav.known$date_time[2:dim(gps.breeding.data.behav.known)[1]])%/%seconds(1)) 
gps.breeding.data.behav.known$time_to_next_behav <- c(gps.breeding.data.behav.known$time_to_previous_behav[2:dim(gps.breeding.data.behav.known)[1]], NA)
gps.breeding.data.behav.known$duration_behav = round((gps.breeding.data.behav.known$time_to_previous_behav+gps.breeding.data.behav.known$time_to_next_behav)/2,2)/60 # to get the duration in decimal minutes
table(round(gps.breeding.data.behav.known$duration_behav,0))
# (again) select on durations <61 minutes (and >0 minute, to remove the negative numbers between fixes of different birds)
gps.breeding.data.behav.known <- gps.breeding.data.behav.known[gps.breeding.data.behav.known$duration_behav>0 & gps.breeding.data.behav.known$duration_behav<61,]
hist(gps.breeding.data.behav.known$duration_behav) # the vast majoirty has a duration of <30 minutes, so that is a better selection.
table(round(gps.breeding.data.behav.known$duration_behav,0))
gps.breeding.data.behav.sel <- gps.breeding.data.behav.known[gps.breeding.data.behav.known$duration_behav<=30,]

# assign nest locations to attempts: 
nest.locations.bird.year <- aggregate(cbind(nest1,nest2)~year+birdID+attempt+lat.nest1+lon.nest1+lat.nest2+lon.nest2+nest1.real+nest2.real,gps.breeding.data.behav.sel,sum)
nest.locations.bird.year = nest.locations.bird.year[order(nest.locations.bird.year$year, nest.locations.bird.year$birdID, nest.locations.bird.year$attempt),]
# when nest1>nest2 (always being a real nesting attempt)
nest.locations.bird.year[nest.locations.bird.year$nest1>nest.locations.bird.year$nest2,c('lat.nest','lon.nest')] <- nest.locations.bird.year[nest.locations.bird.year$nest1>nest.locations.bird.year$nest2,c('lat.nest1','lon.nest1')]
# when nest2>nest1 and nest2 was a real nesting attempt: 
nest.locations.bird.year[nest.locations.bird.year$nest2>nest.locations.bird.year$nest1 & nest.locations.bird.year$nest2.real==1,c('lat.nest','lon.nest')] <- nest.locations.bird.year[nest.locations.bird.year$nest2>nest.locations.bird.year$nest1 & nest.locations.bird.year$nest2.real==1,c('lat.nest2','lon.nest2')]
# during the time allocation analysis, it turned out that the nest assignment went wrong for 6288 and 6289 in 2017. The reason for 6288 was that the transmitter failed shortly after the real breeding attempt, therefore producing more locations at an earlier unsuccessful settlement. The reason for 6289 was that she had two breeding attempts with similar amounts of data. 
nest.locations.bird.year[nest.locations.bird.year$year==2017 & nest.locations.bird.year$birdID==6288,c('lat.nest','lon.nest')] <- nest.locations.bird.year[nest.locations.bird.year$year==2017 & nest.locations.bird.year$birdID==6288,c('lat.nest2','lon.nest2')] 
nest.locations.bird.year[nest.locations.bird.year$year==2017 & nest.locations.bird.year$birdID==6289,c('lat.nest','lon.nest')] <- nest.locations.bird.year[nest.locations.bird.year$year==2017 & nest.locations.bird.year$birdID==6289,c('lat.nest1','lon.nest1')] 
# add lat.nest and lon.nest to gps.breeding.data.behav.sel
gps.breeding.data.behav.sel <- merge(gps.breeding.data.behav.sel, nest.locations.bird.year[,c('birdID','year','attempt','lat.nest','lon.nest')])

# calculate distance to nest:
gps.breeding.data.behav.sel$distance.to.nest <- NA
gps.breeding.data.behav.sel$distance.to.nest[is.na(gps.breeding.data.behav.sel$lat.nest)==F] <- round(distCosine(gps.breeding.data.behav.sel[is.na(gps.breeding.data.behav.sel$lat.nest)==F,c('longitude','latitude')], gps.breeding.data.behav.sel[is.na(gps.breeding.data.behav.sel$lat.nest)==F,c('lon.nest','lat.nest')], r=6378137),3) # gives the distance in meters between nest location and actual position of the bird; can only be calculated when lat.nest (and lon.nest) is known.

# make column "behaviour2" that combines behaviour with habitat:
gps.breeding.data.behav.sel$behaviour2 = gps.breeding.data.behav.sel$behaviour
gps.breeding.data.behav.sel$behaviour2[gps.breeding.data.behav.sel$behaviour=='foraging'] = paste(gps.breeding.data.behav.sel$behaviour[gps.breeding.data.behav.sel$behaviour=='foraging'], gps.breeding.data.behav.sel$habitat_salinity[gps.breeding.data.behav.sel$behaviour=='foraging'], sep='_')
gps.breeding.data.behav.sel$behaviour2[gps.breeding.data.behav.sel$behaviour=='resting'] = paste(gps.breeding.data.behav.sel$behaviour[gps.breeding.data.behav.sel$behaviour=='resting'], gps.breeding.data.behav.sel$habitat_type[gps.breeding.data.behav.sel$behaviour=='resting'], sep='_')
gps.breeding.data.behav.sel$behaviour2[gps.breeding.data.behav.sel$distance.to.nest<10] = 'at_nest' # explorative analysis showed that this criterion leads to ca. 100% overall nest attendance when males and females are combined. 
table(gps.breeding.data.behav.sel$behaviour2)
# turn foraging_land into "other"  
gps.breeding.data.behav.sel$behaviour2[gps.breeding.data.behav.sel$behaviour2=="foraging_land"&gps.breeding.data.behav.sel$habitat_type=="schier"] <- "other" 
gps.breeding.data.behav.sel$behaviour2[gps.breeding.data.behav.sel$behaviour2=="foraging_land"&gps.breeding.data.behav.sel$habitat_type=="mainland"] <- "other"

gps.breeding.data.behav.sel$breeding.phase2 <- as.character(gps.breeding.data.behav.sel$breeding.phase)
gps.breeding.data.behav.sel$breeding.phase2[gps.breeding.data.behav.sel$breeding.phase.nr==5|gps.breeding.data.behav.sel$breeding.phase.nr==6] <- "5.post-breeding"

# check again where to put the criterium of removing data
duration_yday_birdID_year_all <- aggregate(duration_behav/60~year+birdID+sex+yday_CEST+week+breeding.phase+breeding.phase.nr+attempt+day_rel_hatching+week_rel_hatching, gps.breeding.data.behav.sel[gps.breeding.data.behav.sel$breeding.phase.nr>2,], sum) 
names(duration_yday_birdID_year_all)[11] = 'dur_per_yday'
dim(duration_yday_birdID_year_all) # 4757 birddays
distr.duration <- table(round(duration_yday_birdID_year_all$dur_per_yday,1), duration_yday_birdID_year_all$birdID)
distr.duration[130:160,]
duration_yday_birdID_year = duration_yday_birdID_year_all[duration_yday_birdID_year_all$dur_per_yday>23.5,] # remove days with summed durations of intervals with estimated behaviours and known habitats of 23.5 h or less. As duration of 30 minutes and smaller were kept, it means that for an entire day to be covered, the total duration should be >23.5 hours.  
dim(duration_yday_birdID_year) ## 3981 birddays left (4757-3982 = 776 birddays removed)
duration_yday_birdID_year_removed = duration_yday_birdID_year_all[duration_yday_birdID_year_all$dur_per_yday<23.5,] # data that were removed
table(duration_yday_birdID_year_removed$birdID, duration_yday_birdID_year_removed$year) ## this is where the data of 656 in 2013-2015 got removed. probably because this bird often vistsed locations outside the habitat map, and these data were removed within the determine.breeding.phases function. therefore, the total duration per yday left over is often less than 23.5 hour, despite that the GPS interval was usually 1 hour. 

gps.breeding.data.behav.sel = merge(gps.breeding.data.behav.sel, duration_yday_birdID_year[,c('year','birdID','yday_CEST')]) # remove the days with durations<=23.5 in the gps.breeding.data.behav.sel file, as well as the days in the pre-breeding or (very broad) breeding phase. 
gps.breeding.data.behav.sel <- gps.breeding.data.behav.sel[order(gps.breeding.data.behav.sel$birdID, gps.breeding.data.behav.sel$date_time),]
dim(gps.breeding.data.behav.known) # 919630 rows
dim(gps.breeding.data.behav.sel) # 713608 rows 
table(gps.breeding.data.behav.sel$birdID, gps.breeding.data.behav.sel$year)

hist(gps.breeding.data.behav.sel$duration_behav) # the vast majority of data has duration of <20 minutes. I guess it is better to only use these data. 

keep(ColHabitats, gps.breeding.data.behav.sel, duration_yday_birdID_year, bird.data, breeding.data, sure=T)
