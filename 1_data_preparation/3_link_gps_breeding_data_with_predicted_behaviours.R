# breeding.phase 'breeding' was determined in a separate column. 
# replace NA values in the breeding.phase column with the value in the breeding column
gps.breeding.data$breeding.phase[is.na(gps.breeding.data$breeding.phase)]<-gps.breeding.data$breeding[is.na(gps.breeding.data$breeding.phase)]
table(gps.breeding.data$birdID, gps.breeding.data$year)

# remove NA's from predicted behaviour file 
df.behav.NA <- df.all.fixed.0.4[is.na(df.all.fixed.0.4$pred.behav),]
table(is.na(df.behav.NA$kurt.z)) # the cases where behaviour could not predicted (N=293962) had NA for kurtosis and skewness of the z-axis. These all concerned old transmitters.
# remove these NA's
df.all.fixed.0.4 <- df.all.fixed.0.4[order(df.all.fixed.0.4$birdID, df.all.fixed.0.4$date_time),]
df.pred.behav.sel <- na.omit(df.all.fixed.0.4[,c('birdID','date_time','pred.behav')])
# select the most occurring behaviour per birdID & date_time combination. 
# first pool behavioural categories
df.pred.behav.sel$behaviour <- as.character(df.pred.behav.sel$pred.behav)
df.pred.behav.sel$behaviour[df.pred.behav.sel$pred.behav%in%c("for-handle","for-intake","for-search")] = "foraging"
df.pred.behav.sel$behaviour[df.pred.behav.sel$pred.behav%in%c("walk","drink")] = "other"
df.pred.behav.sel$behaviour[df.pred.behav.sel$pred.behav%in%c("sit","stand")] = "resting"
df.pred.behav.sel$behaviour[df.pred.behav.sel$pred.behav%in%c("fly-flap","fly-soar")] = "flying"
# select the most often classified behaviour per GPS fix (as there are often multiple segments (2-4) of 0.4 s per GPS fix)
df.pred.behav.sel$freq <- 1
birdID.datetime.behaviours <- aggregate(freq~birdID+date_time+behaviour, df.pred.behav.sel, sum)
birdID.datetime.behaviours[is.na(birdID.datetime.behaviours$behaviour)]
birdID.datetime.behav.max <- aggregate(freq~birdID+date_time, birdID.datetime.behaviours, max) # the number of segments that the most often classified behaviour is classified
birdID.datetime.behavdom <- merge(birdID.datetime.behaviours, birdID.datetime.behav.max, by=c('birdID','date_time','freq')) # behaviour that is expressed most often. This could still be more than one, if so, then randomly chose on of these behaviours: 
birdID.datetime.behavdom$rnd <- runif(dim(birdID.datetime.behavdom)[1])
birdID.datetime.rndmax <- aggregate(rnd~birdID+date_time, birdID.datetime.behavdom, max)
birdID.datetime.behavsel <- merge(birdID.datetime.behavdom, birdID.datetime.rndmax, by=c("birdID","date_time","rnd"))[,c("birdID","date_time","behaviour")]
table(birdID.datetime.behavsel$birdID, year(birdID.datetime.behavsel$date_time))
rm(df.all.fixed.0.4)

# link gps.breeding.data with predicted behaviour data:
gps.breeding.data.behav <- merge(gps.breeding.data, birdID.datetime.behavsel, all.x=T)
gps.breeding.data.behav$behaviour[is.na(gps.breeding.data.behav$behaviour)] <- "unknown"
table(gps.breeding.data.behav$birdID, gps.breeding.data.behav$behaviour)
# check when and why ACC data are sometimes missing:
data.no.behaviour <- gps.breeding.data.behav[gps.breeding.data.behav$behaviour=="unknown",c('birdID','date_time')]
# check a sample of the missing data:
data.no.behaviour[80000,]
df.all.fixed.0.4[df.all.fixed.0.4$birdID==6284 & df.all.fixed.0.4$date_time=="2016-06-14 20:20:18",] # does not exist. 
acc.data.list[["6284"]][185000:185100,] # It is also not in the imported acc.data.list, suggesting that erroneously, not acc data was collected during this GPS fix. 
data.no.behaviour[85000,]
df.all.fixed.0.4[df.all.fixed.0.4$birdID==6287 & df.all.fixed.0.4$date_time=="2016-05-11 07:50:48",] # does not exist. 
acc.data.list[["6287"]][1,] # This is before the first ACC measurement was taken... Checking the UvA-BiTS database, this is true. 
acc.data.list[["6282"]][12095:12105,c(2:6)] 

## resting on nest (within 5 m of the nest position, dit wordt door de kolommen nest 1 en nest 2 aangegeven):
gps.breeding.data.behav$behaviour2 = gps.breeding.data.behav$behaviour
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour=='foraging'] = paste(gps.breeding.data.behav$behaviour[gps.breeding.data.behav$behaviour=='foraging'], gps.breeding.data.behav$habitat_salinity[gps.breeding.data.behav$behaviour=='foraging'], sep='_')
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour=='resting'] = paste(gps.breeding.data.behav$behaviour[gps.breeding.data.behav$behaviour=='resting'], gps.breeding.data.behav$habitat_type[gps.breeding.data.behav$behaviour=='resting'], sep='_')
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$nest1==1|gps.breeding.data.behav$nest2==1] = 'at_nest'
table(gps.breeding.data.behav$behaviour2)

gps.breeding.data.behav$speed_2d = round(gps.breeding.data.behav$speed_2d,2)

## numbering breeding phases:
gps.breeding.data.behav$breeding.phase.nr = 1
gps.breeding.data.behav$breeding.phase.nr[gps.breeding.data.behav$breeding.phase=='breeding'] = 2
gps.breeding.data.behav$breeding.phase.nr[gps.breeding.data.behav$breeding.phase=='eggs'] = 3
gps.breeding.data.behav$breeding.phase.nr[gps.breeding.data.behav$breeding.phase=='chicks'] = 4
gps.breeding.data.behav$breeding.phase.nr[gps.breeding.data.behav$breeding.phase=='fledglings'] = 5
gps.breeding.data.behav$breeding.phase.nr[gps.breeding.data.behav$breeding.phase=='post.breeding.successful'] = 6
gps.breeding.data.behav$breeding.phase.nr[gps.breeding.data.behav$breeding.phase=='post.breeding.unsuccessful'] = 7

gps.breeding.data.behav$breeding.phase2 = gps.breeding.data.behav$breeding.phase
gps.breeding.data.behav$breeding.phase = paste(gps.breeding.data.behav$breeding.phase.nr, gps.breeding.data.behav$breeding.phase, sep='.')
table(gps.breeding.data.behav$breeding.phase) # somehow, no 'breeding' was classified... 

## add columns with week relative to the hatching day.
## change the attempt number of the pre-breeding phase, to make it the same attempt as the subsequent breeding attempt:
gps.breeding.data.behav$attempt[gps.breeding.data.behav$breeding.phase=='1.pre-breeding'] = gps.breeding.data.behav$attempt[gps.breeding.data.behav$breeding.phase=='1.pre-breeding']+1
gps.breeding.data.behav = gps.breeding.data.behav[order(gps.breeding.data.behav$year, gps.breeding.data.behav$birdID, gps.breeding.data.behav$yday_CEST),]
ydays.year.bird.phase = unique(gps.breeding.data.behav[,c('year','birdID','sex','breeding.phase.nr','attempt','yday_CEST','week')])
dim(ydays.year.bird.phase) 

yday.min.year.bird.phase = aggregate(yday_CEST~birdID+sex+breeding.phase.nr+year+attempt, data=ydays.year.bird.phase, min)
hatch_day_bird_year = yday.min.year.bird.phase[yday.min.year.bird.phase$breeding.phase.nr==4,c('birdID','year','attempt','yday_CEST')]
names(hatch_day_bird_year)[4]='hatch_day'
gps.breeding.data.behav = merge(gps.breeding.data.behav, hatch_day_bird_year, all.x=T)
unique(gps.breeding.data.behav$hatch_day)
gps.breeding.data.behav$day_rel_hatching = gps.breeding.data.behav$yday_CEST-gps.breeding.data.behav$hatch_day
gps.breeding.data.behav$week_rel_hatching = floor(gps.breeding.data.behav$day_rel_hatching/7)
table(gps.breeding.data.behav$birdID, gps.breeding.data.behav$year)
# hier zit 656 nog steeds in

# remove data of duration of more than an hour (but check the durations for transmitters set at a GPS fix every hour, as this is often slightly more than an hour; these data should not be removed)
table(round(gps.breeding.data.behav$duration,1)) # only durations of <61 minutes are included, which happened within the determine.breeding.phases function
## Determine the total duration recorded per day, and remove the days with less than 23.1 hours of data (this can happen, as in a previous step we deleted points with a duration of more than 60 minutes, and in some cases, behaviour could not be predicted)
# remove breeding phases that we will not use in the analysis (exclude, pre-breeding and breeding, i.e. breeding.phase.nr>2):
duration_yday_birdID_year_all <- aggregate(duration/60~year+birdID+sex+yday_CEST+week+breeding.phase+breeding.phase.nr, gps.breeding.data.behav[gps.breeding.data.behav$breeding.phase.nr>2,], sum) 
dim(duration_yday_birdID_year_all) # 2570 birddays
names(duration_yday_birdID_year_all)[8] = 'dur_per_yday'
distr.duration <- table(round(duration_yday_birdID_year_all$dur_per_yday,1), duration_yday_birdID_year_all$birdID)
distr.duration # by far most data is with daily durations of 23.8-24.1 hours of data.  
duration_yday_birdID_year = duration_yday_birdID_year_all[duration_yday_birdID_year_all$dur_per_yday>=23.5,] # remove days with summed durations of less than 23.5h 
dim(duration_yday_birdID_year) ## 2508 birddays left (62 removed)
duration_yday_birdID_year_removed = duration_yday_birdID_year_all[duration_yday_birdID_year_all$dur_per_yday<23.5,] # remove days with summed durations of less than 23.5h 
table(duration_yday_birdID_year_removed$birdID, duration_yday_birdID_year_removed$year) ## 2508 birddays left (62 removed); this is where the data of 656 in 2013-2015 got removed. 
ydays.year.bird.phase <- unique(gps.breeding.data.behav[,c('year','birdID','sex','breeding.phase.nr','attempt','yday_CEST','week','day_rel_hatching','week_rel_hatching')])
duration_yday_birdID_year = merge(ydays.year.bird.phase, duration_yday_birdID_year[c('year','birdID','yday_CEST','breeding.phase' ,'dur_per_yday')], by=c('year','birdID','yday_CEST')) # to add the columns rel_day_hatching and rel_week_hatching to the duration_yday_birdID_year dataframe.

gps.breeding.data.behav.sel = merge(gps.breeding.data.behav.sel1, duration_yday_birdID_year[,c('year','birdID','yday_CEST')]) # remove the days with durations<=23.1 in the raw gps.breeding.data.behav file, as well as the days win the pre-breeding or (very broad) breeding phase. 
gps.breeding.data.behav.sel <- gps.breeding.data.behav.sel[order(gps.breeding.data.behav.sel$birdID, gps.breeding.data.behav.sel$date_time),]
dim(gps.breeding.data.behav) # 1129261 rows
dim(gps.breeding.data.behav.sel) # 872835 rows 
table(gps.breeding.data.behav.sel$birdID, gps.breeding.data.behav.sel$year) # hier zit 656 niet meer in...

gps.breeding.data.behav.sel[1:20,c('birdID','date_time','breeding.phase','behaviour','behaviour2')]

ydays.year.bird.phase.sel = unique(gps.breeding.data.behav.sel[,c('year','birdID','sex','breeding.phase.nr','attempt','yday_CEST','week','day_rel_hatching','week_rel_hatching')])

keep(ColHabitats, gps.breeding.data.behav.sel, ydays.year.bird.phase.sel, duration_yday_birdID_year, schier_new84_sel, sure=T)