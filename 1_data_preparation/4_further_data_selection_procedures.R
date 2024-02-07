unique(gps.breeding.data.behav[gps.breeding.data.behav$year==2014 & gps.breeding.data.behav$birdID==6118,c('birdID','month','yday_CEST','breeding.phase','behaviour')]) # no acceleration data were collected for 6118 in 2014 until August... therefore only post-breeding data

# remove the date of deployment from the data (can be removed afterwards, as it is now included in the code for determining breeding phases):
gps.breeding.data.behav <- gps.breeding.data.behav[floor_date(gps.breeding.data.behav$date_hour_CEST, 'days')>floor_date(gps.breeding.data.behav$start_deployment, 'days'),]

# check the attempts and nest locations per bird per year:
nest.location.bird.year.attempt = unique(gps.breeding.data.behav[,c('year','birdID','attempt','lat.nest','lon.nest')])
nest.location.bird.year.attempt = nest.location.bird.year.attempt[order(nest.location.bird.year.attempt$year, nest.location.bird.year.attempt$birdID, nest.location.bird.year.attempt$attempt),]

# data of 763 are removed from 2015 onward, as GPS locations are very inaccurate (which is visible from the wide spread of locations around the nest location, and corresponding low nest attendance)
gps.breeding.data.behav$exclude <- 0
gps.breeding.data.behav$exclude[gps.breeding.data.behav$birdID==763&(gps.breeding.data.behav$year>=2015)] <- 1
gps.breeding.data.behav <- gps.breeding.data.behav[gps.breeding.data.behav$exclude==0,]
table(gps.breeding.data.behav$birdID, gps.breeding.data.behav$year)
# check breeding phases: only include pre-breeding from the day the bird first visited the Oosterkwelder in a particular year. This means that the column 'breeding' says 'breeding' (and not 'pre-breeding'; which is a different definition from pre-breeding than in the category breeding.phase).
table(gps.breeding.data.behav$breeding.phase, gps.breeding.data.behav$breeding)
gps.breeding.data.behav$breeding.phase[gps.breeding.data.behav$breeding=='pre-breeding'] = NA
table(gps.breeding.data.behav$breeding.phase, gps.breeding.data.behav$breeding)

# remove cases where breeding.phase=NA
gps.breeding.data.behav = gps.breeding.data.behav[!is.na(gps.breeding.data.behav$breeding.phase),]
# remove cases where behaviour was not predicted (no acceleration data available):
gps.breeding.data.behav = gps.breeding.data.behav[gps.breeding.data.behav$behaviour!='unknown',]
# remove cases where habitat was unknown (because outside habitat map):
gps.breeding.data.behav = gps.breeding.data.behav[gps.breeding.data.behav$habitat!='unknown',]

# what to do with the Noordzee-locations? 
noordzee.data = gps.breeding.data.behav[gps.breeding.data.behav$habitat=='Noordzee',]
coordinates(noordzee.data) <- c("longitude","latitude")
proj4string(noordzee.data) <- CRS("+proj=longlat +datum=WGS84")
mapview(noordzee.data, zcol="behaviour", col.regions=c('red','blue','grey','green')) # locations 
# Include them into marine habitat when foraging, or Schier/island when resting.  
table(noordzee.data$behaviour2)
table(gps.breeding.data.behav$behaviour2)
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$habitat=='Noordzee' & gps.breeding.data.behav$behaviour2 =='resting_rest']<-'resting_schier'

# what to do with locations on "Eilanden_rest"?
table(gps.breeding.data.behav$behaviour2, gps.breeding.data.behav$habitat)

# check where the locations on Eilanden_Rest are:
gps.breeding.data.rest = gps.breeding.data.behav[gps.breeding.data.behav$habitat=='Eilanden_Rest',]
gps.breeding.data.rest$behav.nr <- 1
gps.breeding.data.rest$behav.nr[gps.breeding.data.rest$behaviour=='foraging'] <- 2
gps.breeding.data.rest$behav.nr[gps.breeding.data.rest$behaviour=='flying'] <- 3
coordinates(gps.breeding.data.rest) <- c("longitude","latitude")
proj4string(gps.breeding.data.rest) <- CRS("+proj=longlat +datum=WGS84")
mapview(gps.breeding.data.rest, zcol="behaviour", col.regions=c('red','blue','grey','green')) # locations on saltmarsh of Ameland and in gully on Rottumerplaat 
# when foraging, include into foraging brackish, when resting, include into 'resting wadden sea'
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
gps.breeding.data.behav <- gps.breeding.data.behav[order(gps.breeding.data.behav$birdID, gps.breeding.data.behav$date_time),]
gps.breeding.data.behav <- gps.breeding.data.behav %>%
  mutate(time_interval = floor_date(date_time, unit = "30 mins"))

gps.breeding.data.behav.30min <- gps.breeding.data.behav %>%
  group_by(time_interval, year, birdID, yday_CEST) %>%
  slice(1)

# how often are different behaviours assigned:
sample.sizes.behaviours = table(gps.breeding.data.behav.30min$behaviour)
sample.sizes.behaviours['other']/sum(sample.sizes.behaviours) # 5.0% was estimated as 'other' behaviour.

# check data:
gps.breeding.data.behav.30min[1:10,c('year','birdID','yday_CEST','date_time','time_interval')]

# number of samples per individual per day after down-sampling to 30 minute interval:
N_yday_birdID_year <- aggregate(freq~year+birdID+sex+yday_CEST+week+breeding.phase+breeding.phase.nr+attempt+day_rel_hatching+week_rel_hatching, gps.breeding.data.behav.30min[gps.breeding.data.behav.30min$breeding.phase.nr!=2,], sum) 
table(N_yday_birdID_year$freq)

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
table(N_yday_birdID_year$freq>46) # 4646 bird days with 47 or 48 samples are kept, 408 bird days with less samples are removed.
table(N_yday_birdID_year$days_since_start_postbreeding<=30) # 1476 bird days during post-breeding are removed; 3435 kept.
table(is.na(N_yday_birdID_year$days_since_end_prebreeding) | N_yday_birdID_year$days_since_end_prebreeding>-30) # 25 days during pre-breeding are removed; 5029 kept. 
# when combined (sometimes the criteria overlap, so less days removed than the sum of 408 + 1476 + 25 = 1909), 
table(N_yday_birdID_year$freq>46 & N_yday_birdID_year$days_since_start_postbreeding<=30 & (is.na(N_yday_birdID_year$days_since_end_prebreeding) | N_yday_birdID_year$days_since_end_prebreeding>-30)) # 1741 bird days are removed.

N_yday_birdID_year[N_yday_birdID_year$birdID==6288 & N_yday_birdID_year$year==2017,] # transmitter didn't work properly from the incubation phase onward, therefore having incomplete data for most of the days, and therefore the incubation, chick-rearing and post-breeding phase were excluded. 

# remove data of 6118 in 2014 (only post-breeding data) and of 6288 in 2017 (only pre-breeding data):
N_yday_birdID_year_sel <- N_yday_birdID_year_sel[!(N_yday_birdID_year_sel$birdID==6118 & N_yday_birdID_year_sel$year==2014) & !(N_yday_birdID_year_sel$birdID==6288 & N_yday_birdID_year_sel$year==2017),]

# determine number of days per bird per year per breeding phase (no need to specify per attempt, it doesn't matter if the days come from several attempts in the same year), and only include when at least 5 days with near-complete data:
N_yday_birdID_year_sel$n_days=1
ndays.bird.phase.year = aggregate(n_days~birdID+year+breeding.phase, N_yday_birdID_year_sel, sum)

N_yday_birdID_year_sel <- merge(N_yday_birdID_year_sel, ndays.bird.phase.year[ndays.bird.phase.year$n_days>=5,c('birdID','year','breeding.phase')])
# from 3259 to 3115 bird days; 144 bird days removed.
N_yday_birdID_year_sel <- N_yday_birdID_year_sel[order(N_yday_birdID_year_sel$birdID, N_yday_birdID_year_sel$year, N_yday_birdID_year_sel$yday_CEST),]

# link to gps.behav data so that only nearly complete days are kept:
gps.breeding.data.behav.sel = merge(gps.breeding.data.behav.30min, N_yday_birdID_year_sel[,c('birdID','year','yday_CEST')])
 
gps.breeding.data.behav <- gps.breeding.data.behav.sel
gps.breeding.data.behav <- gps.breeding.data.behav[order(gps.breeding.data.behav$birdID, gps.breeding.data.behav$year, gps.breeding.data.behav$yday_CEST),]


### add binary columns for whether bird is attending the nest, foraging or foraging in marine water: 
gps.breeding.data.behav$at_nest <- 0
gps.breeding.data.behav$at_nest[gps.breeding.data.behav$behaviour2=='at_nest'] <- 1
gps.breeding.data.behav$foraging <- 0
gps.breeding.data.behav$foraging[gps.breeding.data.behav$behaviour2=='foraging_freshwater'|gps.breeding.data.behav$behaviour2=='foraging_brackish'|gps.breeding.data.behav$behaviour2=='foraging_marine'] <- 1 # foraging on land is not considered as foraging
gps.breeding.data.behav$foraging_marine <- 0
gps.breeding.data.behav$foraging_marine[gps.breeding.data.behav$behaviour2=='foraging_brackish'|gps.breeding.data.behav$behaviour2=='foraging_marine'] <- 1

keep(gps.breeding.data.behav, N_yday_birdID_year, N_yday_birdID_year_sel, bird.data, breeding.data, sure=T)
