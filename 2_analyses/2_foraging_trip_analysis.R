# first run the time allocation analysis script
keep(gps.breeding.data.behav, duration_yday_birdID_year, bird.data, breeding.data, sure=T)

source("functions.R") # to have the most updated version of the functions
Sys.setenv(TZ='Europe/Amsterdam')

### determine a circle of 50 m around the nest, which we here call "colony". If a bird moves outside the colony, we assume it is on a foraging trip (note that resting periods away from the colony are included in the foraging trip):
gps.breeding.data.behav$at_colony_50m = 0
gps.breeding.data.behav$at_colony_50m[gps.breeding.data.behav$distance.to.nest<50]=1 

gps.breeding.data.behav$foraging_trip = 0

### Determine and label individual foraging trips  (takes about 5 minutes to calculate)
### Important that the data is ordered correctly, on year, bird, date
gps.breeding.data.behav <- gps.breeding.data.behav[order(gps.breeding.data.behav$year, gps.breeding.data.behav$birdID, gps.breeding.data.behav$date_time),]

for(bird in unique(gps.breeding.data.behav$birdID)) {
  ## per year
  for (year in unique(gps.breeding.data.behav$year[gps.breeding.data.behav$birdID==bird])) {
    print(paste(bird, year))
    foraging.trip=0
    ## make temporary df for selected birdID and year:
    df.tmp = gps.breeding.data.behav[gps.breeding.data.behav$birdID==bird&gps.breeding.data.behav$year==year,]
    # determine if the first fix of a bird in a specific year is foraging or at the colony:
    if (df.tmp$at_colony_50m[1]==0) {
      df.tmp$foraging_trip[1]=1
      foraging.trip=1
    }
    for (i in 2:dim(df.tmp)[1]) {
      if (df.tmp$at_colony_50m[i]==1 & df.tmp$at_colony_50m[i-1]==1) df.tmp$foraging_trip[i]=0 # bird is still at the colony
      if (df.tmp$at_colony_50m[i]==0 & df.tmp$at_colony_50m[i-1]==1) {
        df.tmp$foraging_trip[i]=foraging.trip + 1 # bird starts a new foraging trip
        foraging.trip = foraging.trip + 1
        }
      if (df.tmp$at_colony_50m[i]==0 & df.tmp$at_colony_50m[i-1]==0) df.tmp$foraging_trip[i]=df.tmp$foraging_trip[i-1] # the bird is still on the same foraging trip
      if (df.tmp$at_colony_50m[i]==1 & df.tmp$at_colony_50m[i-1]==0) df.tmp$foraging_trip[i]=0 # bird returns to the colony from a foraging trip
    }
    ## paste the data of this bird into the df
    gps.breeding.data.behav$foraging_trip[gps.breeding.data.behav$birdID==bird&gps.breeding.data.behav$year==year] = df.tmp$foraging_trip  
    }
}

gps.breeding.data.behav$year = as.factor(gps.breeding.data.behav$year)

df.for.trips = gps.breeding.data.behav[gps.breeding.data.behav$foraging_trip>0,]

df.for.trips.34 = df.for.trips[df.for.trips$breeding.phase=='3.eggs'|df.for.trips$breeding.phase=='4.chicks',]

# check in which habitats foraging was estimated to happen: 
table(df.for.trips.34$habitat[df.for.trips.34$behaviour=='foraging'])
# we did not use this criteria: foraging here is also possible on the Schier_Kwelder for example... 
# however, birds, and particularly males, often make short trips on the saltmarsh to search for nestmaterial. 
table(df.for.trips.34$habitat[df.for.trips.34$foraging==1]) # however, in the time allocation analysis script, we used the criterium that we assumed a bird was foraging when it was estimated to be foraging on the basis of acc data AND was located in foraging habitat (water)

# use slightly different columns for habitat-specific foraging columns than in the time allocation analysis:
df.for.trips.34$dur_foraging = ifelse(df.for.trips.34$behaviour=='foraging',df.for.trips.34$duration/60,0)
df.for.trips.34$dur_foraging_marine = ifelse(df.for.trips.34$behaviour=='foraging'&df.for.trips.34$habitat_salinity=='marine',df.for.trips.34$duration/60,0)
df.for.trips.34$dur_foraging_freshwater = ifelse(df.for.trips.34$behaviour=='foraging'&df.for.trips.34$habitat_salinity=='freshwater',df.for.trips.34$duration/60,0)
df.for.trips.34$dur_foraging_brackish = ifelse(df.for.trips.34$behaviour=='foraging'&df.for.trips.34$habitat_salinity=='brackish',df.for.trips.34$duration/60,0)
# duration presence on the mainland:
df.for.trips.34$dur_mainland = ifelse(df.for.trips.34$habitat_type=='mainland',df.for.trips.34$duration/60,0)

max.distance.year.bird.trip = aggregate(distance.to.nest ~ year + birdID + foraging_trip, data=df.for.trips.34, max) 
dur.foraging.per.trip = aggregate(cbind(dur_foraging, dur_foraging_marine, dur_foraging_freshwater, dur_foraging_brackish, dur_mainland)~year+birdID+sex+foraging_trip, data=df.for.trips.34, sum) # breeding phase is removed, as a foraging trip can cover the switch from egg to chick phase, or from chick to postfledging... Later, the breeding phases will be coupled again.  
dur.foraging.per.trip$dur_foraging_water = dur.foraging.per.trip$dur_foraging_marine+dur.foraging.per.trip$dur_foraging_freshwater+dur.foraging.per.trip$dur_foraging_brackish
dur.foraging.per.trip$prop_foraging_marine = dur.foraging.per.trip$dur_foraging_marine/dur.foraging.per.trip$dur_foraging_water
dur.foraging.per.trip$prop_foraging_brackish = dur.foraging.per.trip$dur_foraging_brackish/dur.foraging.per.trip$dur_foraging_water
dur.foraging.per.trip$prop_foraging_fresh = dur.foraging.per.trip$dur_foraging_freshwater/dur.foraging.per.trip$dur_foraging_water
start_time_of_trips = aggregate(date_time-0.5*time_to_previous~year+birdID+foraging_trip, data=df.for.trips.34, min)
names(start_time_of_trips)[4] = 'start_time'
end_time_of_trips = aggregate(date_time+0.5*time_to_next~year+birdID+foraging_trip, data=df.for.trips.34, max)
names(end_time_of_trips)[4] = 'end_time'
trips.start.end <- cbind(start_time_of_trips, end_time=end_time_of_trips$end_time)
trips.start.end$yday_start <- yday(trips.start.end$start_time)
trips.start.end$yday_end <- yday(trips.start.end$end_time)
## only keep foraging trips when there are no missing days between the start and end of the trip (because the bird went outside the study area for example, or because it included durations of >60 minutes): 
ydays.year.bird.phase.sel <- ydays.year.bird.phase.sel[order(ydays.year.bird.phase.sel$year, ydays.year.bird.phase.sel$birdID, ydays.year.bird.phase.sel$yday_CEST),]
ydays.year.bird.phase.sel$next.day.missing <- 0
for (i in 2:dim(ydays.year.bird.phase.sel)[1]) if (ydays.year.bird.phase.sel$yday_CEST[i]!=(ydays.year.bird.phase.sel$yday_CEST[i-1]+1)) ydays.year.bird.phase.sel$next.day.missing[i-1] <- 1
trips.start.end.sel <- merge(trips.start.end, ydays.year.bird.phase.sel, by.x=c('year','birdID','yday_start'), by.y=c('year','birdID','yday_CEST'), all.x=T)
trips.start.end.sel[is.na(trips.start.end.sel$next.day.missing),] # these are cases where this yday was excluded because <23.5 hours of data. 
trips.start.end.sel <- merge(trips.start.end, ydays.year.bird.phase.sel, by.x=c('year','birdID','yday_start'), by.y=c('year','birdID','yday_CEST'))
trips.start.end.sel$end.trip.next.day <- ifelse(trips.start.end.sel$yday_end>trips.start.end.sel$yday_start,1,0)

day_week_rel_hatching_trips = aggregate(cbind(day_rel_hatching, week_rel_hatching, week,yday_CEST)~year+birdID+foraging_trip, data=df.for.trips.34, min)
trip_info = cbind(start_time_of_trips,end_time=end_time_of_trips$end_time,distance.to.nest=max.distance.year.bird.trip$distance.to.nest, day_week_rel_hatching_trips[,c('day_rel_hatching', 'week_rel_hatching', 'week','yday_CEST')])
df.for.trips.final.34 = merge(dur.foraging.per.trip, trip_info)
df.for.trips.final.34$dur_tot = round(as.numeric(df.for.trips.final.34$end_time-df.for.trips.final.34$start_time),2)/60
dim(df.for.trips.final.34)
dim(df.for.trips.final.34[df.for.trips.final.34$dur_foraging==0,]) ## in 2181 of the 6580 trips, birds were not (registered to be) foraging.
dim(df.for.trips.final.34[df.for.trips.final.34$dur_foraging_water==0,]) ## in 3253 of the 6580 trips, birds were not (registered to be) foraging in water (the only habitat where foraging is possible)
table(round(df.for.trips.final.34$dur_tot[df.for.trips.final.34$dur_foraging_water==0],1)) ## these were generally short trips, of less than half an hour.

df.for.trips.final.34 = df.for.trips.final.34[df.for.trips.final.34$dur_foraging_water>0,] # remove foraging trips with a foraging duration in water of 0 

dim(df.for.trips.final.34) # N=3327 trips left
# merge dataframe foraging trips (df.for.trips.final.34) with "ydays.year.bird.phase" to add breeding.phase to df.for.trips.final.34
df.for.trips.final.34 = merge(df.for.trips.final.34, ydays.year.bird.phase.sel[,c('year','birdID','yday_CEST','breeding.phase.nr')])
df.for.trips.final.34$breeding.phase = '3.eggs' # make all egg phase
df.for.trips.final.34$breeding.phase[df.for.trips.final.34$breeding.phase.nr==4] = '4.chicks' # convert part to chick phase


windows(10,6)
layout(matrix(1:6, ncol=3, byrow=T))
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$year==2013,],main="males in 2013", ylim=c(0,12), xlab="weeks to hatching", ylab="total duration foraging (h) in Waddensea", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$year==2014,],main="males in 2014", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$year==2015,],main="males in 2015", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$year==2016,],main="males in 2016", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$year==2017,],main="males in 2017", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$year==2018,],main="males in 2018", ylim=c(0,12), xlab="weeks to hatching", las=1)

windows(10,6)
layout(matrix(1:6, ncol=3, byrow=T))
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="F"&df.for.trips.final.34$year==2013,],main="females in 2013", ylim=c(0,12), xlab="weeks to hatching", ylab="total daily duration foraging (h) in Waddensea", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="F"&df.for.trips.final.34$year==2014,],main="females in 2014", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="F"&df.for.trips.final.34$year==2016,],main="females in 2016", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="F"&df.for.trips.final.34$year==2017,],main="females in 2017", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="F"&df.for.trips.final.34$year==2018,],main="females in 2018", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="F"&df.for.trips.final.34$year==2019,],main="females in 2019", ylim=c(0,12), xlab="weeks to hatching", las=1)


###########################################################

# distribution foraging trips
windows()
hist(df.for.trips.final.34$dur_tot, breaks=20, xlab='Duration foraging trip (h)', main='Distribution of foraging trips')
table(floor(df.for.trips.final.34$dur_tot))
table(df.for.trips.final.34$dur_tot>24) # only 116 trips >24 hours of a total of 3237 trips 
table(df.for.trips.final.34$dur_tot>24, df.for.trips.final.34$week_rel_hatching) # these are not necessarily trips late in the chick phase. 
df.for.trips.final.34[df.for.trips.final.34$dur_tot>24,c('birdID','start_time','distance.to.nest','dur_tot','week_rel_hatching')]
dim(df.for.trips.final.34)

df.for.trips.long.34 = df.for.trips.final.34[df.for.trips.final.34$dur_tot>24,] # shows that very long foraging trip 656 is due to logger removed and replaced by logger 1609, so we have to delete this trip, 82 hour trip in egg phase 647 should also be omitted as is only one datapoint in egg phase, note that we should also omit 1 trip 6117 of 29 hours
df.for.trips.final.34 = df.for.trips.final.34[df.for.trips.final.34$dur_tot<82,] # remove the 2 extremely long foraging trips (of 4 and 12 days...)
# NOW JUST DELETE ALL TRIPS< 29 HOURS
df.for.trips.final.34 = df.for.trips.final.34[df.for.trips.final.34$dur_tot<29,] # removes all foraging trips of >=29 hours.
hist(df.for.trips.final.34$dur_tot, breaks=20, xlab='Duration foraging trip (h)', main='Distribution of foraging trips')

# sex-specific distribution of foraging trip duration, specified per breeding phase:
windows(10,6)
layout(matrix(1:4,nrow=2,byrow=T))
multhist(list(df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase.nr==3],df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="F"&df.for.trips.final.34$breeding.phase.nr==3]),breaks=seq(0,30,2), xlab='Duration foraging trip (h)', main='Egg incubation', col=c("lightskyblue","lightcoral"))
multhist(list(df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase.nr==4],df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="F"&df.for.trips.final.34$breeding.phase.nr==4]),breaks=seq(0,30,2), xlab='Duration foraging trip (h)', main='Chick rearing', col=c("lightskyblue","lightcoral"))
# sex-specific distribution of foraging trip distance, specified per breeding phase
multhist(list(df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase.nr==3],df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="F"&df.for.trips.final.34$breeding.phase.nr==3]),breaks=seq(0,30,2), xlab='Foraging trip distance (km)', main='Egg incubation', col=c("lightskyblue","lightcoral"))
multhist(list(df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase.nr==4],df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="F"&df.for.trips.final.34$breeding.phase.nr==4]),breaks=seq(0,30,2), xlab='Foraging trip distance (km)', main='Chick rearing', col=c("lightskyblue","lightcoral"))

# Voordat ik had geselecteerd dat het foerageren daadwerkelijk op het water moest gebeuren, waren er heel veel hele korte foerageert trips (zowel in duur als afstand), deze zijn er nu uit. 


# interesting that dur_tot is very high for 6067 in 2015, but indeed seems to forage in freshwater 
df.for.trips.final.34$freq=1
Nr_for.trips.final.34_mean_dur_phase_year_bird = aggregate(freq~breeding.phase+sex+birdID+year, data=df.for.trips.final.34, sum)
# should also omit data egg phase when <3 trips are made in egg phase (647 (had 2 trips one 82h=already omitted), 6117 (egg 1 trip 29 hours already omitted), (6068 has only 2 trips)

# TO DO: I WOULD LIKE TO PLOT THE MEAN dur_tot PER BIRD PER YEAR PER week_rel_hatching to show decrease duration trips in females
windows()
layout(matrix(1:2, ncol=1))
par(mar=c(2,4,1,1), oma=c(2,0,0,0))
boxplot(dur_tot~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="F",], col="lightcoral", ylim=c(0,20))
boxplot(dur_tot~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="M",], col="lightskyblue", ylim=c(0,20))
mtext("Week relative to hatching",1,3)
mtext("Duration of foraging trip",2,-1, outer=T)

# assess the proportion of foraging in marine, freshwater and brackish in the foraging trips: 
windows()
hist(df.for.trips.final.34$prop_foraging_marine, breaks=20, xlab='prop foraging marine', main='Distribution of proportion marine per foraging trip', ylim=c(0,2500)) # shows mainly 0 or 100% trip in marine

windows()
hist(df.for.trips.final.34$prop_foraging_fresh, breaks=20, xlab='prop foraging freshwater', main='Distribution of proportion freshwater per foraging trip', ylim=c(0,2500)) # shows mainly 0 or 100% trip in fresh (actually very few 100% freshwater trips!)

windows()
hist(df.for.trips.final.34$prop_foraging_brackish, breaks=20, xlab='prop foraging brackish', main='Distribution of proportion brackish per foraging trip', ylim=c(0,2500)) # shows mainly 0 or 100% trip in brackish
## therefore it is reasonable to categorize the foraging trips in either of the three habitat categories, on the basis of the highest proportion foraging in that habitat during a specific trip:

df.for.trips.final.34$habitat_cat = ifelse(df.for.trips.final.34$prop_foraging_marine>df.for.trips.final.34$prop_foraging_fresh & df.for.trips.final.34$prop_foraging_marine>df.for.trips.final.34$prop_foraging_brackish, 'marine',ifelse(df.for.trips.final.34$prop_foraging_fresh>df.for.trips.final.34$prop_foraging_marine&df.for.trips.final.34$prop_foraging_fresh>df.for.trips.final.34$prop_foraging_brackish, 'freshwater',ifelse(df.for.trips.final.34$prop_foraging_brackish>df.for.trips.final.34$prop_foraging_marine&df.for.trips.final.34$prop_foraging_brackish>df.for.trips.final.34$prop_foraging_fresh,'brackish','mixed')))
table(df.for.trips.final.34$habitat_cat) # using this criterium, there was never a mixed-habitat foraging trip. (when using the criterium than more than 50% of the foraging should be in the defined habitat, then there are 9 mixed habitat trips.)

df.for.trips.final.34$habitat_cat_nr = 1
df.for.trips.final.34$habitat_cat_nr[df.for.trips.final.34$habitat_cat=='brackish'] = 2
df.for.trips.final.34$habitat_cat_nr[df.for.trips.final.34$habitat_cat=='marine'] = 3

### which to use: habitat_cat or habitat_cat_prop?

df.for.trips.final.34$hour_end = hour(df.for.trips.final.34$end_time)
df.for.trips.final.34$yday_start = yday(df.for.trips.final.34$start_time)
df.for.trips.final.34$yday_end = yday(df.for.trips.final.34$end_time)
df.for.trips.final.34$year = as.factor(df.for.trips.final.34$year)
df.for.trips.final.34$prop_foraging <- (df.for.trips.final.34$dur_foraging_marine+df.for.trips.final.34$dur_foraging_fresh+df.for.trips.final.34$dur_foraging_brackish)/df.for.trips.final.34$dur_tot
## tot_dur trip per habitat for Males during the egg phase (unit=foraging trip of which mean is taken per phase, however to each trip a habitat is assigned based on habitat_cat

class(df.for.trips.final.34$habitat_cat)
df.for.trips.final.34$habitat_cat_f = factor(df.for.trips.final.34$habitat_cat, levels=c('freshwater','brackish','marine'))

m.tripdur.sex <- lme(dur_tot~sex, df.for.trips.final.34, random=~1|birdID)
summary(m.tripdur.sex) # males make longer foraging trips than females...
m.fordur.trip.sex <- lme(dur_foraging~sex, df.for.trips.final.34, random=~1|birdID)
summary(m.fordur.trip.sex) # males (almost) forage longer during their trips. 
m.forprop.trip.sex <- lme(prop_foraging~sex, df.for.trips.final.34, random=~1|birdID)
summary(m.forprop.trip.sex) # but not proportionally to the length of their foraging trip. 

# nu kijken of dit zoetwater of zout/brakwater trips waren, voor alleen de mannen (aangezien vrouwen bijna exclusively in brakwater/marien milieu foerageren):
windows(10,6)
layout(matrix(1:4,nrow=2,byrow=T))
multhist(list(df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase.nr==3&df.for.trips.final.34$habitat_cat!="freshwater"],df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase.nr==3&df.for.trips.final.34$habitat_cat=="freshwater"]),breaks=seq(0,30,2), xlab='Duration foraging trip (h)', main='Egg incubation', col=c("blue","lightskyblue"), beside=F)
multhist(list(df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase.nr==4&df.for.trips.final.34$habitat_cat!="freshwater"],df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase.nr==4&df.for.trips.final.34$habitat_cat=="freshwater"]),breaks=seq(0,30,2), xlab='Duration foraging trip (h)', main='Chick rearing', col=c("blue","lightskyblue"), beside=F)
# sex-specific distribution of foraging trip distance, specified per breeding phase
multhist(list(df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase.nr==3&df.for.trips.final.34$habitat_cat!="freshwater"]/1000,df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase.nr==3&df.for.trips.final.34$habitat_cat=="freshwater"]/1000),breaks=seq(0,30,2), xlab='Foraging trip distance (km)', main='Egg incubation', col=c("blue","lightskyblue"), beside=F)
multhist(list(df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase.nr==4&df.for.trips.final.34$habitat_cat!="freshwater"]/1000,df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase.nr==4&df.for.trips.final.34$habitat_cat=="freshwater"]/1000),breaks=seq(0,30,2), xlab='Foraging trip distance (km)', main='Chick rearing', col=c("blue","lightskyblue"), beside=F)

# first make plots per phase
windows(10,8)
layout(matrix(1:4, ncol=2))
par(mar=c(4,4,0,0) ,oma=c(0,1,3,1))
boxplot(dur_tot~habitat_cat_f, data=df.for.trips.final.34[df.for.trips.final.34$breeding.phase=='3.eggs',], xlab='', ylab='', boxwex=0.5, col=c('lightblue','cadetblue','blue'),cex.lab=1.3, cex.axis=1.2, ylim=c(0,35), las=1, xaxt="n")
mtext('incubation-phase', 3, 1, cex=1.3)
mtext('Total duration foraging trip (h)', 2, 3.5, cex=1.2)
boxplot(prop_foraging~habitat_cat_f, data=df.for.trips.final.34[df.for.trips.final.34$breeding.phase=='3.eggs',], main="", xlab='', ylab='', col=c('lightblue','cadetblue','blue'),boxwex=0.5, cex.lab=1.3, las=1,cex.axis=1.2)
mtext('Proportion foraging per trip', 2, 3.5, cex=1.2)
boxplot(dur_tot~habitat_cat_f, data=df.for.trips.final.34[df.for.trips.final.34$breeding.phase=='4.chicks',], las=1, xlab='', ylab='', boxwex=0.5, col=c('lightblue','cadetblue','blue'),cex.lab=1.3, cex.axis=1.2, ylim=c(0,35), xaxt="n", yaxt="n")
mtext('chick-phase', 3, 1, cex=1.3)
axis(2, at=c(0,5,10,15,20,25,30,35),xaxt="n", tick=T, cex.axis=1.2, labels=F)
boxplot(prop_foraging~habitat_cat_f, data=df.for.trips.final.34[df.for.trips.final.34$breeding.phase=='4.chicks',],  las=1, xlab='', ylab='', col=c('lightblue','cadetblue','blue'),boxwex=0.5, cex.lab=1.3, cex.axis=1.2, main='', yaxt="n")
axis(2, at=c(0,0.2,0.4,0.6,0.8,1),xaxt="n", tick=T, cex.axis=1.2, labels=F)

n_trips_males_eggs <- dim(df.for.trips.final.34[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase=='3.eggs',])[1]
n_trips_females_eggs <- dim(df.for.trips.final.34[df.for.trips.final.34$sex=="F"&df.for.trips.final.34$breeding.phase=='3.eggs',])[1]
n_trips_males_chicks <- dim(df.for.trips.final.34[df.for.trips.final.34$sex=="M"&df.for.trips.final.34$breeding.phase=='4.chicks',])[1]
n_trips_females_chicks <- dim(df.for.trips.final.34[df.for.trips.final.34$sex=="F"&df.for.trips.final.34$breeding.phase=='4.chicks',])[1]

## max distance foraging trips per sex
windows()
boxplot(distance.to.nest/1000~sex, data=df.for.trips.final.34, main="", ylim=c(0,30), col=c("lightcoral","lightskyblue"), las=1, x.axt="n", cex.axis=1.3, xaxt="n", xlab="", ylab="")
mtext("Max distance to the nest during foraging trip (km)", 2, 2.5, cex=1.5)
text(0.5,30.5,'N=', cex=1.3, xpd=NA, adj=0)
text(0.935,30.5,n_trips_females_eggs+n_trips_females_chicks, cex=1.3, xpd=NA, adj=0)
text(1.9,30.5,n_trips_males_eggs+n_trips_males_chicks, cex=1.3, xpd=NA, adj=0)
text(0.865,-3.5,'females', cex=1.3, xpd=NA, adj=0)
text(1.855,-3.5,'males', cex=1.3, xpd=NA, adj=0)

# max distance foraging trips per sex and breeding phase
windows(8,6)
layout(matrix(1:2,ncol=2))
par(mar=c(4,1,1,1), oma=c(0,4,0,0))
boxplot(distance.to.nest/1000~breeding.phase, data=df.for.trips.final.34[df.for.trips.final.34$sex=="F",], main="", ylim=c(0,35), col="pink", las=1, cex.axis=1.3, xlab="")
text(0.5,35.5,'N=', cex=1.3, xpd=NA, adj=0)
text(1,35.5,n_trips_females_eggs, cex=1.3, xpd=NA, adj=0)
text(2,35.5,n_trips_females_chicks, cex=1.3, xpd=NA, adj=0)
boxplot(distance.to.nest/1000~breeding.phase, data=df.for.trips.final.34[df.for.trips.final.34$sex=="M",], main="", ylim=c(0,35), col="blue", las=1, cex.axis=1.3, yaxt='n', xlab="")
text(0.5,35.5,'N=', cex=1.3, xpd=NA, adj=0)
text(1,35.5,n_trips_males_eggs, cex=1.3, xpd=NA, adj=0)
text(2,35.5,n_trips_males_chicks, cex=1.3, xpd=NA, adj=0)
mtext("Max distance to the nest during foraging trip (km)", 2, 2.5, cex=1.5, outer=T)

# calculate mean distance of foraging trip per bird per breeding phase, as statistical unit
for.trip.dist.dur.bird.year <- aggregate(cbind(distance.to.nest, dur_tot) ~ breeding.phase + year + birdID + sex, df.for.trips.final.34, mean)

# plot average FORAGING DISTANCE per sex per breeding phase (only possible for eggs and chicks)
windows(4,6)
par(mar=c(6,6,3,1))
boxplot(distance.to.nest/1000~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="F",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, ylim=c(0,15), xlim=c(0.8,2.5), las=1, cex.axis=1.3)
boxplot(distance.to.nest/1000~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=1.3)
mtext("Foraging trip distance (km)", 2, 4, cex=1.5)
mtext("Breeding phase", 1, 4, cex=1.5)
legend("topleft", legend=c("females","males"), pch=22, pt.bg = c("lightcoral","lightskyblue"), cex=1.5, bty="n")
# show sample sizes! 
N.prop.for.phase.sex <- table(for.trip.dist.dur.bird.year$breeding.phase, for.trip.dist.dur.bird.year$sex)
axis(3, at=1:2, N.prop.for.phase.sex[,1], tick=F, cex.axis=1.3)
axis(3, at=1:2+0.3, N.prop.for.phase.sex[,2], tick=F, cex.axis=1.3)
axis(3, at=0.8, "N =", tick=F, cex.axis=1.3)

# plot average DURATION of foraging trips per sex per breeding phase (only possible for eggs and chicks)
windows(4,6)
par(mar=c(6,6,3,1))
boxplot(dur_tot~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="F",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,12), las=1, cex.axis=1.3)
boxplot(dur_tot~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=1.3)
mtext("Foraging trip duration (h)", 2, 4, cex=1.5)
mtext("Breeding phase", 1, 4, cex=1.5)
legend("topleft", legend=c("females","males"), pch=22, pt.bg = c("lightcoral","lightskyblue"), cex=1.5, bty="n")
# show sample sizes! 
axis(3, at=1:2, N.prop.for.phase.sex[,1], tick=F, cex.axis=1.3)
axis(3, at=1:2+0.3, N.prop.for.phase.sex[,2], tick=F, cex.axis=1.3)
axis(3, at=0.8, "N =", tick=F, cex.axis=1.3)


# STATISTCS DISTANCE TO NEST OF FORAGING TRIPS PER SEX PER STAGE
# (1) using mean foraging trip distance per bird per breeding phase per year as statistical unit
m_for_trip_dist  = lme(distance.to.nest~breeding.phase*sex, data=for.trip.dist.dur.bird.year, random=~1|birdID/year, method="ML")
dd.dist_trips = dredge(m_for_trip_dist)
# only sex is significant: (breeding phase is in the top model but with dAIC=0.3 no strong support)
m_for_trip_dist_pars <- lme(distance.to.nest~sex, data=for.trip.dist.dur.bird.year, random=~1|birdID/year, method="REML")
summary(m_for_trip_dist_pars) # males make foraging trips of on average 5 km further max distance than females. 
# (2) using foraging trip distance per foraging trip as statistical uni
m_for_trip_dist_all  = lme(distance.to.nest~breeding.phase*sex, data=df.for.trips.final.34, random=~1|birdID/year, method="ML")
dd.dist_trips_all = dredge(m_for_trip_dist_all)
# interaction between sex and breeding phase is best supported. 
m_for_trip_dist_all_pars <- lme(distance.to.nest~breeding.phase*sex, data=df.for.trips.final.34, random=~1|birdID/year, method="REML")
summary(m_for_trip_dist_all_pars) 
df.for.trips.final.34$pred.trip.dist.all <- predict(m_for_trip_dist_all_pars, df.for.trips.final.34, level=0)
unique(df.for.trips.final.34[,c("breeding.phase","sex","pred.trip.dist.all")]) # males make about 5 km longer trips than females (9.4 versus 4.4 km), and whereas females make about 1 km shorter trips during the chick phase than males, males only make 200 m shorter foraging trips after the chicks have hatched. 

# STATISTICS DURATION FORAGING TRIPS PER SEX PER STAGE
# (1) using mean foraging trip duration per bird per breeding phase per year as statistical unit
m_dur_trips_full  = lme(dur_tot~breeding.phase*sex, data=for.trip.dist.dur.bird.year, random=~1|birdID/year, method="ML")
dd.dur_trips = dredge(m_dur_trips_full) # support for an effect of both sex and breeding phase. 
m_for_trip_dur_pars <- lme(dur_tot~sex+breeding.phase, data=for.trip.dist.dur.bird.year, random=~1|birdID/year, method="REML")
summary(m_for_trip_dur_pars) # females make foraging trips of shorter duration than males.
# duration of foraging trips is (somewhat) shorter in chick phase than in egg phase, for both males and females
# (2) using duration of individual foraging trips as statistical unit
m_dur_trips__all  = lme(dur_tot~breeding.phase*sex, data=df.for.trips.final.34, random=~1|birdID/year, method="ML")
dd.dur_trips_all = dredge(m_dur_trips__all) # interaction between sex and breeding phase is best supported
m_for_trip_dur_all_pars <- lme(dur_tot~sex*breeding.phase, data=df.for.trips.final.34, random=~1|birdID/year, method="REML")
summary(m_for_trip_dur_all_pars) # females make shorter foraging trips (shorter duration) than males, duration of foraging trips decreases after chicks hatch, but more strongly in females than in males. 


#######################################
## ANALYZING AND PLOTTING THE NUMBER OF FORAGING TRIPS PER DAY PER BREEDING PHASE PER SEX
# unique days per breeding phase, bird and year (to know which yday there are in the dataset so that we do not omit days without foraging trips):
phase_yday_bird_year = unique(gps.breeding.data.behav[gps.breeding.data.behav$breeding.phase=='3.eggs'|gps.breeding.data.behav$breeding.phase=='4.chicks',c('year','birdID','sex','yday_CEST','breeding.phase')])
phase_yday_bird_year$yday_start = phase_yday_bird_year$yday_CEST
# days with number of foraging trips per bird per year (days without foraging trips are absent, therefore link with above dataframe phase_yday_bird_year)
df.for.trips.final.34$freq=1
## number of foraging trips per day per individual:
n_trips_day_bird_year = aggregate(freq~year+birdID+sex+yday_start, data=df.for.trips.final.34, sum)
## merge the two df's to get the 0's for days without foraging trips:
n_trips_day_bird_year_zeros = merge(phase_yday_bird_year, n_trips_day_bird_year, all.x=T)
n_trips_day_bird_year_zeros$freq[is.na(n_trips_day_bird_year_zeros$freq)]=0
names(n_trips_day_bird_year_zeros)[7]='n_trips'
n_trips_day_bird_year_mean = aggregate(n_trips~year+birdID+sex+breeding.phase, data=n_trips_day_bird_year_zeros, mean)
n_trips_day_bird_year_mean$year = as.factor(n_trips_day_bird_year_mean$year)
n_trips_day_bird_year_mean[order(n_trips_day_bird_year_mean$birdID, n_trips_day_bird_year_mean$year),]

# STATISTCS NR FORAGING TRIPS PER SEX PER STAGE
# (1) using mean number of foraging trips per 24 h per bird per breeding phase per year as statistical unit
m_for_trips_nr  = lme(n_trips~breeding.phase*sex, data=n_trips_day_bird_year_mean, random=~1|birdID/year, method="ML")
summary(m_for_trips_nr)
dd.dur_trips = dredge(m_for_trips_nr) # most parsimonious model has effects of sex and breeding phase
m_for_trips_nr_pars <- lme(n_trips~breeding.phase+sex, data=n_trips_day_bird_year_mean, random=~1|birdID/year, method="REML")
summary(m_for_trips_nr_pars) # significantly more foraging trips during chick phase than egg phase, and fewer by males than by females.
# (2) using the number of foraging trips per date per bird as statistical unit
m_for_trips_nr_all  = lme(n_trips~breeding.phase*sex, data=n_trips_day_bird_year_zeros, random=~1|birdID/year, method="ML")
dd.dur_trips_all = dredge(m_for_trips_nr_all) # most parsimonious model includes the intercation between sex and breeding phase
m_for_trips_nr_all_pars <- lme(n_trips~breeding.phase*sex, data=n_trips_day_bird_year_zeros, random=~1|birdID/year, method="REML")
summary(m_for_trips_nr_all_pars) # significantly more foraging trips during chick phase than egg phase, and fewer by males than by females.
fixef(m_for_trips_nr_all_pars)[1] # 1.7 trips per day by females during egg phase
fixef(m_for_trips_nr_all_pars)[1]+fixef(m_for_trips_nr_all_pars)[2] # 3.2 trips per day by females during chick phase
fixef(m_for_trips_nr_all_pars)[1]+fixef(m_for_trips_nr_all_pars)[3] # 1.2 trips per day by males during egg phase
fixef(m_for_trips_nr_all_pars)[1]+fixef(m_for_trips_nr_all_pars)[2]+fixef(m_for_trips_nr_all_pars)[3]+fixef(m_for_trips_nr_all_pars)[4] # 1.7 trips per day by males during chick phase
# Females increase the number of trips more than males when chicks have hatched. 


# NUMBER OF FORAGING TRIPS PER BREEDING PHASE PER SEX
windows(4,6)
par(mar=c(6,6,3,1))
boxplot(n_trips~breeding.phase, data=n_trips_day_bird_year_mean[n_trips_day_bird_year_mean$sex=="F",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,5), las=1, cex.axis=1.3)
boxplot(n_trips~breeding.phase, data=n_trips_day_bird_year_mean[n_trips_day_bird_year_mean$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=1.3)
mtext("Number of foraging trips per 24 hours", 2, 3, cex=1.5)
mtext("Breeding phase", 1, 4, cex=1.5)
legend("topleft", legend=c("females","males"), pch=22, pt.bg = c("lightcoral","lightskyblue"), cex=1.5, bty="n")
# show sample sizes! 
axis(3, at=1:2, N.prop.for.phase.sex[,1], tick=F, cex.axis=1.3)
axis(3, at=1:2+0.3, N.prop.for.phase.sex[,2], tick=F, cex.axis=1.3)
axis(3, at=0.8, "N =", tick=F, cex.axis=1.3)

# Now combine the three graphs into one
pdf("output/Fig4.pdf",12,8)
#windows(12,8)
par(mar=c(1,6,0,0), oma=c(5,0,3,1))
layout(matrix(1:3, ncol=3))
# number of foraging trips per 24h
boxplot(n_trips~breeding.phase, data=n_trips_day_bird_year_mean[n_trips_day_bird_year_mean$sex=="F",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,5), las=1, cex.axis=2)
boxplot(n_trips~breeding.phase, data=n_trips_day_bird_year_mean[n_trips_day_bird_year_mean$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=2)
mtext("Number of foraging trips per 24 hours", 2, 3.5, cex=1.5)
legend("topleft", legend=c("females","males"), pch=22, pt.bg = c("lightcoral","lightskyblue"), cex=2, bty="n")
# show sample sizes! 
axis(3, at=1:2, N.prop.for.phase.sex[,1], tick=F, cex.axis=1.8)
axis(3, at=1:2+0.3, N.prop.for.phase.sex[,2], tick=F, cex.axis=1.8)
axis(3, at=0.8, "N =", tick=F, cex.axis=1.8)
# duration of foraging trips
boxplot(dur_tot~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="F",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,12), las=1, cex.axis=2)
boxplot(dur_tot~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=2)
mtext("Foraging trip duration (h)", 2, 3.5, cex=1.5)
# distance of foraging trips
boxplot(distance.to.nest/1000~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="F",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, ylim=c(0,15), xlim=c(0.8,2.5), las=1, cex.axis=2)
boxplot(distance.to.nest/1000~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=2)
mtext("Foraging trip distance (km)", 2, 3.5, cex=1.5)
mtext("Breeding phase", 1, 3, outer=T, cex=1.5)
dev.off()

# does the difference in foraging trip distance between the sexes remain significant when only including foraging trips to marine foraging grounds? (as an indication that more than sex-specific habitat preferences plays a role in shaping sexual differences in foraging trip distance)

# re-calculate mean distance and duration of foraging trip per bird per breeding phase, only for marine foraging trips. To exlude trips where birds were resting on the mainland, and subsequently foraging in marine waters, we only selected trips where the birds did not visit the mainland: 
df.marine.for.trips <- df.for.trips.final.34[df.for.trips.final.34$dur_mainland==0&df.for.trips.final.34$habitat_cat_f!="freshwater",]
marine.for.trip.dist.dur.bird.year <- aggregate(cbind(distance.to.nest, dur_tot) ~ breeding.phase + year + birdID + sex, df.marine.for.trips, mean)
# calculate the number of marine foraging trips per day:
## number of foraging trips per day per individual:
n_marine_trips_day_bird_year = aggregate(freq~year+birdID+sex+yday_start, data=df.marine.for.trips, sum)
n_marine_trips_day_bird_year_zeros = merge(phase_yday_bird_year, n_marine_trips_day_bird_year, all.x=T)
n_marine_trips_day_bird_year_zeros$freq[is.na(n_marine_trips_day_bird_year_zeros$freq)]=0
names(n_marine_trips_day_bird_year_zeros)[7]='n_marine_trips'
n_marine_trips_day_bird_year_mean = aggregate(n_marine_trips~year+birdID+sex+breeding.phase, data=n_marine_trips_day_bird_year_zeros, mean)
n_marine_trips_day_bird_year_mean$year = as.factor(n_marine_trips_day_bird_year_mean$year)
n_marine_trips_day_bird_year_mean[order(n_marine_trips_day_bird_year_mean$birdID, n_marine_trips_day_bird_year_mean$year),]

pdf("output/FigS3.pdf", 12, 8)
#windows(12,8)
par(mar=c(1,6,0,0), oma=c(5,0,3,1))
layout(matrix(1:3, ncol=3))
# number of foraging marine_trips per 24h
boxplot(n_marine_trips~breeding.phase, data=n_marine_trips_day_bird_year_mean[n_marine_trips_day_bird_year_mean$sex=="F",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,5), las=1, cex.axis=2)
boxplot(n_marine_trips~breeding.phase, data=n_marine_trips_day_bird_year_mean[n_marine_trips_day_bird_year_mean$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=2)
mtext("Number of marine foraging trips per 24 hours", 2, 3.5, cex=1.5)
legend("topleft", legend=c("females","males"), pch=22, pt.bg = c("lightcoral","lightskyblue"), cex=2, bty="n")
# show sample sizes! 
axis(3, at=1:2, N.prop.for.phase.sex[,1], tick=F, cex.axis=1.8)
axis(3, at=1:2+0.3, N.prop.for.phase.sex[,2], tick=F, cex.axis=1.8)
axis(3, at=0.8, "N =", tick=F, cex.axis=1.8)
# duration of foraging marine_trips
boxplot(dur_tot~breeding.phase, data=marine.for.trip.dist.dur.bird.year[marine.for.trip.dist.dur.bird.year$sex=="F",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,12), las=1, cex.axis=2)
boxplot(dur_tot~breeding.phase, data=marine.for.trip.dist.dur.bird.year[marine.for.trip.dist.dur.bird.year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=2)
mtext("Marine foraging trip duration (h)", 2, 3.5, cex=1.5)
# distance of foraging marine_trips
boxplot(distance.to.nest/1000~breeding.phase, data=marine.for.trip.dist.dur.bird.year[marine.for.trip.dist.dur.bird.year$sex=="F",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, ylim=c(0,15), xlim=c(0.8,2.5), las=1, cex.axis=2)
boxplot(distance.to.nest/1000~breeding.phase, data=marine.for.trip.dist.dur.bird.year[marine.for.trip.dist.dur.bird.year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=2)
mtext("Marine foraging trip distance (km)", 2, 3.5, cex=1.5)
mtext("Breeding phase", 1, 3, outer=T, cex=1.5)
dev.off()

# the DISTANCE of marine foraging trips seems still slightly longer in males; what do the statistics say?
m_marine_for_trip_dist_all  = lme(distance.to.nest~breeding.phase*sex, data=df.marine.for.trips, random=~1|birdID/year, method="ML")
dd.dist_marine_trips_all = dredge(m_marine_for_trip_dist_all) 
# the interaction is supported, but not the model with the sex effect alone. 
m_marine_for_trip_dist_all_pars  = lme(distance.to.nest~breeding.phase*sex, data=df.marine.for.trips, random=~1|birdID/year, method="REML")
summary(m_marine_for_trip_dist_all_pars) 
m_marine_for_trip_dist_all_pars  = lme(distance.to.nest~breeding.phase+sex, data=df.marine.for.trips, random=~1|birdID/year, method="REML")
lsmeans(m_marine_for_trip_dist_all_pars, pairwise~sex*breeding.phase) # the only significant pairwise difference is that females make shorter distance trips during the chick than during the egg phase. The difference between males and females is not significant. The significant interaction should does be interpreted as: females decrease the distance of their marine foraging trips when chicks hatch, but males do not. 

# what about marine foraging trip DURATION?
m_marine_for_trip_dur_all  = lme(dur_tot~breeding.phase*sex, data=df.marine.for.trips, random=~1|birdID/year, method="ML")
dd.dur_marine_trips_all = dredge(m_marine_for_trip_dur_all) 
# only breeding phase is supported: 
m_marine_for_trip_dur_all_pars  = lme(dur_tot~breeding.phase, data=df.marine.for.trips, random=~1|birdID/year, method="REML")
summary(m_marine_for_trip_dur_all_pars) # about 1 hour shorter trips during chick than during egg phase. 

# is the increase in the NUMBER of marine foraging trips significant for both males and females?
m_marine_for_trips_nr_all  = lme(n_marine_trips~breeding.phase*sex, data=n_marine_trips_day_bird_year_zeros, random=~1|birdID/year, method="ML")
dd.dur_marine_trips_all = dredge(m_marine_for_trips_nr_all) # most parsimonious model includes the intercation between sex and breeding phase
m_marine_for_trips_nr_all_pars <- lme(n_marine_trips~breeding.phase*sex, data=n_marine_trips_day_bird_year_zeros, random=~1|birdID/year, method="REML")
summary(m_for_trips_nr_all_pars) # significantly more foraging trips during chick phase than egg phase, and fewer by males than by females.
lsmeans(m_for_trips_nr_all_pars, pairwise~breeding.phase*sex) # both males and females make significantly more marine foraging trips during the chick than during the egg phase. But the increase is stronger in females than in males. The latter keeps making freshwater foraging trips as well.  
