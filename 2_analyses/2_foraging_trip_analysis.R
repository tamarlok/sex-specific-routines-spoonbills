source("functions.R") # to have the most updated version of the functions
gps.breeding.data.behav <- gps.breeding.data.behav.sel

### determine a circle of 50 m around the nest. If a bird moves outside this 50m range, we assume it is on a foraging trip (note that resting periods away from the colony are included in the foraging trip):
gps.breeding.data.behav$at_colony_50m = 0
gps.breeding.data.behav$at_colony_50m[gps.breeding.data.behav$dist.nest1<50|gps.breeding.data.behav$dist.nest2<50]=1 # voor nu wordt 50m rondom de tweede nestpoging ook meegeteld, niet echt netjes, maar dit zal vrijwel geen effect hebben op het resultaat. Dat heeft het dus wel aangezien bij bijvoorbeeld 656 in 2013 het tweede "zogenaamde" nest in het Lauwersmeer ligt!

# ik weet niet hoe lat.nest en lon.nest in de df terecht zijn gekomen, maar ervan uitgaande dat dit de correcte nestlocatie is, bereken ik hier opnieuw de afstand tot het nest:
gps.breeding.data.behav$dist.nest = 100000 # far out of the range of the real values, so easily recognizable (and NA's are not allowed in the below loop to assign foraging trips)
gps.breeding.data.behav$dist.nest[is.na(gps.breeding.data.behav$lon.nest)==F] <- distCosine(as.matrix(gps.breeding.data.behav[,c('lon_rnd','lat_rnd')][is.na(gps.breeding.data.behav$lon.nest)==F,]), as.matrix(gps.breeding.data.behav[,c('lon.nest','lat.nest')][is.na(gps.breeding.data.behav$lon.nest)==F,]), r=6378137) # gives the distance in meters

na.omit(unique(gps.breeding.data.behav[,c('year','birdID','attempt','lat.nest','lon.nest','breeding.phase')]))

gps.breeding.data.behav$foraging_trip = 0

### LOOP TO DETERMINE ALL FORAGING TRIPS #### (TAKES A LONG TIME! CAN BE SKIPPED, THEN LOAD THE RDATA-FILE UNDER THE LOOP
### heel belangrijk dat df.sel goed gesorteerd is, op jaar, birdID (zelfde voor 656, 1609 en 6282) en date_time!
gps.breeding.data.behav <- gps.breeding.data.behav[order(gps.breeding.data.behav$year, gps.breeding.data.behav$birdID, gps.breeding.data.behav$date_time),]
unique(gps.breeding.data.behav$birdID)

for(bird in unique(gps.breeding.data.behav$birdID)) {
  ## per year
  for (year in unique(gps.breeding.data.behav$year[gps.breeding.data.behav$birdID==bird])) {
    print(paste(bird, year))
    foraging.trip=0
    ## make temporary df for selected birdID and year:
    df.tmp = gps.breeding.data.behav[gps.breeding.data.behav$birdID==bird&gps.breeding.data.behav$year==year,]
    # determine if the first fix of a bird in a specific year is foraging or at the colony:
    if (df.tmp$dist.nest[1]>=50) {
      df.tmp$foraging_trip[1]=1
      foraging.trip=1
    }
    for (i in 2:dim(df.tmp)[1]) {
      if (df.tmp$dist.nest[i]<50 & df.tmp$dist.nest[i-1]<50) df.tmp$foraging_trip[i]=0 # bird is still at the colony
      if (df.tmp$dist.nest[i]>=50 & df.tmp$dist.nest[i-1]<50) {
        df.tmp$foraging_trip[i]=foraging.trip + 1 # bird starts a new foraging trip
        foraging.trip = foraging.trip + 1
        }
      if (df.tmp$dist.nest[i]>=50 & df.tmp$dist.nest[i-1]>=50) df.tmp$foraging_trip[i]=df.tmp$foraging_trip[i-1] # the bird is still on the same foraging trip
      if (df.tmp$dist.nest[i]<50 & df.tmp$dist.nest[i-1]>=50) df.tmp$foraging_trip[i]=0 # bird returns to the colony from a foraging trip
    }
  ## paste the data of this bird into the df
    gps.breeding.data.behav$foraging_trip[gps.breeding.data.behav$birdID==bird&gps.breeding.data.behav$year==year] = df.tmp$foraging_trip  
    }
}
head(gps.breeding.data.behav)  
save.image('data/processed/Foraging.trip.data.0524.RData')


load('Breeding.phases.data.sel.trips_CEST3_13122018.RData')

gps.breeding.data.behav$year = as.factor(gps.breeding.data.behav$year)

df.for_trips = gps.breeding.data.behav[gps.breeding.data.behav$foraging_trip>0,]

df.for_trips_phase_34 = df.for_trips[df.for_trips$breeding.phase=='3.eggs'|df.for_trips$breeding.phase=='4.chicks',]

# check that foraging is only possible in water:
table(df.for_trips_phase_34$habitat[df.for_trips_phase_34$behaviour=='foraging'])
# we did not use this criteria: foraging here is also possible on the Schier_Kwelder for example... 
# however, birds, and particularly males, often make short trips on the saltmarsh to search for nestmaterial. 

df.for_trips_phase_34$dur_foraging = ifelse(df.for_trips_phase_34$behaviour=='foraging',df.for_trips_phase_34$duration/60,0) # gives duration foraging in hours
df.for_trips_phase_34$dur_foraging_marine = ifelse(df.for_trips_phase_34$behaviour=='foraging'&df.for_trips_phase_34$habitat_salinity=='marine',df.for_trips_phase_34$duration/60,0)
df.for_trips_phase_34$dur_foraging_freshwater = ifelse(df.for_trips_phase_34$behaviour=='foraging'&df.for_trips_phase_34$habitat_salinity=='freshwater',df.for_trips_phase_34$duration/60,0)
df.for_trips_phase_34$dur_foraging_brackish = ifelse(df.for_trips_phase_34$behaviour=='foraging'&df.for_trips_phase_34$habitat_salinity=='brackish',df.for_trips_phase_34$duration/60,0)

# next gives unique coordinates different nests per year per bird
nest_coords = na.omit(unique(df.for_trips_phase_34[,c('year','birdID','attempt','lon.nest','lat.nest')]))
df.for_trips_phase_34 = merge(df.for_trips_phase_34[,c('year','yday_CEST','birdID','date_time','week','day_rel_hatching','week_rel_hatching','latitude','longitude','speed_2d','time_to_previous','time_to_next','duration','habitat','distance.to.prev','distance.to.next','behaviour','sex','habitat_salinity','breeding.phase','attempt','at_colony_50m','foraging_trip','dur_foraging','dur_foraging_marine','dur_foraging_freshwater','dur_foraging_brackish')], nest_coords, by=c('year','birdID','attempt'))
# deze truc moet gedaan worden omdat nestlocatie blijkbaar niet goed gekoppeld is voor alle punten (misschien omdat volgens de definitie van minimaal 5 uur op het nest, bepaalde data niet als breeding worden gezien, en alleen voor deze data de lat.nest en lon.nest al gekoppeld zijn)
df.for_trips_phase_34$distance.to.nest = distCosine(as.matrix(df.for_trips_phase_34[,c('longitude','latitude')]), as.matrix(df.for_trips_phase_34[,c('lon.nest','lat.nest')]), r=6378.137) # in km

max.distance.year.bird.trip = aggregate(distance.to.nest ~ year + birdID + foraging_trip, data=df.for_trips_phase_34, max) 
dur_foraging_per_trip = aggregate(cbind(dur_foraging, dur_foraging_marine, dur_foraging_freshwater, dur_foraging_brackish)~year+birdID+sex+foraging_trip, data=df.for_trips_phase_34, sum) # hier is nu dus breeding.phase weg gehaald
dur_foraging_per_trip$dur_foraging_water = dur_foraging_per_trip$dur_foraging_marine+dur_foraging_per_trip$dur_foraging_freshwater+dur_foraging_per_trip$dur_foraging_brackish
dur_foraging_per_trip$prop_foraging_marine = dur_foraging_per_trip$dur_foraging_marine/dur_foraging_per_trip$dur_foraging
dur_foraging_per_trip$prop_foraging_brackish = dur_foraging_per_trip$dur_foraging_brackish/dur_foraging_per_trip$dur_foraging
dur_foraging_per_trip$prop_foraging_fresh = dur_foraging_per_trip$dur_foraging_freshwater/dur_foraging_per_trip$dur_foraging
start_time_of_trips = aggregate(date_time-0.5*time_to_previous~year+birdID+foraging_trip, data=df.for_trips_phase_34, min)
names(start_time_of_trips)[4] = 'start_time'
end_time_of_trips = aggregate(date_time+0.5*time_to_next~year+birdID+foraging_trip, data=df.for_trips_phase_34, max)
names(end_time_of_trips)[4] = 'end_time'
day_week_rel_hatching_trips = aggregate(cbind(day_rel_hatching, week_rel_hatching, week,yday_CEST)~year+birdID+foraging_trip, data=df.for_trips_phase_34, min)
trip_info = cbind(start_time_of_trips,end_time=end_time_of_trips$end_time,distance.to.nest=max.distance.year.bird.trip$distance.to.nest, day_week_rel_hatching_trips[,c('day_rel_hatching', 'week_rel_hatching', 'week','yday_CEST')])
df.for.trips.final.34 = merge(dur_foraging_per_trip, trip_info)
df.for.trips.final.34$dur_tot = round(as.numeric(df.for.trips.final.34$end_time-df.for.trips.final.34$start_time),2)/60
dim(df.for.trips.final.34)
dim(df.for.trips.final.34[df.for.trips.final.34$dur_foraging==0,]) ## in 2324 of the 7778 trips, birds were not (registered to be) foraging.
dim(df.for.trips.final.34[df.for.trips.final.34$dur_foraging_water==0,]) ## in 3506 of the 7778 trips, birds were not (registered to be) foraging in water (the only habitat where foraging is possible)
table(round(df.for.trips.final.34$dur_tot[df.for.trips.final.34$dur_foraging_water==0],1)) ## these were generally short trips, of less than half an hour.

df.for.trips.final.34 = df.for.trips.final.34[df.for.trips.final.34$dur_foraging_water>0,] # remove foraging trips with a foraging duration in water of 0 (BESLISSING MAKEN OF WE DE AANVULLENDE CRITERIUM DAT ZE IN HET WATER MOETEN FOERAGEREN WEL MEE MOETEN NEMEN)

dim(df.for.trips.final.34) # N=4174 trips left
head(df.for.trips.final.34)
head(ydays.year.bird.phase) #df with unique  yday and breeding stage 
# merge dataframe foraging trips (df.for.trips.final.34) with "ydays.year.bird.phase" to add breeding.phase to df.for.trips.final.34

## WAAR IS ydays.year.bird.phase?
df.for.trips.final.34 = merge(df.for.trips.final.34, ydays.year.bird.phase[,c('year','birdID','yday_CEST','breeding.phase.nr')])
df.for.trips.final.34$breeding.phase = '3.eggs' # make all egg phase
df.for.trips.final.34$breeding.phase[df.for.trips.final.34$breeding.phase.nr==4] = '4.chicks' # convert part to chick phase


# PUT NEXT GRAPHS IN FORAGING DURATION SCRIPT

windows(10,6)
layout(matrix(1:6, ncol=3, byearow=T))
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$year==2013,],main="males in 2013", ylim=c(0,12), xlab="weeks to hatching", ylab="total duration foraging (h) in Waddensea", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$year==2014,],main="males in 2014", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$year==2015,],main="males in 2015", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$year==2016,],main="males in 2016", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$year==2017,],main="males in 2017", ylim=c(0,12), xlab="weeks to hatching", las=1)

# the long foraging duration in marine waters by males in 2017 is driven by 6118, who was mainly foraging on Groninger Wad.
aggregate(dur_foraging_marine~week_rel_hatching+birdID, data=df.for.trips.final.34[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$year==2017,], mean)

windows(10,6)
layout(matrix(c(1:2,0,3:4,0), ncol=3, byearow=T))
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="f"&df.for.trips.final.34$year==2013,],main="females in 2013", ylim=c(0,7), xlab="weeks to hatching", ylab="total daily duration foraging (h) in Waddensea", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="f"&df.for.trips.final.34$year==2014,],main="females in 2014", ylim=c(0,7), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="f"&df.for.trips.final.34$year==2016,],main="females in 2016", ylim=c(0,7), xlab="weeks to hatching", las=1)
boxplot(dur_foraging_marine~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="f"&df.for.trips.final.34$year==2017,],main="females in 2017", ylim=c(0,7), xlab="weeks to hatching", las=1)


###########################################################

# distribution foraging trips
windows()
hist(df.for.trips.final.34$dur_tot, breaks=20, xlab='Duration foraging trip (h)', main='Distribution of foraging trips')
table(floor(df.for.trips.final.34$dur_tot)) # only 17 trips> 24 hours of a total of 3579 trips = 0.005% so we could omit them, but since they contain information we leave them in the dataset (but 1 of 62 hours, 82 hours, and 284 hours...). I would advice to remove the latter 3... 
dim(df.for.trips.final.34)

df.for.trips.long.34 = df.for.trips.final.34[df.for.trips.final.34$dur_tot>24,] # shows that very long foraging trip 656 is due to logger removed and replaced by logger 1609, so we have to delete this trip, 82 hour trip in egg phase 647 should also be omitted as is only one datapoint in egg phase, note that we should also omit 1 trip 6117 of 29 hours
df.for.trips.final.34 = df.for.trips.final.34[df.for.trips.final.34$dur_tot<82,] # remove the 2 extremely long foraging trips (of 4 and 12 days...)
# NOW JUST DELETE ALL TRIPS< 29 HOURS
df.for.trips.final.34 = df.for.trips.final.34[df.for.trips.final.34$dur_tot<29,] # removes all foraging trips of >=29 hours.
hist(df.for.trips.final.34$dur_tot, breaks=20, xlab='Duration foraging trip (h)', main='Distribution of foraging trips')

# sex-specific distribution of foraging trip duration, specified per breeding phase:
windows(10,6)
layout(matrix(1:4,nrow=2,byearow=T))
multhist(list(df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$breeding.phase.nr==3],df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="f"&df.for.trips.final.34$breeding.phase.nr==3]),breaks=seq(0,30,2), xlab='Duration foraging trip (h)', main='Egg incubation', col=c("lightskyblue","lightcoral"))
multhist(list(df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$breeding.phase.nr==4],df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="f"&df.for.trips.final.34$breeding.phase.nr==4]),breaks=seq(0,30,2), xlab='Duration foraging trip (h)', main='Chick rearing', col=c("lightskyblue","lightcoral"))
# sex-specific distribution of foraging trip distance, specified per breeding phase
multhist(list(df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$breeding.phase.nr==3],df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="f"&df.for.trips.final.34$breeding.phase.nr==3]),breaks=seq(0,30,2), xlab='Foraging trip distance (km)', main='Egg incubation', col=c("lightskyblue","lightcoral"))
multhist(list(df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$breeding.phase.nr==4],df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="f"&df.for.trips.final.34$breeding.phase.nr==4]),breaks=seq(0,30,2), xlab='Foraging trip distance (km)', main='Chick rearing', col=c("lightskyblue","lightcoral"))

# Voordat ik had geselecteerd dat het foerageren daadwerkelijk op het water moest gebeuren, waren er heel veel hele korte foerageert trips (zowel in duur als afstand), deze zijn er nu uit. 


# interesting that dur_tot is very high for 6067 in 2015, but indeed seems to forage in freshwater 
df.for.trips.final.34$freq=1
Nr_for.trips.final.34_mean_dur_phase_year_bird = aggregate(freq~breeding.phase+sex+birdID+year, data=df.for.trips.final.34, sum)
# should also omit data egg phase when <3 trips are made in egg phase (647 (had 2 trips one 82h=already omitted), 6117 (egg 1 trip 29 hours already omitted), (6068 has only 2 trips)

# TO DO: I WOULD LIKE TO PLOT THE MEAN dur_tot PER BIRD PER YEAR PER week_rel_hatching to show decrease duration trips in females
windows()
layout(matrix(1:2, ncol=1))
par(mar=c(2,4,1,1), oma=c(2,0,0,0))
boxplot(dur_tot~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="f",], col="red", ylim=c(0,20))
boxplot(dur_tot~week_rel_hatching, data=df.for.trips.final.34[df.for.trips.final.34$sex=="m",], col="blue", ylim=c(0,20))
mtext("Week relative to hatching",1,3)
mtext("Duration of foraging trip",2,-1, outer=T)

# assess the proportion of foraging in marine, freshwater and brackish in the foraging trips: 
windows()
hist(df.for.trips.final.34$prop_foraging_marine, breaks=20, xlab='prop foraging marine', main='Distribution of proportion marine per foraging trip') # shows mainly 0 or 100% trip in marine

windows()
hist(df.for.trips.final.34$prop_foraging_fresh, breaks=20, xlab='prop foraging freshwater', main='Distribution of proportion freshwater per foraging trip') # shows mainly 0 or 100% trip in fresh


windows()
hist(df.for.trips.final.34$prop_foraging_brackish, breaks=20, xlab='prop foraging brackish', main='Distribution of proportion brackish per foraging trip')# shows mainly 0 or 100% trip in brackish
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
df.for.trips.final.34$prop_foraging <- df.for.trips.final.34$prop_foraging_marine+df.for.trips.final.34$prop_foraging_fresh+df.for.trips.final.34$prop_foraging_brackish
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
layout(matrix(1:4,nrow=2,byearow=T))
multhist(list(df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$breeding.phase.nr==3&df.for.trips.final.34$habitat_cat!="freshwater"],df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$breeding.phase.nr==3&df.for.trips.final.34$habitat_cat=="freshwater"]),breaks=seq(0,30,2), xlab='Duration foraging trip (h)', main='Egg incubation', col=c("blue","lightskyblue"), beside=F)
multhist(list(df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$breeding.phase.nr==4&df.for.trips.final.34$habitat_cat!="freshwater"],df.for.trips.final.34$dur_tot[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$breeding.phase.nr==4&df.for.trips.final.34$habitat_cat=="freshwater"]),breaks=seq(0,30,2), xlab='Duration foraging trip (h)', main='Chick rearing', col=c("blue","lightskyblue"), beside=F)
# sex-specific distribution of foraging trip distance, specified per breeding phase
multhist(list(df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$breeding.phase.nr==3&df.for.trips.final.34$habitat_cat!="freshwater"],df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$breeding.phase.nr==3&df.for.trips.final.34$habitat_cat=="freshwater"]),breaks=seq(0,30,2), xlab='Foraging trip distance (km)', main='Egg incubation', col=c("blue","lightskyblue"), beside=F)
multhist(list(df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$breeding.phase.nr==4&df.for.trips.final.34$habitat_cat!="freshwater"],df.for.trips.final.34$distance.to.nest[df.for.trips.final.34$sex=="m"&df.for.trips.final.34$breeding.phase.nr==4&df.for.trips.final.34$habitat_cat=="freshwater"]),breaks=seq(0,30,2), xlab='Foraging trip distance (km)', main='Chick rearing', col=c("blue","lightskyblue"), beside=F)

# first make plots per phase
windows(10,8)
layout(matrix(1:4, ncol=2))
par(mar=c(4,4,0,0) ,oma=c(0,1,3,1))
boxplot(dur_tot~habitat_cat_f, data=df.for.trips.final.34[df.for.trips.final.34$breeding.phase=='3.eggs',], ylab='', boxwex=0.5, col=c('lightblue','cadetblue','blue'),cex.lab=1.3, cex.axis=1.2, ylim=c(0,35), las=1,xaxt="n")
mtext('incubation-phase', 3, 1, cex=1.3)
mtext('Total duration foraging trip (h)', 2, 3.5, cex=1.2)

boxplot(prop_foraging~habitat_cat_f, data=df.for.trips.final.34[df.for.trips.final.34$breeding.phase=='3.eggs',], main="",ylab='', col=c('lightblue','cadetblue','blue'),boxwex=0.5, cex.lab=1.3, las=1,cex.axis=1.2)
mtext('Proportion foraging per trip', 2, 3.5, cex=1.2)

boxplot(dur_tot~habitat_cat_f, data=df.for.trips.final.34[df.for.trips.final.34$breeding.phase=='4.chicks',], las=1,ylab='', boxwex=0.5, col=c('lightblue','cadetblue','blue'),cex.lab=1.3, cex.axis=1.2, ylim=c(0,35), xaxt="n", yaxt="n")
mtext('chick-phase', 3, 1, cex=1.3)
axis(2, at=c(0,5,10,15,20,25,30,35),xaxt="n", tick=T, cex.axis=1.2, labels=F)

boxplot(prop_foraging~habitat_cat_f, data=df.for.trips.final.34[df.for.trips.final.34$breeding.phase=='4.chicks',],  las=1,ylab='', col=c('lightblue','cadetblue','blue'),boxwex=0.5, cex.lab=1.3, cex.axis=1.2, main='', yaxt="n")
axis(2, at=c(0,0.2,0.4,0.6,0.8,1),xaxt="n", tick=T, cex.axis=1.2, labels=F)

n_trips_males_eggs <- dim(df.for.trips.final.34[df.for.trips.final.34$sex=='m'&df.for.trips.final.34$breeding.phase=='3.eggs',])[1]
n_trips_females_eggs <- dim(df.for.trips.final.34[df.for.trips.final.34$sex=='f'&df.for.trips.final.34$breeding.phase=='3.eggs',])[1]
n_trips_males_chicks <- dim(df.for.trips.final.34[df.for.trips.final.34$sex=='m'&df.for.trips.final.34$breeding.phase=='4.chicks',])[1]
n_trips_females_chicks <- dim(df.for.trips.final.34[df.for.trips.final.34$sex=='f'&df.for.trips.final.34$breeding.phase=='4.chicks',])[1]

## max distance foraging trips per sex
windows()
boxplot(distance.to.nest~sex, data=df.for.trips.final.34[df.for.trips.final.34$distance.to.nest<100,], main="", ylim=c(0,45), col=c("pink","blue"), las=1, x.axt="n", cex.axis=1.3, xaxt="n")
mtext("Max distance to the nest during foraging trip (km)", 2, 2.5, cex=1.5)
text(0.5,45.5,'N=', cex=1.3, xpd=NA, adj=0)
text(0.935,45.5,n_trips_females_eggs+n_trips_females_chicks, cex=1.3, xpd=NA, adj=0)
text(1.9,45.5,n_trips_males_eggs+n_trips_males_chicks, cex=1.3, xpd=NA, adj=0)
text(0.865,-3.5,'females', cex=1.3, xpd=NA, adj=0)
text(1.855,-3.5,'males', cex=1.3, xpd=NA, adj=0)

# max distance foraging trips per sex and breeding phase
windows(8,6)
layout(matrix(1:2,ncol=2))
par(mar=c(4,1,1,1), oma=c(0,4,0,0))
boxplot(distance.to.nest~breeding.phase, data=df.for.trips.final.34[df.for.trips.final.34$sex=='f',], main="", ylim=c(0,35), col="pink", las=1, cex.axis=1.3)
text(0.5,35.5,'N=', cex=1.3, xpd=NA, adj=0)
text(1,35.5,n_trips_females_eggs, cex=1.3, xpd=NA, adj=0)
text(2,35.5,n_trips_females_chicks, cex=1.3, xpd=NA, adj=0)
boxplot(distance.to.nest~breeding.phase, data=df.for.trips.final.34[df.for.trips.final.34$sex=='m',], main="", ylim=c(0,35), col="blue", las=1, cex.axis=1.3, yaxt='n')
text(0.5,35.5,'N=', cex=1.3, xpd=NA, adj=0)
text(1,35.5,n_trips_males_eggs, cex=1.3, xpd=NA, adj=0)
text(2,35.5,n_trips_males_chicks, cex=1.3, xpd=NA, adj=0)
mtext("Max distance to the nest during foraging trip (km)", 2, 2.5, cex=1.5, outer=T)

# calculate mean distance of foraging trip per bird per breeding phase, as statistical unit
for.trip.dist.dur.bird.year <- aggregate(cbind(distance.to.nest, dur_tot) ~ breeding.phase + year + birdID + sex, df.for.trips.final.34, mean)

# plot average FORAGING DISTANCE per sex per breeding phase (only possible for eggs and chicks)
windows(4,6)
par(mar=c(6,6,3,1))
boxplot(distance.to.nest~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="f",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, ylim=c(0,15), xlim=c(0.8,2.5), las=1, cex.axis=1.3)
boxplot(distance.to.nest~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="m",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
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
boxplot(dur_tot~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="f",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,12), las=1, cex.axis=1.3)
boxplot(dur_tot~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="m",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=1.3)
mtext("Foraging trip duration (h)", 2, 4, cex=1.5)
mtext("Breeding phase", 1, 4, cex=1.5)
legend("topleft", legend=c("females","males"), pch=22, pt.bg = c("lightcoral","lightskyblue"), cex=1.5, bty="n")
# show sample sizes! 
axis(3, at=1:2, N.prop.for.phase.sex[,1], tick=F, cex.axis=1.3)
axis(3, at=1:2+0.3, N.prop.for.phase.sex[,2], tick=F, cex.axis=1.3)
axis(3, at=0.8, "N =", tick=F, cex.axis=1.3)


# STATISTCS DISTANCE TO NEST OF FORAGING TRIPS PER SEX PER STAGE
head(for.trip.dist.dur.bird.year)
hist(for.trip.dist.dur.bird.year$distance.to.nest)
m_for_trip_dist  = lme(distance.to.nest~breeding.phase*sex, data=for.trip.dist.dur.bird.year, random=~1|birdID/year, method="ML")
dd.dist_trips = dredge(m_for_trip_dist)
# only sex is significant: (breeding phase is in the top model but with dAIC=0.3 no strong support)
m_for_trip_dist_pars <- lme(distance.to.nest~sex, data=for.trip.dist.dur.bird.year, random=~1|birdID/year, method="REML")
summary(m_for_trip_dist_pars) # males make foraging trips of on average 5 km further max distance than females. 

# STATISTICS DURATION FORAGING TRIPS PER SEX PER STAGE
m_dur_trips_full  = lme(dur_tot~breeding.phase*sex, data=for.trip.dist.dur.bird.year, random=~1|birdID/year, method="ML")
dd.dur_trips = dredge(m_dur_trips_full) # support for an effect of both sex and breeding phase. 
m_for_trip_dur_pars <- lme(distance.to.nest~sex+breeding.phase, data=for.trip.dist.dur.bird.year, random=~1|birdID/year, method="REML")
summary(m_for_trip_dur_pars) # how is it possible that breeding phase is not significant here, while dAIC is 4.74?
# females make foraging trips of shorter duration than males.
# duration of foraging trips is (somewhat) shorter in chick phase than in egg phase, for both males and females


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
hist(n_trips_day_bird_year_mean$n_trips)
m_for_trips_nr  = lme(n_trips~breeding.phase*sex, data=n_trips_day_bird_year_mean, random=~1|birdID/year, method="ML")
summary(m_for_trips_nr)
dd.dur_trips = dredge(m_for_trips_nr) # most parsimonious model has effects of sex and breeding phase
m_for_trips_nr_pars <- lme(n_trips~breeding.phase+sex, data=n_trips_day_bird_year_mean, random=~1|birdID/year, method="REML")
summary(m_for_trips_nr_pars) # significantly more foraging trips during chick phase than egg phase, and fewer by males than by females.

# NUMBER OF FORAGING TRIPS PER BREEDING PHASE PER SEX
windows(4,6)
par(mar=c(6,6,3,1))
boxplot(n_trips~breeding.phase, data=n_trips_day_bird_year_mean[n_trips_day_bird_year_mean$sex=="f",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,5), las=1, cex.axis=1.3)
boxplot(n_trips~breeding.phase, data=n_trips_day_bird_year_mean[n_trips_day_bird_year_mean$sex=="m",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=1.3)
mtext("Number of foraging trips per day", 2, 3, cex=1.5)
mtext("Breeding phase", 1, 4, cex=1.5)
legend("topleft", legend=c("females","males"), pch=22, pt.bg = c("lightcoral","lightskyblue"), cex=1.5, bty="n")
# show sample sizes! 
axis(3, at=1:2, N.prop.for.phase.sex[,1], tick=F, cex.axis=1.3)
axis(3, at=1:2+0.3, N.prop.for.phase.sex[,2], tick=F, cex.axis=1.3)
axis(3, at=0.8, "N =", tick=F, cex.axis=1.3)

#