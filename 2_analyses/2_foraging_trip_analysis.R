### determine a circle of 50 m around the nest, which we here call "colony". If a bird moves outside the colony, we assume it is on a foraging trip (note that resting periods away from the colony are included in the foraging trip):
gps.breeding.data.behav$at_colony_50m = 0
gps.breeding.data.behav$at_colony_50m[gps.breeding.data.behav$distance.to.nest<50]=1 
gps.breeding.data.behav$foraging_trip = 0

### Determine and label individual foraging trips
### Important that the data are ordered correctly, on year, bird, date
gps.breeding.data.behav <- gps.breeding.data.behav[order(gps.breeding.data.behav$year, gps.breeding.data.behav$birdID, gps.breeding.data.behav$date_time_CEST),]

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

# until now, foraging trips are defined on the basis of leaving the colony, not yet by the fact that birds were also foraging for some time when away from the colony. This is defined below. 

gps.breeding.data.behav$year = as.factor(gps.breeding.data.behav$year)

df.for.trips = gps.breeding.data.behav[gps.breeding.data.behav$foraging_trip>0,]

df.for.trips.34 = df.for.trips[df.for.trips$breeding.phase=='3.eggs'|df.for.trips$breeding.phase=='4.chicks',]

max.distance.year.bird.trip = aggregate(distance.to.nest/1000 ~ year + birdID + foraging_trip, data=df.for.trips.34, max) 
names(max.distance.year.bird.trip)[4]<-'distance.to.nest'
df.for.trips.34$freq=1
foraging.sums.per.trip = aggregate(cbind(freq, foraging)~year+birdID+foraging_trip, data=df.for.trips.34, sum) # breeding phase is removed, as a foraging trip can cover the switch from egg to chick phase, or from chick to postfledging... Later, the breeding phases will be coupled again. Here, the info on whether a bird was actually foraging is calculated. 
start_time_of_trips = aggregate(date_time_CEST~year+birdID+foraging_trip, data=df.for.trips.34, min) # we assume the trip starts at the first location outside the colony, because otherwise (if 0.5*time_to_previous is substracted) the start of the trip sometimes ends up in a yday that is excluded from analysis because it has less than 23.5 hours of duration data.  
names(start_time_of_trips)[4] = 'start_time'
# other info at the start of the foraging trip:
end_time_of_trips = aggregate(date_time_CEST~year+birdID+foraging_trip, data=df.for.trips.34, max) # we assume the trip ends at the last sample outside the colony. We then add 30 minutes, to account for the fact that on average, the individual departed 15 minutes prior to the first sample outside the colony and returned on average 15 minutes after the last sample outside the colony.  
names(end_time_of_trips)[4] = 'end_time'
trips.start.end <- cbind(start_time_of_trips, end_time=end_time_of_trips$end_time)
trips.start.end$yday_start <- yday(trips.start.end$start_time)
trips.start.end$yday_end <- yday(trips.start.end$end_time)
trips.start.end$trip.duration <- round(difftime(trips.start.end$end_time, trips.start.end$start_time, units="hour"),2)+0.5 # we add 30 minutes (=0.5h), see above
trip.info <- cbind(trips.start.end, foraging.sums.per.trip[,c('freq','foraging')], distance.to.nest=max.distance.year.bird.trip$distance.to.nest)

## only use trips where the bird was actually foraging for some time:
trip.info.sel <- trip.info[trip.info$foraging>0,] # 3160 trips left from 4295
## only keep foraging trips that (1) took place within a single day or (2) if the trip covered more than one day, only trips where the day after the start date of the trip is not missing (because the bird went outside the study area for example, or because it included durations of >60 minutes): 
gps.breeding.data.behav$freq=1
complete_birdyeardays <- aggregate(freq~birdID+yday_CEST+year+breeding.phase+breeding.phase.nr+sex, gps.breeding.data.behav, sum)
table(complete_birdyeardays$freq)
complete_birdyeardays <- complete_birdyeardays[order(complete_birdyeardays$year, complete_birdyeardays$birdID, complete_birdyeardays$yday_CEST),]
complete_birdyeardays$next.day.missing <- 0
for (i in 2:dim(complete_birdyeardays)[1]) if (complete_birdyeardays$yday_CEST[i]!=(complete_birdyeardays$yday_CEST[i-1]+1)) complete_birdyeardays$next.day.missing[i-1] <- 1
trip.info.sel2 <- merge(trip.info.sel, complete_birdyeardays[c('year','birdID','yday_CEST','sex','breeding.phase.nr','breeding.phase','next.day.missing')], by.x=c('year','birdID','yday_start'), by.y=c('year','birdID','yday_CEST'))
trip.info.sel2$end.trip.next.day <- ifelse(trip.info.sel2$yday_end>trip.info.sel2$yday_start,1,0)
# exclude trips that ended on a later date than the start date and where the date after the start date was excluded (because less than 47 samples):
trip.info.sel2 <- trip.info.sel2[trip.info.sel2$end.trip.next.day==0|(trip.info.sel2$end.trip.next.day==1&trip.info.sel2$next.day.missing==0),]
dim(trip.info.sel)[1]-dim(trip.info.sel2)[1] # 40 trips excluded because of incomplete data on the day itself or the day after.

table(round(as.numeric(trip.info.sel2$trip.duration),1)) # what to do with the few trips that covered more than 24 hours (17 out of the 3120 trips)? 
# Only for the trips that were >48 hours, this could possibly be caused by the day after the next day to be missing in the dataset. Check if this was the case:
na.omit(trip.info.sel2[trip.info.sel2$trip.duration>48,])

complete_birdyeardays[complete_birdyeardays$birdID==1606 & complete_birdyeardays$year==2015 & complete_birdyeardays$yday_CEST%in%c(162:165),]
complete_birdyeardays[complete_birdyeardays$birdID==651 & complete_birdyeardays$yday_CEST%in%c(170:173),]
complete_birdyeardays[complete_birdyeardays$birdID==6287 & complete_birdyeardays$year==2017 & complete_birdyeardays$yday_CEST%in%c(185:195),]
# in all cases, it concerns the end of the chick phase, when chicks are already very large

dim(trip.info.sel2) # N=3120 trips left
df.for.trips <- trip.info.sel2
df.for.trips$breeding.phase <- factor(df.for.trips$breeding.phase)
df.for.trips$trip.duration <- as.numeric(df.for.trips$trip.duration)
df.for.trips$hour_end = hour(df.for.trips$end_time)
df.for.trips$yday_start = yday(df.for.trips$start_time)
df.for.trips$yday_end = yday(df.for.trips$end_time)
df.for.trips$year = as.factor(df.for.trips$year)

### STATISTICS ###

### (1) NUMBER OF FORAGING TRIPS
#######################################
# unique days per breeding phase, bird and year (to know which yday there are in the dataset so that we do not omit days without foraging trips):
phase_yday_bird_year = unique(gps.breeding.data.behav[gps.breeding.data.behav$breeding.phase=='3.eggs'|gps.breeding.data.behav$breeding.phase=='4.chicks',c('year','birdID','sex','yday_CEST','breeding.phase')])
phase_yday_bird_year$yday_start = phase_yday_bird_year$yday_CEST
# days with number of foraging trips per bird per year (days without foraging trips are absent, therefore link with above dataframe phase_yday_bird_year)
df.for.trips$freq=1
## number of foraging trips per day per individual:
n_trips_day_bird_year = aggregate(freq~year+birdID+sex+yday_start, data=df.for.trips, sum)
## merge the two df's to get the 0's for days without foraging trips:
n_trips_day_bird_year_zeros = merge(phase_yday_bird_year, n_trips_day_bird_year, all.x=T)
n_trips_day_bird_year_zeros$freq[is.na(n_trips_day_bird_year_zeros$freq)]=0
names(n_trips_day_bird_year_zeros)[7]='n_trips'

# using the number of foraging trips per date per bird as statistical unit
var(n_trips_day_bird_year_zeros$n_trips)/mean(n_trips_day_bird_year_zeros$n_trips) # 0.58, no overdispersion. 
hist(n_trips_day_bird_year_zeros$n_trips, breaks=8)
m_n_for_trips  = glmer(n_trips~breeding.phase*sex+(1|birdID)+(1|year), data=n_trips_day_bird_year_zeros, family="poisson", na.action='na.fail')
summary(m_n_for_trips)
model.sel.n.trips = dredge(m_n_for_trips)
m.pars.n.trips = get.models(model.sel.n.trips,1)[[1]]
emmeans.n.trips <- emmeans(m_n_for_trips, pairwise~breeding.phase*sex, regrid='response')
emmeans.n.trips$contrasts # all pairwise comparisons are significant, except eggs F vs chicks M. 
n.trips.pairs = as.data.frame(emmeans.n.trips$contrasts)[c(2,5,1,6),]
n.trips.pairs$sign = ifelse(n.trips.pairs$p.value<0.001, "***",
                            ifelse(n.trips.pairs$p.value<0.01, "**",
                                   ifelse(n.trips.pairs$p.value<0.05, "*", "n.s.")))
as.data.frame(emmeans.n.trips$emmeans)
# Females increase the number of trips more than males when chicks have hatched. 

### (2) FORAGING TRIP DURATION

# using duration of individual foraging trips as statistical unit
m_for.trip.dur  = lme(trip.duration~breeding.phase*sex, data=df.for.trips, random=~1|birdID, method="ML") # cross random effects are very difficult to model with lme, much easier with (g)lmer; so to check for assumptions of homogeneity I only included the birdID random intercept 

# check assumptions of homogeneity of variances in relation to fitted values and explanatory variables:
windows()
plot(m_for.trip.dur)
qqnorm(m_for.trip.dur)
qqnorm(m_for.trip.dur, ~ranef(., level=1))
# check for equal variances among explanatory variables
plot( m_for.trip.dur, resid(., type = "p") ~ fitted(.) | sex, id = 0.05, adj = 0 )
plot( m_for.trip.dur, resid(., type = "p") ~ fitted(.) | breeding.phase * sex )

# as the distribution of trip durations is right skewed, we use a gamma distribution:
hist(df.for.trips$trip.duration) # right skewed distribution... (many short trips)
m_for.trip.dur.gamma  = glmer(trip.duration~breeding.phase*sex+(1|birdID)+(1|year), data=df.for.trips, family=Gamma("inverse"), na.action="na.fail")
model.sel.for.trip.dur.gamma = dredge(m_for.trip.dur.gamma) 
m.pars.for.trip.dur = get.models(model.sel.for.trip.dur.gamma,1)[[1]]
summary(m.pars.for.trip.dur)
emmeans.for.trip.dur <- emmeans(m.pars.for.trip.dur, pairwise~breeding.phase*sex, type='response')
emmeans.for.trip.dur # all pairwise comparisons are significant, except eggs M vs chicks M and eggs F vs eggs/chicks M). 
dur.trips.pairs = as.data.frame(emmeans.for.trip.dur$contrasts)[c(2,5,1,6),]
dur.trips.pairs$sign = ifelse(dur.trips.pairs$p.value<0.001, "***",
                            ifelse(dur.trips.pairs$p.value<0.01, "**",
                                   ifelse(dur.trips.pairs$p.value<0.05, "*", "n.s.")))

### (3) FORAGING TRIP DISTANCE

# (1) using mean foraging trip distance per bird per breeding phase per year as statistical unit
m_for_trip_dist  = lme(distance.to.nest~breeding.phase*sex, data=df.for.trips, random=~1|year/birdID, method="ML")

# check assumptions of homogeneity of variances in relation to fitted values and explanatory variables:
windows()
plot(residuals(m_for_trip_dist)~predict(m_for_trip_dist)) # residuals in relation to fitted values: very skewed!
boxplot(residuals(m_for_trip_dist)~paste(df.for.trips$sex, df.for.trips$breeding.phase)) # variation is homogeneuous/similar between explanatory variables
# some other checking figures:
plot(m_for_trip_dist)
qqnorm(m_for_trip_dist)
qqnorm(m_for_trip_dist, ~ranef(., level=1))
qqnorm(m_for_trip_dist, ~ranef(., level=2))
# check for equal variances among explanatory variables
plot( m_for_trip_dist, resid(., type = "p") ~ fitted(.) | sex )
plot( m_for_trip_dist, resid(., type = "p") ~ fitted(.) | breeding.phase * sex )

# similar as for the duration of foraging trips, the distribution of foraging trip distances is right skewed, so we use a gamma distribution:
hist(df.for.trips$distance.to.nest)
m_for_trip_dist.gamma  = glmer(distance.to.nest~breeding.phase*sex+(1|birdID)+(1|year), data=df.for.trips, family=Gamma("inverse"), na.action="na.fail")
plot(residuals(m_for_trip_dist.gamma)~predict(m_for_trip_dist.gamma)) # nicely homogeneous!
boxplot(residuals(m_for_trip_dist.gamma)~paste(df.for.trips$sex, df.for.trips$breeding.phase))
model.sel.for.trip.distance = dredge(m_for_trip_dist.gamma) 
m.pars.for.trip.distance = get.models(model.sel.for.trip.distance,1)[[1]]
emmeans.for.trip.distance <- emmeans(m_for_trip_dist.gamma, pairwise~breeding.phase*sex, type='response')
emmeans.for.trip.distance # all pairwise comparisons are significant, except eggs vs chicks for males. 
dist.trips.pairs = as.data.frame(emmeans.for.trip.distance$contrasts)[c(2,5,1,6),]
dist.trips.pairs$sign = ifelse(dist.trips.pairs$p.value<0.001, "***",
                              ifelse(dist.trips.pairs$p.value<0.01, "**",
                                     ifelse(dist.trips.pairs$p.value<0.05, "*", "n.s.")))

# Save the different tables into subtables to combine as Table 2:
write.csv(make.table.from.dredge.output(model.sel.n.trips), "output/Table2a - Foraging trip number.csv")
write.csv(make.table.from.dredge.output(model.sel.for.trip.dur.gamma), "output/Table2b - Foraging trip duration.csv")
write.csv(make.table.from.dredge.output(model.sel.for.trip.distance), "output/Table2c - Foraging trip distance.csv")
# the models using the poisson distribution count one parameter less, as the variance is not estimated but calculated. 