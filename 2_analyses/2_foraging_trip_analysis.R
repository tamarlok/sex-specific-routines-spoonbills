# first run the time allocation analysis script
keep(gps.breeding.data.behav, duration_yday_birdID_year, bird.data, breeding.data, sure=T)

source("functions.R") # to have the most updated version of the functions
Sys.setenv(TZ='Europe/Amsterdam') # just to be sure

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

df.for.trips.34$duration_foraging_freshwater = df.for.trips.34$duration_foraging - df.for.trips.34$duration_foraging_marine
# duration presence on the mainland:
df.for.trips.34$duration_mainland = ifelse(df.for.trips.34$habitat_type=='mainland',df.for.trips.34$duration_behav/60,0)

max.distance.year.bird.trip = aggregate(distance.to.nest/1000 ~ year + birdID + foraging_trip, data=df.for.trips.34, max) 
names(max.distance.year.bird.trip)[4]<-'distance.to.nest'
dur.foraging.per.trip = aggregate(cbind(duration_foraging/60, duration_foraging_marine/60, duration_foraging_freshwater/60, duration_mainland/60)~year+birdID+foraging_trip, data=df.for.trips.34, sum) # breeding phase is removed, as a foraging trip can cover the switch from egg to chick phase, or from chick to postfledging... Later, the breeding phases will be coupled again.  
names(dur.foraging.per.trip)[4:7] <- c('duration_foraging', 'duration_foraging_marine', 'duration_foraging_freshwater', 'duration_mainland')
start_time_of_trips = aggregate(date_time~year+birdID+foraging_trip, data=df.for.trips.34, min) # we assume the trip starts at the first location outside the colony, because otherwise (if 0.5*time_to_previous is substracted) the start of the trip sometimes ends up in a yday that is excluded from analysis because it has less than 23.5 hours of duration data.  
names(start_time_of_trips)[4] = 'start_time'
# other info at the start of the foraging trip:
other_start_info_trips = aggregate(cbind(day_rel_hatching, week_rel_hatching, week, yday_CEST)~year+birdID+foraging_trip, data=df.for.trips.34, min) # we assume 
end_time_of_trips = aggregate(date_time+time_to_next~year+birdID+foraging_trip, data=df.for.trips.34, max) # we assume the trip ends at the first location back in the colony after a trip. 
names(end_time_of_trips)[4] = 'end_time'
trips.start.end <- cbind(start_time_of_trips, end_time=end_time_of_trips$end_time)
trips.start.end$yday_start <- yday(trips.start.end$start_time)
trips.start.end$yday_end <- yday(trips.start.end$end_time)
trips.start.end$trip.duration <- difftime(trips.start.end$end_time, trips.start.end$start_time, units="hour")
# check that aggregate functions were done in the same order:
table(trips.start.end$birdID==dur.foraging.per.trip$birdID)
table(trips.start.end$birdID==max.distance.year.bird.trip$birdID)
table(trips.start.end$birdID==other_start_info_trips$birdID)
trip.info <- cbind(trips.start.end, other_start_info_trips[,c('day_rel_hatching','week_rel_hatching','week')], dur.foraging.per.trip[,c('duration_foraging', 'duration_foraging_marine', 'duration_foraging_freshwater', 'duration_mainland')], distance.to.nest=max.distance.year.bird.trip$distance.to.nest)
## only keep foraging trips that (1) took place within a single day or (2) if the trip covered more than one day, only trips where the day after the start date of the trip is not missing (because the bird went outside the study area for example, or because it included durations of >60 minutes): 
duration_yday_birdID_year <- duration_yday_birdID_year[order(duration_yday_birdID_year$year, duration_yday_birdID_year$birdID, duration_yday_birdID_year$yday_CEST),]
duration_yday_birdID_year$next.day.missing <- 0
for (i in 2:dim(duration_yday_birdID_year)[1]) if (duration_yday_birdID_year$yday_CEST[i]!=(duration_yday_birdID_year$yday_CEST[i-1]+1)) duration_yday_birdID_year$next.day.missing[i-1] <- 1
trip.info.sel <- merge(trip.info, duration_yday_birdID_year[c('year','birdID','yday_CEST','dur_per_yday','next.day.missing')], by.x=c('year','birdID','yday_start'), by.y=c('year','birdID','yday_CEST'), all.x=T)
trip.info.sel$end.trip.next.day <- ifelse(trip.info.sel$yday_end>trip.info.sel$yday_start,1,0)
# exclude trips that ended on a later date than the start date and where the date after the start date was excluded (because <23.5 hour data):
trip.info.sel <- trip.info.sel[trip.info.sel$end.trip.next.day==0|(trip.info.sel$end.trip.next.day==1&trip.info.sel$next.day.missing==0),]
dim(trips.start.end)[1]-dim(trip.info.sel)[1] # 96 trips excluded
table(round(as.numeric(trip.info.sel$trip.duration),1)) # what to do with the few trips that covered more than 24 hours (12 out of the 5989 trips)? Only for the trips that were >48 hours, this could possibly be caused by the day after the next day to be missing in the dataset.
# therefore, I excluded the 3 trips that were longer than 48 hours:
trip.info.sel <- trip.info.sel[trip.info.sel$trip.duration<48,]

dim(trip.info.sel[trip.info.sel$duration_foraging==0,]) ## in 2883 of the 6085 trips, birds were not (registered to be) foraging.
trip.info.sel <- trip.info.sel[trip.info.sel$duration_foraging>0,]
dim(trip.info.sel) # N=3103 trips left
# merge with duration_yday_birdID_year to add sex and breeding.phase again
df.for.trips = merge(trip.info.sel, duration_yday_birdID_year[,c('year','birdID','yday_CEST','sex','breeding.phase.nr','breeding.phase')], by.x=c('year','birdID','yday_start'), by.y=c('year','birdID','yday_CEST'))
df.for.trips$trip.duration <- as.numeric(df.for.trips$trip.duration)

# calculate proportion of marine vs freshwater foraging during foraging trip:
df.for.trips$prop_foraging_marine <- df.for.trips$duration_foraging_marine/df.for.trips$duration_foraging
df.for.trips$prop_foraging_freshwater <- df.for.trips$duration_foraging_freshwater/df.for.trips$duration_foraging
df.for.trips$marine.trip <- ifelse(df.for.trips$prop_foraging_marine>0.5,1,0)

# assess the proportion of foraging in marine/brackish and freshwater foraging trips: 
windows()
hist(df.for.trips$prop_foraging_marine, breaks=20, xlab='prop foraging marine', main='Distribution of proportion marine per foraging trip', ylim=c(0,2500)) # shows mainly
windows()
hist(df.for.trips$prop_foraging_fresh, breaks=20, xlab='prop foraging freshwater', main='Distribution of proportion freshwater per foraging trip', ylim=c(0,2500)) # this is now logically exactly the opposite pattern as that of prop marine foraging. (as brackish foraging is pooled with marine foraging)

df.for.trips$hour_end = hour(df.for.trips$end_time)
df.for.trips$yday_start = yday(df.for.trips$start_time)
df.for.trips$yday_end = yday(df.for.trips$end_time)
df.for.trips$year = as.factor(df.for.trips$year)
df.for.trips$prop_foraging <- df.for.trips$duration_foraging/df.for.trips$trip.duration

# plot the distribution of foraging trip distances for females and males:
windows()
layout(1:2)
par(mar=c(1,5,4,1), oma=c(4,0,0,0))
multhist(list(df.for.trips$distance.to.nest[df.for.trips$sex=="F"&df.for.trips$marine.trip==1], df.for.trips$distance.to.nest[df.for.trips$sex=="F"&df.for.trips$marine.trip==0]), col=c("blue","lightblue"), breaks=0:24, names.arg=0:23, xlab="Foraging trip distance", ylab="Frequency", main="Females")
multhist(list(df.for.trips$distance.to.nest[df.for.trips$sex=="M"&df.for.trips$marine.trip==1], df.for.trips$distance.to.nest[df.for.trips$sex=="M"&df.for.trips$marine.trip==0]), col=c("blue","lightblue"), breaks=0:24, names.arg=0:23, xlab="Foraging trip distance", ylab="Frequency", main="Males")
mtext("Foraging trip distance (km)",1,2,outer=T)

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
var(n_trips_day_bird_year_zeros$n_trips)/mean(n_trips_day_bird_year_zeros$n_trips) # 0.75, no overdispersion. 
hist(n_trips_day_bird_year_zeros$n_trips, breaks=8)
m_n_for_trips_rnd_bird_within_year  = glmer(n_trips~breeding.phase*sex+(1|year/birdID), data=n_trips_day_bird_year_zeros, family="poisson", na.action='na.fail') # warning of singularity of fit; probably due to some birds having data in only one year
m_n_for_trips_rnd_bird  = glmer(n_trips~breeding.phase*sex+(1|birdID), data=n_trips_day_bird_year_zeros, family="poisson", na.action='na.fail')
m_n_for_trips_rnd_bird_year  = glmer(n_trips~breeding.phase*sex+(1|birdID)+(1|year), data=n_trips_day_bird_year_zeros, family="poisson", na.action='na.fail')
AIC(m_n_for_trips, m_n_for_trips_rnd_bird, m_n_for_trips_rnd_bird_year) # additive random effects of bird and year is best supported, though the model with only bird as a random effect is the most parsimonious.
summary(m_n_for_trips)
m_n_for_trips  = glmer(n_trips~breeding.phase*sex+(1|birdID)+(1|year), data=n_trips_day_bird_year_zeros, family="poisson", na.action='na.fail') # use model with birdID and year as random intercepts in an additive rather than nested structure
summary(m_n_for_trips)
n_parameters(m_n_for_trips, effects='fixed')
n_parameters(m_n_for_trips, effects='random')

model.sel.n.trips = dredge(m_n_for_trips) # most parsimonious model includes additive effect of sex and breeding phase
m.pars.n.trips = get.models(model.sel.n.trips,1)[[1]]
emmeans.n.trips <- emmeans(m.pars.n.trips, pairwise~breeding.phase*sex, transform='response')
emmeans.n.trips$contrasts # all pairwise comparisons are significant, except eggs F vs chicks M. 
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
model.sel.for.trip.dur.gamma = dredge(m_for.trip.dur.gamma) # interaction between sex and breeding phase is best supported; why are 7 parameters counted in the full model, instead of 6? Is this because the estimate of "residual variation" is added for the Gamma or normal distribution, but not for the Poisson distribution (as residual variation can be calculated (rather than estimated) for a Poisson distribution). 
m.pars.for.trip.dur = get.models(model.sel.for.trip.dur.gamma,1)[[1]]
summary(m.pars.for.trip.dur)
emmeans.for.trip.dur <- emmeans(m.pars.for.trip.dur, pairwise~breeding.phase*sex, type='response')
emmeans.for.trip.dur # all pairwise comparisons are significant, except eggs M vs chicks M (and eggs F vs eggs/chicks M). 

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
emmeans.for.trip.distance <- emmeans(m.pars.for.trip.distance, pairwise~breeding.phase*sex, type='response')
emmeans.for.trip.distance # all pairwise comparisons are significant, except eggs vs chicks for males. 

# Save the different tables into subtables to combine as Table S6:
write.csv(make.table.from.dredge.output(model.sel.n.trips, lme4=T), "output/TableS6a - Foraging trip number.csv")
write.csv(make.table.from.dredge.output(model.sel.for.trip.dur.gamma, lme4=T), "output/TableS6b - Foraging trip duration.csv")
write.csv(make.table.from.dredge.output(model.sel.for.trip.distance, lme4=T), "output/TableS6c - Foraging trip distance.csv")
# the models using the poisson distribution count one parameter less, as the variance is not estimated but calculated. 

# SAME ANALYSIS BUT ONLY FOR MARINE (incl brackish) FORAGING TRIPS: 
df.marine.for.trips <- df.for.trips[df.for.trips$duration_mainland==0&df.for.trips$marine.trip==1,]
marine.for.trip.dist.dur.bird.year <- aggregate(cbind(distance.to.nest, trip.duration) ~ breeding.phase + year + birdID + sex, df.marine.for.trips, mean)
# calculate the number of marine foraging trips per day:
## number of foraging trips per day per individual:
n_marine_trips_day_bird_year = aggregate(freq~year+birdID+sex+yday_start, data=df.marine.for.trips, sum)
n_marine_trips_day_bird_year_zeros = merge(phase_yday_bird_year, n_marine_trips_day_bird_year, all.x=T)
n_marine_trips_day_bird_year_zeros$freq[is.na(n_marine_trips_day_bird_year_zeros$freq)]=0
names(n_marine_trips_day_bird_year_zeros)[7]='n_marine_trips'
n_marine_trips_day_bird_year_mean = aggregate(n_marine_trips~year+birdID+sex+breeding.phase, data=n_marine_trips_day_bird_year_zeros, mean)
n_marine_trips_day_bird_year_mean$year = as.factor(n_marine_trips_day_bird_year_mean$year)
n_marine_trips_day_bird_year_mean[order(n_marine_trips_day_bird_year_mean$birdID, n_marine_trips_day_bird_year_mean$year),]

# NUMBER OF MARINE FORAGING TRIPS
m_n_for_mar_trips  = glmer(n_marine_trips~breeding.phase*sex+(1|birdID)+(1|year), data=n_marine_trips_day_bird_year_zeros, family="poisson", na.action='na.fail')
model.sel.n.mar.trips = dredge(m_n_for_mar_trips) # most parsimonious model includes the interaction between sex and breeding phase
# sex is supported, but the interaction is not. This shows that females and males show a similar increase in the number of marine foraging trips. 
m.pars.n.mar.trips = get.models(model.sel.n.mar.trips,2)[[1]]
summary(m.pars.n.mar.trips)
emmeans.n.mar.trips <- emmeans(m.pars.n.mar.trips, pairwise~breeding.phase+sex, transform='response')
emmeans.n.mar.trips # both females and males double the number of marine foraging trips, but in absolute number, this increase is much higher for females than for males.
# It is due to the Poisson distribution that a multiplicative effect is estimated rather than an additive effect.

# DURATION OF FORAGING TRIPS
m_mar.for.trip.dur  = glmer(trip.duration~breeding.phase*sex+(1|birdID)+(1|year), data=df.marine.for.trips, family=Gamma("inverse"), na.action="na.fail")
model.sel.mar.for.trip.dur = dredge(m_mar.for.trip.dur) # the sex effect is still supported, but much less so. 
m.pars.mar.trip.dur = get.models(model.sel.mar.for.trip.dur,2)[[1]]
emmeans.mar.trip.dur <- emmeans(m.pars.mar.trip.dur, pairwise~breeding.phase+sex, transform='response')
emmeans.mar.trip.dur # pairwise differences between males and females are N.S. Females make marine trips that last slightly longer than those of males. 
# Marine trips are shorter during chick rearing than during egg incubation.

# DISTANCE OF MARINE FORAGING TRIPS
m_mar.for.trip.dist  = glmer(distance.to.nest~breeding.phase*sex+(1|birdID)+(1|year), data=df.marine.for.trips, family=Gamma("inverse"), na.action="na.fail")
model.sel.mar.for.trip.dist = dredge(m_mar.for.trip.dist) # the sex effect is still supported, but much less so. 
m.pars.mar.trip.dist = get.models(model.sel.mar.for.trip.dist,1)[[1]]
summary(m.pars.mar.trip.dist)
emmeans.mar.trip.dist <- emmeans(m.pars.mar.trip.dist, pairwise~breeding.phase*sex, transform='response')
emmeans.mar.trip.dist # pairwise difference is only sign. for eggs F > chicks F. 
# basically, the only real support is the interaction between breeding phase and sex. Where males make longer distance foraging trips during chick-rearing than during egg incubation, females make shorter trips. 

# Save info for Table S7
write.csv(make.table.from.dredge.output(model.sel.n.mar.trips, lme4=T), "output/TableS7a - Marine foraging trip number.csv")
write.csv(make.table.from.dredge.output(model.sel.mar.for.trip.dur, lme4=T), "output/TableS7b - Marine foraging trip duration.csv")
write.csv(make.table.from.dredge.output(model.sel.mar.for.trip.dist, lme4=T), "output/TableS7c - Marine foraging trip distance.csv")

hist()

