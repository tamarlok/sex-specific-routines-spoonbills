source("functions.R") # to have the most updated version of the functions
Sys.setenv(TZ='Europe/Amsterdam')
gps.breeding.data.behav <- gps.breeding.data.behav.sel

table(gps.breeding.data.behav$habitat_salinity, gps.breeding.data.behav$habitat, gps.breeding.data.behav$behaviour)
table(gps.breeding.data.behav$nest1, gps.breeding.data.behav$year, gps.breeding.data.behav$birdID)

# assign nest locations to attempts: 
nest.locations.bird.year <- aggregate(cbind(nest1,nest2)~year+birdID+attempt+lat.nest1+lon.nest1+lat.nest2+lon.nest2+nest1.real+nest2.real,gps.breeding.data.behav,sum)
nest.locations.bird.year = nest.locations.bird.year[order(nest.locations.bird.year$year, nest.locations.bird.year$birdID, nest.locations.bird.year$attempt),]
# when nest1>nest2 (always being a real nesting attempt)
nest.locations.bird.year[nest.locations.bird.year$nest1>nest.locations.bird.year$nest2,c('lat.nest','lon.nest')] <- nest.locations.bird.year[nest.locations.bird.year$nest1>nest.locations.bird.year$nest2,c('lat.nest1','lon.nest1')]
# when nest2>nest1 and nest2 was a real nesting attempt: 
nest.locations.bird.year[nest.locations.bird.year$nest2>nest.locations.bird.year$nest1 & nest.locations.bird.year$nest2.real==1,c('lat.nest','lon.nest')] <- nest.locations.bird.year[nest.locations.bird.year$nest2>nest.locations.bird.year$nest1 & nest.locations.bird.year$nest2.real==1,c('lat.nest2','lon.nest2')]
# add lat.nest and lon.nest to gps.breeding.data.behav
gps.breeding.data.behav <- merge(gps.breeding.data.behav, nest.locations.bird.year[,c('birdID','year','attempt','lat.nest','lon.nest')])
# calculate distance to nest:
gps.breeding.data.behav$distance.to.nest <- NA
gps.breeding.data.behav$distance.to.nest[is.na(gps.breeding.data.behav$lat.nest)==F] <- round(distCosine(gps.breeding.data.behav[is.na(gps.breeding.data.behav$lat.nest)==F,c('longitude','latitude')], gps.breeding.data.behav[is.na(gps.breeding.data.behav$lat.nest)==F,c('lon.nest','lat.nest')], r=6378137),3) # gives the distance in meters between nest location and actual position of the bird; can only be calculated when lat.nest (and lon.nest) is known.

# remove the data where the habitat is unknown:
gps.breeding.data.behav$behaviour2 = gps.breeding.data.behav$behaviour
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour=='foraging'] = paste(gps.breeding.data.behav$behaviour[gps.breeding.data.behav$behaviour=='foraging'], gps.breeding.data.behav$habitat_salinity[gps.breeding.data.behav$behaviour=='foraging'], sep='_')
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour=='resting'] = paste(gps.breeding.data.behav$behaviour[gps.breeding.data.behav$behaviour=='resting'], gps.breeding.data.behav$habitat_type[gps.breeding.data.behav$behaviour=='resting'], sep='_')
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$distance.to.nest<5] = 'at_nest'
# TL 2022.05.26: eerder stond hier als nest1==1|nest2==1, die waren in determine.breeding.phases berekend als <5 meter van nest1 of nest2 af. Dit zou hetzelfde moeten geven als distance.to.nest<5, maar dat is blijkbaar niet zo, want ineens is het percentage nest attendance veel lager. zijn de nest coordinaten dan toch verkeerd toegekend? Ik ben nu de determine.breeding.phases functie opnieuw aan het runnen waarbij ik de kolommen nest1 en nest2 behoud, om te kijken wat hier fout gaat. 
#gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$nest1==1|gps.breeding.data.behav$nest2==1] = 'at_nest'

gps.breeding.data.behav$behaviour2.nr <- NA
gps.breeding.data.behav$behaviour2.nr[gps.breeding.data.behav$behaviour2=='foraging_marine'] = 9
gps.breeding.data.behav$behaviour2.nr[gps.breeding.data.behav$behaviour2=='foraging_brackish'] = 8
gps.breeding.data.behav$behaviour2.nr[gps.breeding.data.behav$behaviour2=='foraging_freshwater'] = 7
gps.breeding.data.behav$behaviour2.nr[gps.breeding.data.behav$behaviour2=='flying'] = 6
gps.breeding.data.behav$behaviour2.nr[gps.breeding.data.behav$behaviour2=='walking'] = 5
gps.breeding.data.behav$behaviour2.nr[gps.breeding.data.behav$behaviour2=='resting_wadden'] = 4
gps.breeding.data.behav$behaviour2.nr[gps.breeding.data.behav$behaviour2=='resting_mainland'] = 3
gps.breeding.data.behav$behaviour2.nr[gps.breeding.data.behav$behaviour2=='resting_schier'] = 2
gps.breeding.data.behav$behaviour2.nr[gps.breeding.data.behav$behaviour2=='at_nest'] = 1

gps.breeding.data.behav$breeding.phase2 <- as.character(gps.breeding.data.behav$breeding.phase)
gps.breeding.data.behav$breeding.phase2[gps.breeding.data.behav$breeding.phase.nr==5|gps.breeding.data.behav$breeding.phase.nr==6] <- "5.post-breeding"

# for now, remove the foraging_NA and resting_NA cases for behaviour2, but we can probably include them by using smart criteria based on longitude and latitude.
table(gps.breeding.data.behav$behaviour2)
gps.breeding.data.behav <- gps.breeding.data.behav[gps.breeding.data.behav$behaviour!="unknown"&gps.breeding.data.behav$behaviour2!="foraging_NA"&gps.breeding.data.behav$behaviour2!="resting_NA"&gps.breeding.data.behav$behaviour2!="resting_rest",]
# similar, we should also use some criteria to define foraging_land better
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour2=="foraging_land"&gps.breeding.data.behav$habitat_type=="schier"] <- "foraging_brackish" # this is not necessarily true!
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour2=="foraging_land"&gps.breeding.data.behav$habitat_type=="mainland"] <- "foraging_freshwater"
# remove the 5 points with habitat_type=rest
gps.breeding.data.behav <- gps.breeding.data.behav[gps.breeding.data.behav$habitat_type!="rest",]

### Investigate patterns of total time spent foraging per day: 
foraging_duration_yday_bird_phase = aggregate(duration/60~year+birdID+yday_CEST+day_rel_hatching+week_rel_hatching, gps.breeding.data.behav[gps.breeding.data.behav$behaviour=='foraging',], sum)
names(foraging_duration_yday_bird_phase)[6] = 'dur_foraging_yday' # change column duration in to "dur_foraging_yday"
duration_yday_birdID_year <- duration_yday_birdID_year[duration_yday_birdID_year$breeding.phase!='1.exclude',]
duration_yday_birdID_year$breeding.phase <- factor(duration_yday_birdID_year$breeding.phase)
foraging_duration_yday_bird_phase = merge(duration_yday_birdID_year, foraging_duration_yday_bird_phase, all.x=T) # to make sure that all the days for each bird are in the data, also the days without foraging (the famous zero's)
foraging_duration_yday_bird_phase$dur_foraging_yday[is.na(foraging_duration_yday_bird_phase$dur_foraging_yday)] = 0 # change the NA's into zeros.
foraging_duration_yday_bird_phase = foraging_duration_yday_bird_phase[order(foraging_duration_yday_bird_phase$year, foraging_duration_yday_bird_phase$birdID, foraging_duration_yday_bird_phase$yday_CEST),]

### Remove data for which hatching dates were unknown:
foraging_duration_yday_bird_phase <- foraging_duration_yday_bird_phase[is.na(foraging_duration_yday_bird_phase$day_rel_hatching)==F,]
for_dur_week_bird_year = aggregate(dur_foraging_yday~year+birdID+sex+week, foraging_duration_yday_bird_phase, mean)
for_dur_week_bird_year$sex.nr = ifelse(for_dur_week_bird_year$sex=='F',2,1)
for_dur_week_rel_hatching_bird_year = aggregate(dur_foraging_yday~year+birdID+sex+week_rel_hatching, foraging_duration_yday_bird_phase, mean)
for_dur_week_rel_hatching_bird_year$sex.nr = ifelse(for_dur_week_rel_hatching_bird_year$sex=='F',2,1)

# Now translate to proportion of time spent foraging
foraging_duration_yday_bird_phase$prop_for = foraging_duration_yday_bird_phase$dur_foraging_yday/foraging_duration_yday_bird_phase$dur_per_yday
foraging_duration_yday_bird_phase$logit_prop_for = qlogis(foraging_duration_yday_bird_phase$prop_for+min(foraging_duration_yday_bird_phase$prop_for[foraging_duration_yday_bird_phase$prop_for>0]))
# calculate average time spent foraging per bird per breeding phase (this should probably be the unit of the statistical analysis)
foraging_duration_phase_year_bird <- aggregate(prop_for~breeding.phase+year+birdID+sex, foraging_duration_yday_bird_phase, mean)

### determine the proportion foraging in brackish and marine (versus freshwater) habitat
# calculate total foraging duration in brackish or marine habitat per bird per yday
for_dur_bird_yday_marine = aggregate(duration/60~year+birdID+sex+week+yday_CEST, gps.breeding.data.behav[gps.breeding.data.behav$behaviour=='foraging'&gps.breeding.data.behav$habitat_salinity!="freshwater",], sum)
# merge with file of all days that birds were foraging at least some time of the day:
names(for_dur_bird_yday_marine)[6]="dur_for_brackish_marine"
for_dur_bird_yday_marine_vs_total <- merge(foraging_duration_yday_bird_phase, for_dur_bird_yday_marine, all.x=T)
for_dur_bird_yday_marine_vs_total$dur_for_brackish_marine[is.na(for_dur_bird_yday_marine_vs_total$dur_for_brackish_marine)] = 0 # where there is no foraging in brackish/marine but there is foraging in the day, duration should be 0.
for_dur_bird_yday_marine_vs_total$prop_for_marine <- for_dur_bird_yday_marine_vs_total$dur_for_brackish_marine/for_dur_bird_yday_marine_vs_total$dur_foraging_yday

# calculate means per bird per breeding phase per year
for_prop_marine_bird_year_phase <- aggregate(prop_for_marine~breeding.phase+year+birdID+sex, for_dur_bird_yday_marine_vs_total, mean)
for_prop_marine_bird_year_phase[order(for_prop_marine_bird_year_phase$sex, for_prop_marine_bird_year_phase$birdID, for_prop_marine_bird_year_phase$breeding.phase),]

# Statistical analysis of Proportion of time spent foraging
# Discussion on whether (or not) to use p-values with mixed-effects models (https://stats.stackexchange.com/questions/22988/how-to-obtain-the-p-value-check-significance-of-an-effect-in-a-lme4-mixed-mode)
foraging_duration_phase_year_bird$logit_prop_for <- qlogis(foraging_duration_phase_year_bird$prop_for)
m_for.dur.phasexsex = lme(logit_prop_for~breeding.phase*sex, data=foraging_duration_phase_year_bird, random=~1|year/birdID, method="ML") 

# check assumptions of homogeneity of variances in relation to fitted values and explanatory variables:
windows()
plot(m_for.dur.phasexsex)
qqnorm(m_for.dur.phasexsex)
qqnorm(m_for.dur.phasexsex, ~ranef(., level=1))
qqnorm(m_for.dur.phasexsex, ~ranef(., level=2))
# check for equal variances among explanatory variables
plot( m_for.dur.phasexsex, resid(., type = "p") ~ fitted(.) | sex, id = 0.05, adj = 0 )
plot( m_for.dur.phasexsex, resid(., type = "p") ~ fitted(.) | breeding.phase * sex )
# calculate significance of explanatory variables 
# different methods to do so...
anova(m_for.dur.phasexsex, type="marginal")
Anova(m_for.dur.phasexsex,type='III')
dredge(m_for.dur.phasexsex)
summary(m_for.dur.phasexsex)

# way to provide p-values instead of perform model selection is to use Wald or LRT tests:
# https://www.ssc.wisc.edu/sscc/pubs/MM/MM_TestEffects.html
m_for.dur.phasexsex.for.lmer <- lmer(logit_prop_for~breeding.phase*sex + (1|year/birdID), data=foraging_duration_phase_year_bird, REML=FALSE) 
anova(m_for.dur.phasexsex.for.lmer, type="marginal")
Anova(m_for.dur.phasexsex.for.lmer)
summary(m_for.dur.phasexsex.for.lmer)
qqplot(m_for.dur.phasexsex.for.lmer)

# Model selection method, selecting the most parsimonious model for plotting:
dredge(m_for.dur.phasexsex)
m_for.dur.phase_REML = lme(logit_prop_for~breeding.phase+sex, data=foraging_duration_phase_year_bird, random=~1|year/birdID, method="REML")
summary(m_for.dur.phase_REML) 
intervals(m_for.dur.phase_REML)
emmeans_for_dur_phase_sex = emmeans(m_for.dur.phase_REML, pairwise~breeding.phase+sex) # within breeding phases, proportion of time spent foraging is higher in females than in males (p=0.0098). Proportion of time spent foraging decreases over the season, though the only pairwise comparison that was significant was between chick and post-breeding unsuccessful phase. # the most parsimonious model does not include the interaction between breeding phase and sex. However, perhaps it is true that in one of the breeding phases, the sexes significantly differ in time spent foraging (i.e. when caring for the eggs/chicks) but not during post-breeding?
m_for.dur.phasexsex_REML = lme(logit_prop_for~breeding.phase*sex, data=foraging_duration_phase_year_bird, random=~1|year/birdID, method="REML")
summary(m_for.dur.phasexsex_REML)
lsmeans_for_dur_phasexsex = lsmeans(m_for.dur.phasexsex_REML, pairwise~breeding.phase*sex) 
# when looking at the interaction model, only during the egg (p=0.0020) and chick phase (p=0.0378), females spend significantly more time foraging than males. 
# within females, there were no significant differences in time spent foraging between breeding phases
# within males, time spent foraging during chick phase was significant higher than during post-breeding unsuccessful phase. 


# Statistical analysis of proportion of foraging time spent in marine habitat
for_prop_marine_bird_year_phase$prop_for_marine[for_prop_marine_bird_year_phase$prop_for_marine==1] <- 1-min(for_prop_marine_bird_year_phase$prop_for_marine)
for_prop_marine_bird_year_phase$logit_prop_for_marine <- qlogis(for_prop_marine_bird_year_phase$prop_for_marine)
m_for.marine.phasexsex = lme(logit_prop_for_marine~breeding.phase*sex, data=for_prop_marine_bird_year_phase, random=~1|year/birdID, method="ML") 
dredge(m_for.marine.phasexsex)
m_for.marine.phase.sex_REML = lme(logit_prop_for_marine~breeding.phase+sex, data=for_prop_marine_bird_year_phase, random=~1|year/birdID, method="REML")
summary(m_for.marine.phase.sex_REML)
lsmeans_for_marine_phase_sex = lsmeans(m_for.marine.phase.sex_REML, pairwise~breeding.phase+sex) # females forage significantly more in marine habitats than males. The proportion of time spent foraging in marine habitat was significantly higher during the egg phase than during the post-breeding phase (either success- or unsuccessful).

# the most parsimonious model does not include the interaction between breeding phase and sex. however, perhaps it is true that in one of the breeding phases, the sexes significantly differ in time spent foraging (i.e. when caring for the eggs/chicks) but not during post-breeding?
m_for.marine.phasexsex_REML = lme(logit_prop_for_marine~breeding.phase*sex, data=for_prop_marine_bird_year_phase, random=~1|year/birdID, method="REML")
summary(m_for.marine.phasexsex_REML)
lsmeans_for_marine_phasexsex = lsmeans(m_for.marine.phasexsex_REML, pairwise~breeding.phase*sex) 
# during all breeding phases, females spend more time foraging in marine waters than males
# within females, the proportion of time spent foraging in marine habitat does not differ between breeding phases. 
# within males, the proportion of time spent foraging in marine habitat is only significantly higher during the egg phase than during the post-breeding unsuccessful phase. 

# plot proportion of foraging in marine waters for males only, per individual per year. 
males_prop_marine <- for_prop_marine_bird_year_phase[for_prop_marine_bird_year_phase$sex=='M'&(for_prop_marine_bird_year_phase$breeding.phase=='3.eggs'|for_prop_marine_bird_year_phase$breeding.phase=='4.chicks'),]
males_prop_marine <- aggregate(prop_for_marine~birdID+year, males_prop_marine, mean)
# plot ordered by prop_for_marine
males_prop_marine <- males_prop_marine[order(males_prop_marine$prop_for_marine),]
plot(males_prop_marine$prop_for_marine, xaxt='n', xlab='')
axis(1, at=1:dim(males_prop_marine)[1], paste(males_prop_marine$birdID, males_prop_marine$year), las=2)

# plot ordered on mean prop_for_marine per individual, and then on year
bird.data <- read.csv("data/raw/bird.data.csv", header=T)
bird.data <- read.csv("bird.data.csv", header=T) # code for NIOZ-server
males_prop_marine_mean <- aggregate(prop_for_marine~birdID, males_prop_marine, mean)
names(males_prop_marine_mean)[2]<- "mean_prop_marine"
males_prop_marine_mean <- merge(males_prop_marine_mean, bird.data[c(-12,-20),c('birdID','bodymass','headbill')]) # remove the biometry from the first two captures of 656 

# analyse proportion of marine foraging of males in relation to headbill length and body mass, 

lm.HB <- lm(mean_prop_marine~headbill, males_prop_marine_mean)
summary(lm.HB) # p=0.0366


########### FIGURE 5 #############
pdf("output/Fig5.pdf")
plot(mean_prop_marine~headbill, males_prop_marine_mean, pch=19, cex=2, xlab='Head-bill length (mm)', ylab='Proportion marine foraging', cex.axis=1.2, cex.lab=1.3)
abline(lm.HB)
dev.off()

males_prop_marine <- merge(males_prop_marine, males_prop_marine_mean)
males_prop_marine <- males_prop_marine[order(males_prop_marine$mean_prop_marine, males_prop_marine$year),]
windows()
plot(males_prop_marine$prop_for_marine, xaxt='n', xlab='')
axis(1, at=1:dim(males_prop_marine)[1], paste(males_prop_marine$birdID, males_prop_marine$year), las=2)
# to do this in a neat way, we should work with logit transformed values...

# what to do with the estimated foraging points on land; assume this is walking?
# redo analysis to see if results change... however, then what should we do for the walking points in water? Assume they were foraging? this is rather arbitrary, as birds may actually be walking in water as well....

# properly analyse the timing of nest attendance and foraging in relation to the daynight and tidal cycle. 
gps.breeding.data.behav$foraging <- 0
gps.breeding.data.behav$foraging[gps.breeding.data.behav$behaviour=='foraging'] <- 1
gps.breeding.data.behav$at_nest <- 0
gps.breeding.data.behav$at_nest[gps.breeding.data.behav$behaviour2=='at_nest'] <- 1

# add tide data:
tide.data <- read.table("data/raw/getij_schiermonnikoog_2010-2019.txt", skip=4)
tide.data <- read.table("getij_schiermonnikoog_2010-2019.txt", skip=4) # for NIOZ-server
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
high.tides <- tide.data[tide.data$lowhigh=="high",c("year","yday_CEST", "waterheight","day_min")]
low.tides <- tide.data[tide.data$lowhigh=="low",c("year","yday_CEST", "waterheight","day_min")]
# determine on hour scale whether tide is low, incoming, high or outgoing:
tidal.phase.hour <- expand.grid(hour=0:23, date=unique(tide.data$date))
tidal.phase.hour$date_hour_CEST <- dmy_h(paste(tidal.phase.hour$date, tidal.phase.hour$hour, sep=" "))
tidal.phase.hour$date_hour_CEST <- force_tz(tidal.phase.hour$date_hour_CEST, tzone="Europe/Amsterdam")
tidal.phase.hour <- merge(tidal.phase.hour, tide.data[,c("date_hour_CEST","lowhigh")], all.x=T)
tidal.phase.hour <- tidal.phase.hour[order(tidal.phase.hour$date_hour_CEST),]
# make adjacent hour before and after the hour of low and high tide also low and high, resulting in 3 hour periods of low and high tide; then, the hours from low to high are labelled as "incoming" and from high to low outgoing:
tidal.phase.hour$tide_min1 <- c(tidal.phase.hour$lowhigh[2:dim(tidal.phase.hour)[1]],NA)
tidal.phase.hour$tide_plus1 <- c(NA,tidal.phase.hour$lowhigh[1:(dim(tidal.phase.hour)[1]-1)])
tidal.phase.hour$tidal_phase <- tidal.phase.hour$lowhigh
# fill in the spots where tide_min1 is not NA:
tidal.phase.hour$tidal_phase[is.na(tidal.phase.hour$tide_min1)==F] <- tidal.phase.hour$tide_min1[is.na(tidal.phase.hour$tide_min1)==F]
# fill in the spots where tide_min1 is not NA:
tidal.phase.hour$tidal_phase[is.na(tidal.phase.hour$tide_plus1)==F] <- tidal.phase.hour$tide_plus1[is.na(tidal.phase.hour$tide_plus1)==F]
# fill in the other NA values with incoming or outgoing
tidal.phase.hour$tidal_phase[is.na(tidal.phase.hour$tidal_phase)] <- "unknown"
for (i in 2:dim(tidal.phase.hour)[1]) {
  if (tidal.phase.hour$tidal_phase[i-1]=='low'&tidal.phase.hour$tidal_phase[i]=="unknown") tidal.phase.hour$tidal_phase[i] <- "incoming"
  if (tidal.phase.hour$tidal_phase[i-1]=='incoming'&tidal.phase.hour$tidal_phase[i]=="unknown") tidal.phase.hour$tidal_phase[i] <- "incoming"
  if (tidal.phase.hour$tidal_phase[i-1]=='high'&tidal.phase.hour$tidal_phase[i]=="unknown") tidal.phase.hour$tidal_phase[i] <- "outgoing"
  if (tidal.phase.hour$tidal_phase[i-1]=='outgoing'&tidal.phase.hour$tidal_phase[i]=="unknown") tidal.phase.hour$tidal_phase[i] <- "outgoing"
}
# make first three cases manually outgoing:
tidal.phase.hour$tidal_phase[1:3] <- "outgoing"
gps.breeding.data.behav$date_hour_CEST <- round_date(gps.breeding.data.behav$date_time, "hour")
gps.breeding.data.behav <- merge(gps.breeding.data.behav, tidal.phase.hour[,c('date_hour_CEST','tidal_phase')])
  
# nest attendance, without random effects:
gps.breeding.data.behav.sel <- na.omit(gps.breeding.data.behav[gps.breeding.data.behav$breeding.phase.nr<5,c('birdID','date_time','hour_CEST','tidal_phase','breeding.phase','sex','at_nest','foraging','year')])
gps.breeding.data.behav.sel$hour_f <- as.factor(gps.breeding.data.behav.sel$hour_CEST)
m.nest.attendance.hour <- glm(at_nest~sex*breeding.phase+sex*tidal_phase*hour_f, gps.breeding.data.behav.sel, family='binomial', na.action='na.fail')
#dredge(m.nest.attendance.hour) # takes ca. 5 minutes to run; full model is by far best supported (dAICc=80.92)
summary(m.nest.attendance.hour)
anova(m.nest.attendance.hour) # does not give p-values
pairwise.comparison.nest.attendance.hour <- lsmeans(m.nest.attendance.hour, pairwise~sex*hour_f+sex*breeding.phase)
pairwise.contrasts.nest.attendance.hour <- summary(pairwise.comparison.nest.attendance.hour$contrasts) # alles verschilt, behalve:
pairwise.contrasts.nest.attendance.hour$p.value <- round(pairwise.contrasts.nest.attendance.hour$p.value,4)
# F, eggs 0:00 versus 21:00, 22:00, 23:00, 1:00, 2:00
# M, eggs 0:00 versus 23:00, 1:00, 2:00, 3:00
data.predict <- expand.grid(breeding.phase=c("3.eggs","4.chicks"), hour=0:23, sex=c("F","M"), tidal_phase=c('low','incoming','high','outgoing'))
data.predict$hour_f = as.factor(data.predict$hour)
model.predictions <- predict(m.nest.attendance.hour, data.predict, se.fit=T)
data.predict$logit.nest.attendance <- model.predictions$fit
data.predict$logit.se.nest.attendance <- model.predictions$se.fit # mean +/- 1.96*se gives the CI
# nest attendance in relation to hour of the day and sex, during low tide and during the egg phase
data.predict.low.eggs <- data.predict[data.predict$tidal_phase=='low'&data.predict$breeding.phase=='3.eggs',]
plot(logit.nest.attendance~hour, data.predict.low.eggs, col=c('red','blue')[as.numeric(data.predict.low.eggs$sex)]) # the known day night pattern of male and female egg incubation
# nest attendance of females in relation to hour of the day and tidal phase during egg phase 
plot(logit.nest.attendance~hour, data.predict[data.predict$sex=='F'&data.predict$breeding.phase=='3.eggs',], col=c('brown','green','blue','lightblue')[as.numeric(data.predict$tidal_phase[data.predict$sex=='F'&data.predict$breeding.phase=='3.eggs'])]) # females are more likely to attend the nest at high tide than at low tide, but primarily during the day until the late evening. Between midnight and early morning, there is no effect of tide and females are always on the nest. 
plot(logit.nest.attendance~hour, data.predict[data.predict$sex=='M'&data.predict$breeding.phase=='3.eggs',], col=c('brown','green','blue','lightblue')[as.numeric(data.predict$tidal_phase[data.predict$sex=='M'&data.predict$breeding.phase=='3.eggs'])]) # for males, there is hardly any pattern with the tide, suggesting that when females are near the nest during high tide, at periods when they are normally not incubating, the males remain close to the nest too. Given the inaccuracy of the GPS measurement, it is uncertain who of the two partners is sitting on the nest during these periods, but field observations suggest that incubation shifts may occur during the day when females are present in the colony during high tide. 

# these models already take quite a lot of time, even though we did not yet correct for pseudoreplication by adding individual as random effect. 
# what happens when we calculate probabilities of nest attendance and foraging per bird, hour, tidal phase and breeding phase? (what about year?)
mean.probs.per.bird.tidal.breeding.phase <- aggregate(cbind(at_nest, foraging)~birdID+sex+hour_CEST+hour_f+tidal_phase+breeding.phase, gps.breeding.data.behav.sel, mean)
mean.probs.per.bird.tidal.breeding.phase$logit_at_nest <- qlogis(mean.probs.per.bird.tidal.breeding.phase$at_nest)
mean.probs.per.bird.tidal.breeding.phase$logit_foraging <- qlogis(mean.probs.per.bird.tidal.breeding.phase$foraging)
min_at_nest <- min(mean.probs.per.bird.tidal.breeding.phase$at_nest[mean.probs.per.bird.tidal.breeding.phase$at_nest>0])
min_foraging <- min(mean.probs.per.bird.tidal.breeding.phase$foraging[mean.probs.per.bird.tidal.breeding.phase$foraging>0])
max_at_nest <- max(mean.probs.per.bird.tidal.breeding.phase$at_nest[mean.probs.per.bird.tidal.breeding.phase$at_nest<1])
max_foraging <- max(mean.probs.per.bird.tidal.breeding.phase$foraging[mean.probs.per.bird.tidal.breeding.phase$foraging<1])
mean.probs.per.bird.tidal.breeding.phase$logit_at_nest[mean.probs.per.bird.tidal.breeding.phase$at_nest==0] <- qlogis(min_at_nest)
mean.probs.per.bird.tidal.breeding.phase$logit_foraging[mean.probs.per.bird.tidal.breeding.phase$foraging==0] <- qlogis(min_foraging)
mean.probs.per.bird.tidal.breeding.phase$logit_at_nest[mean.probs.per.bird.tidal.breeding.phase$at_nest==1] <- qlogis(max_at_nest)
mean.probs.per.bird.tidal.breeding.phase$logit_foraging[mean.probs.per.bird.tidal.breeding.phase$foraging==1] <- qlogis(max_foraging)
lm.nest.attendance <- lm(logit_at_nest~sex*breeding.phase+sex*tidal_phase*hour_f, data=mean.probs.per.bird.tidal.breeding.phase, na.action="na.fail")
anova(lm.nest.attendance) # everything is significant, except the (near-significant) interaction between sex and breeding phase.
dredge(lm.nest.attendance) # the hour x sex x tidal_phase interaction and the hour x tidal_phase interaction are no longer supported. Only limited support for the breeding phase x sex interaction.   
# what does this do to predictions and confidence intervals?
lm.predictions.nest.attendance <- predict(lm.nest.attendance, data.predict, se.fit=T)
data.predict$lm.nest.attendance <- lm.predictions.nest.attendance$fit
data.predict$lm.se.nest.attendance <- lm.predictions.nest.attendance$se.fit # mean +/- 1.96*se gives the CI

data.predict.low.eggs <- data.predict[data.predict$tidal_phase=='low'&data.predict$breeding.phase=='3.eggs',]
plot(lm.nest.attendance~hour, data.predict.low.eggs, col=c('red','blue')[as.numeric(data.predict.low.eggs$sex)]) # the known day night pattern of male and female egg incubation
plot(logit.se.nest.attendance~lm.se.nest.attendance, data.predict) # the logit.se of the glm model on the raw data has about 3-7 times smaller SE's than the lm model on the means. However, in the glm, we should still account for the random effect. 

# compare the CI of the model based on raw data and model based on means per bird / hour / tidal phase / breeding stage
# all data
data.predict$prob.nest.attendance.all <- plogis(data.predict$logit.nest.attendance)  
data.predict$prob.nest.attendance.all.lower <- plogis(data.predict$logit.nest.attendance-1.96*data.predict$logit.se.nest.attendance)  
data.predict$prob.nest.attendance.all.upper <- plogis(data.predict$logit.nest.attendance+1.96*data.predict$logit.se.nest.attendance)  
# mean data
data.predict$prob.nest.attendance.means <- plogis(data.predict$lm.nest.attendance)  
data.predict$prob.nest.attendance.means.lower <- plogis(data.predict$lm.nest.attendance-1.96*data.predict$lm.se.nest.attendance)  
data.predict$prob.nest.attendance.means.upper <- plogis(data.predict$lm.nest.attendance+1.96*data.predict$lm.se.nest.attendance)  
# plot the estimated CI for females during the egg phase during low tide:
data.predict.eggs.females <- data.predict[data.predict$breeding.phase=='3.eggs'&data.predict$sex=="F",]
data.predict.eggs.females.low <- data.predict.eggs.females[data.predict.eggs.females$tidal_phase=="low",]
data.predict.eggs.females.incoming <- data.predict.eggs.females[data.predict.eggs.females$tidal_phase=="incoming",]
data.predict.eggs.females.high <- data.predict.eggs.females[data.predict.eggs.females$tidal_phase=="high",]
data.predict.eggs.females.outgoing <- data.predict.eggs.females[data.predict.eggs.females$tidal_phase=="outgoing",]
plotCI(data.predict.eggs.females.low$hour, data.predict.eggs.females.low$prob.nest.attendance.all, li=data.predict.eggs.females.low$prob.nest.attendance.all.lower, ui=data.predict.eggs.females.low$prob.nest.attendance.all.upper, sfrac=0, gap=0, xlab="hour", ylab="probability of nest attendance", ylim=c(0,1), pch=19)
plotCI(data.predict.eggs.females.low$hour+0.3, data.predict.eggs.females.low$prob.nest.attendance.means, li=data.predict.eggs.females.low$prob.nest.attendance.means.lower, ui=data.predict.eggs.females.low$prob.nest.attendance.means.upper, sfrac=0, gap=0, add=T, col="red", pch=19)
# (as expected) wider CI's when based on means instead of raw data, however, still a clear day-night difference, at least during low tide. 
data.predict.eggs.males <- data.predict[data.predict$breeding.phase=='3.eggs'&data.predict$sex=="M",]
data.predict.eggs.males.low <- data.predict.eggs.males[data.predict.eggs.males$tidal_phase=="low",]
data.predict.eggs.males.incoming <- data.predict.eggs.males[data.predict.eggs.males$tidal_phase=="incoming",]
data.predict.eggs.males.high <- data.predict.eggs.males[data.predict.eggs.males$tidal_phase=="high",]
data.predict.eggs.males.outgoing <- data.predict.eggs.males[data.predict.eggs.males$tidal_phase=="outgoing",]

### Figure S3: nest attendance in reation to hour of the day and tidal phase for females and males
windows(12,6)
layout(matrix(1:2, ncol=2))
par(mar=c(5,1,4,0), oma=c(0,3,0,7))
### (A) females
plotCI(data.predict.eggs.females.low$hour, data.predict.eggs.females.low$prob.nest.attendance.means, li=data.predict.eggs.females.low$prob.nest.attendance.means.lower, ui=data.predict.eggs.females.low$prob.nest.attendance.means.upper, sfrac=0, gap=0, xlab="hour", ylab="", col="red", pch=19, xlim=c(0,24), ylim=c(0,1), main="females")
plotCI(data.predict.eggs.females.incoming$hour+0.2, data.predict.eggs.females.incoming$prob.nest.attendance.means, li=data.predict.eggs.females.incoming$prob.nest.attendance.means.lower, ui=data.predict.eggs.females.incoming$prob.nest.attendance.means.upper, sfrac=0, gap=0, col="green", pch=19, add=T)
plotCI(data.predict.eggs.females.outgoing$hour+0.4, data.predict.eggs.females.outgoing$prob.nest.attendance.means, li=data.predict.eggs.females.outgoing$prob.nest.attendance.means.lower, ui=data.predict.eggs.females.outgoing$prob.nest.attendance.means.upper, sfrac=0, gap=0, col="orange", pch=19, add=T)
plotCI(data.predict.eggs.females.high$hour+0.6, data.predict.eggs.females.high$prob.nest.attendance.means, li=data.predict.eggs.females.high$prob.nest.attendance.means.lower, ui=data.predict.eggs.females.high$prob.nest.attendance.means.upper, sfrac=0, gap=0, col="blue", pch=19, add=T)
### (B) males
plotCI(data.predict.eggs.males.low$hour, data.predict.eggs.males.low$prob.nest.attendance.means, li=data.predict.eggs.males.low$prob.nest.attendance.means.lower, ui=data.predict.eggs.males.low$prob.nest.attendance.means.upper, sfrac=0, gap=0, xlab="hour", ylab="", col="red", pch=19, xlim=c(0,24), ylim=c(0,1), yaxt="n", main="males")
plotCI(data.predict.eggs.males.incoming$hour+0.2, data.predict.eggs.males.incoming$prob.nest.attendance.means, li=data.predict.eggs.males.incoming$prob.nest.attendance.means.lower, ui=data.predict.eggs.males.incoming$prob.nest.attendance.means.upper, sfrac=0, gap=0, col="green", pch=19, add=T)
plotCI(data.predict.eggs.males.outgoing$hour+0.4, data.predict.eggs.males.outgoing$prob.nest.attendance.means, li=data.predict.eggs.males.outgoing$prob.nest.attendance.means.lower, ui=data.predict.eggs.males.outgoing$prob.nest.attendance.means.upper, sfrac=0, gap=0, col="orange", pch=19, add=T)
plotCI(data.predict.eggs.males.high$hour+0.6, data.predict.eggs.males.high$prob.nest.attendance.means, li=data.predict.eggs.males.high$prob.nest.attendance.means.lower, ui=data.predict.eggs.males.high$prob.nest.attendance.means.upper, sfrac=0, gap=0, col="blue", pch=19, add=T)
mtext("probability of nest attendance",2,1.5, outer=T)
legend(26,0.247,c("low","incoming","high","outgoing"),pch=19, col=c("red","green","blue","orange"), xpd=NA)

# analysing mean probabilities of nest attendance and foraging in relation to breeding phase and sex
mean.probs.per.bird.breeding.phase <- aggregate(cbind(at_nest, foraging)~birdID+sex+breeding.phase, gps.breeding.data.behav.sel, mean)
mean.probs.per.bird.breeding.phase$logit_at_nest <- qlogis(mean.probs.per.bird.breeding.phase$at_nest)
mean.probs.per.bird.breeding.phase$logit_foraging <- qlogis(mean.probs.per.bird.breeding.phase$foraging)
save.image("Time.allocation.results.tmp.RData")
load("Time.allocation.results.tmp.RData")

# Check how long the model takes when we add the random effect of individual (and year): 
m.nest.attendance.rnd <- glmer(at_nest~sex*breeding.phase+sex*tidal_phase*hour_f+(1|birdID)+(1|year), gps.breeding.data.behav.sel, family='binomial', na.action='na.fail') # this model takes more than 8 hours to run... 
save.image("Time.allocation.results.tmp.RData")

### NOW the analysis of FORAGING
glm.foraging <- glm(foraging~sex*breeding.phase+sex*tidal_phase*hour_f, gps.breeding.data.behav.sel, family='binomial', na.action='na.fail')
dredge(glm.foraging) # takes ca. 5 minutes to run; full model is by far best supported (dAICc=80.92)
model.predictions.foraging <- predict(glm.foraging, data.predict, se.fit=T)
data.predict$logit.foraging <- model.predictions.foraging$fit
data.predict$logit.se.foraging <- model.predictions.foraging$se.fit # mean +/- 1.96*se gives the CI
plot(logit.foraging~hour, data.predict[data.predict$tidal_phase=='low'&data.predict$breeding.phase=='3.eggs',], col=c('red','blue')[as.numeric(data.predict$sex[data.predict$tidal_phase=='low'&data.predict$breeding.phase=='3.eggs'])])
plot(logit.foraging~hour, data.predict[data.predict$sex=='F'&data.predict$breeding.phase=='3.eggs',], col=c('brown','green','blue','lightblue')[as.numeric(data.predict$tidal_phase[data.predict$sex=='F'&data.predict$breeding.phase=='3.eggs'])]) 
plot(logit.foraging~hour, data.predict[data.predict$sex=='M'&data.predict$breeding.phase=='3.eggs',], col=c('brown','green','blue','lightblue')[as.numeric(data.predict$tidal_phase[data.predict$sex=='M'&data.predict$breeding.phase=='3.eggs'])]) 
