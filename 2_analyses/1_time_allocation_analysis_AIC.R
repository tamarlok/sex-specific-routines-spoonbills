source("functions.R") # to have the most updated version of the functions
Sys.setenv(TZ='Europe/Amsterdam')
gps.breeding.data.behav <- gps.breeding.data.behav.sel

# data of 763 are removed from 2015 onward, as GPS locations are very inaccurate (which is visible from the wide spread of locations around the nest location, and corresponding low nest attendance)
gps.breeding.data.behav$exclude <- 0
gps.breeding.data.behav$exclude[gps.breeding.data.behav$birdID==763&(gps.breeding.data.behav$year>=2015)] <- 1
gps.breeding.data.behav <- gps.breeding.data.behav[gps.breeding.data.behav$exclude==0,]

### Investigate patterns of total time spent foraging and attending the nest per day:
gps.breeding.data.behav$at_nest <- 0
gps.breeding.data.behav$at_nest[gps.breeding.data.behav$behaviour2=='at_nest'] <- 1
gps.breeding.data.behav$foraging <- 0
gps.breeding.data.behav$foraging[gps.breeding.data.behav$behaviour2=='foraging_freshwater'|gps.breeding.data.behav$behaviour2=='foraging_brackish'|gps.breeding.data.behav$behaviour2=='foraging_marine'] <- 1 # foraging on land is not considered as foraging
gps.breeding.data.behav$foraging_marine <- 0
gps.breeding.data.behav$foraging_marine[gps.breeding.data.behav$behaviour2=='foraging_brackish'|gps.breeding.data.behav$behaviour2=='foraging_marine'] <- 1

gps.breeding.data.behav$duration_foraging <- gps.breeding.data.behav$foraging * gps.breeding.data.behav$duration_behav
gps.breeding.data.behav$duration_foraging_marine <- gps.breeding.data.behav$foraging_marine * gps.breeding.data.behav$duration_behav
gps.breeding.data.behav$duration_nest_attendance <- gps.breeding.data.behav$at_nest * gps.breeding.data.behav$duration_behav

# duration per bird per date (only calculated for exploration plots)
duration_behaviour_yday_bird_phase = aggregate(cbind(duration_behav/60,duration_nest_attendance/60,duration_foraging/60,duration_foraging_marine/60)~year+birdID+yday_CEST+day_rel_hatching+week_rel_hatching+sex+breeding.phase, gps.breeding.data.behav, sum)
names(duration_behaviour_yday_bird_phase)[8:11] = c("dur_tot_yday","dur_at_nest_yday","dur_foraging_yday","dur_foraging_marine_yday")
duration_behaviour_yday_bird_phase = duration_behaviour_yday_bird_phase[order(duration_behaviour_yday_bird_phase$year, duration_behaviour_yday_bird_phase$birdID, duration_behaviour_yday_bird_phase$yday_CEST),]
head(duration_behaviour_yday_bird_phase)
duration_behaviour_yday_bird_phase$prop_for <- duration_behaviour_yday_bird_phase$dur_foraging_yday/duration_behaviour_yday_bird_phase$dur_tot_yday
# duration per bird per breeding phase (used for Fig 3 and for statistics)
duration_behaviour_bird_phase_year = aggregate(cbind(duration_behav/60,duration_nest_attendance/60,duration_foraging/60,duration_foraging_marine/60)~year+birdID+sex+breeding.phase, gps.breeding.data.behav, sum)
names(duration_behaviour_bird_phase_year)[5:8] = c("dur_tot","dur_at_nest","dur_foraging","dur_foraging_marine")
duration_behaviour_bird_phase_year = duration_behaviour_bird_phase_year[order(duration_behaviour_bird_phase_year$year, duration_behaviour_bird_phase_year$birdID),]
duration_behaviour_bird_phase_year$prop_nest <- duration_behaviour_bird_phase_year$dur_at_nest/duration_behaviour_bird_phase_year$dur_tot
duration_behaviour_bird_phase_year$prop_for <- duration_behaviour_bird_phase_year$dur_foraging/duration_behaviour_bird_phase_year$dur_tot
duration_behaviour_bird_phase_year$prop_for_marine <- duration_behaviour_bird_phase_year$dur_foraging_marine/duration_behaviour_bird_phase_year$dur_foraging
# only include data per bird per year with at least 5 days of data per breeding phase (dur_tot>23.5*5, as days with at least 23.5 hours of data are included):
duration_behaviour_bird_phase_year <- duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$dur_tot>23.5*5,]
# also use this criterium for later analyses (on timing of nest attendance/foraging and foraging trips), as bird and year are then used as random effects, and may otherwise cause estimation problems. 
gps.breeding.data.behav <- merge(gps.breeding.data.behav, duration_behaviour_bird_phase_year[,c('birdID','year','breeding.phase')])

# Statistical analysis
# Discussion on whether (or not) to use p-values with mixed-effects models (https://stats.stackexchange.com/questions/22988/how-to-obtain-the-p-value-check-significance-of-an-effect-in-a-lme4-mixed-mode)
# I decided to go for model selection approach, as the use of p-values in mixed-effects models is arbritary, and AIC provides more useful information on the level of support for certain effects (instead of just a boundary value for whether to reject a null hypothesis.

# (1) PROPORTION OF TIME SPENT ATTENDING THE NEST (only comparing egg incubation and chick rearing phase)
duration_behaviour_bird_phase_year_egg_chicks <- duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$breeding.phase%in%c('3.eggs','4.chicks'),]
min_prop_nest <- min(duration_behaviour_bird_phase_year_egg_chicks$prop_nest[duration_behaviour_bird_phase_year_egg_chicks$prop_nest>0])
duration_behaviour_bird_phase_year_egg_chicks$prop_nest[duration_behaviour_bird_phase_year_egg_chicks$prop_nest==0] = min_prop_nest
duration_behaviour_bird_phase_year_egg_chicks$logit_prop_nest <- qlogis(duration_behaviour_bird_phase_year_egg_chicks$prop_nest)
m_prop_nest.phasexsex = lme(logit_prop_nest~breeding.phase*sex, data=duration_behaviour_bird_phase_year_egg_chicks, random=~1|year/birdID, method="ML") 
# check homogeneity of residuals in relation to fitted values and explanatory variables
windows()
plot(residuals(m_prop_nest.phasexsex)~predict(m_prop_nest.phasexsex))
# other residuals figures:
plot(m_prop_nest.phasexsex)
qqnorm(m_prop_nest.phasexsex)
qqnorm(m_prop_nest.phasexsex, ~ranef(., level=1))
qqnorm(m_prop_nest.phasexsex, ~ranef(., level=2))
# check for equal variances among explanatory variables
boxplot(residuals(m_prop_nest.phasexsex)~paste(duration_behaviour_bird_phase_year_egg_chicks$sex, duration_behaviour_bird_phase_year_egg_chicks$breeding.phase), xlab="",ylab='residuals') 
plot( m_prop_nest.phasexsex, resid(., type = "p") ~ fitted(.) | breeding.phase * sex )
# residuals are homogeneously distributed over fitted values, explanatory variables (and random effects)
model.sel.prop.at.nest <- dredge(m_prop_nest.phasexsex)
TableS1 <- make.table.from.dredge.output(model.sel.prop.at.nest)
write.csv(TableS1, "output/TableS1 - Proportion_at_nest.csv")
sapply(get.models(model.sel.prop.at.nest, subset=T), predict, new.data=expand.grid(breeding.phase=c('3.eggs','4.breeding.phase'), sex=c('F','M'))) # predicted values for each model in the model selection table
# useful link how to model average predictions (though without confidence intervals): https://sites.google.com/site/rforfishandwildlifegrads/home/mumin_usage_examples?authuser=0 
# get predictions for the most parsimonious model, run with REML: 
# effects returned by predict() are conditional effects (i.e. these are conditioned on certain (reference) levels of factors), while emmeans() returns marginal means, since the effects are “marginalized” (or “averaged”) over the levels of factors.
# it seems to me that marginal means are preferred. See also: https://strengejacke.github.io/ggeffects/articles/technical_differencepredictemmeans.html
m.pars.prop.at.nest = get.models(model.sel.prop.at.nest,1,method='REML')[[1]]
marginal.means.prop.nest <- as.data.frame(emmeans(m.pars.prop.at.nest, ~breeding.phase+sex))
marginal.means.prop.nest <- cbind(marginal.means.prop.nest[,c('sex','breeding.phase','df')], plogis(as.matrix(marginal.means.prop.nest[,c('emmean','lower.CL','upper.CL')])))
# females attend the nest slightly more than males. This is presumably due to the fact that females are often in the colony during the day when it is high tide. They may then start incubating, but the male does not leave as he will take over again when the tide starts going out. 

# (2) PROPORTION OF TIME SPENT FORAGING
duration_behaviour_bird_phase_year$logit_prop_for <- qlogis(duration_behaviour_bird_phase_year$prop_for)
m_prop_for.phasexsex = lme(logit_prop_for~breeding.phase*sex, data=duration_behaviour_bird_phase_year, random=~1|year/birdID, method="ML") 
# check homogeneity of residuals in relation to fitted values and explanatory variables
windows()
plot(residuals(m_prop_for.phasexsex)~predict(m_prop_for.phasexsex)) # homogeneous
plot( m_prop_for.phasexsex, resid(., type = "p") ~ fitted(.) | breeding.phase * sex ) # similar variation among explanatory variables
model.sel.prop.for <- dredge(m_prop_for.phasexsex)
TableS2 <- make.table.from.dredge.output(model.sel.prop.for)
write.csv(TableS2, "output/TableS2 - Proportion_foraging.csv")
m.pars.prop.for = get.models(model.sel.prop.for,1,method='REML')[[1]]
emmeans.prop.for <- emmeans(m.pars.prop.for, pairwise~sex+breeding.phase) # sign difference among breeding phases (similar for the sexes, as the sex effect is additive in the most parsim model: eggs<chicks<post+ ; eggs<post-; chicks=post-, post+=post-.   
marginal.means.prop.for <- as.data.frame(emmeans.prop.for$emmeans)
cbind(marginal.means.prop.for[,c('breeding.phase','df')], plogis(as.matrix(marginal.means.prop.for[,c('emmean','lower.CL','upper.CL')])))

# (3) PROPORTION OF TIME SPENT FORAGING IN MARINE (or brackish) HABITAT
min.prop.for.marine <- min(duration_behaviour_bird_phase_year$prop_for_marine[duration_behaviour_bird_phase_year$prop_for_marine>0])
duration_behaviour_bird_phase_year$prop_for_marine[duration_behaviour_bird_phase_year$prop_for_marine==1] <- 1-min.prop.for.marine
duration_behaviour_bird_phase_year$prop_for_marine[duration_behaviour_bird_phase_year$prop_for_marine==0] <- min.prop.for.marine
duration_behaviour_bird_phase_year$logit_prop_for_marine <- qlogis(duration_behaviour_bird_phase_year$prop_for_marine)
m_for.marine.phasexsex = lme(logit_prop_for_marine~breeding.phase*sex, data=duration_behaviour_bird_phase_year, random=~1|year/birdID, method="ML") 
# check homogeneity of residuals in relation to fitted values and explanatory variables
windows()
plot(residuals(m_for.marine.phasexsex)~predict(m_for.marine.phasexsex)) # homogeneous
plot( m_for.marine.phasexsex, resid(., type = "p") ~ fitted(.) | breeding.phase * sex ) 
# similar, except for the post+ phase of females, which has somewhat smaller (variation in) residuals. 
model.sel.prop.for.marine <- dredge(m_for.marine.phasexsex)
TableS3 <- make.table.from.dredge.output(model.sel.prop.for.marine)
write.csv(TableS3, "output/TableS3 - Proportion_foraging_marine.csv")
m.pars.prop.for.marine = get.models(model.sel.prop.for.marine,1,method='REML')[[1]]
emmeans.prop.for.marine <- emmeans(m.pars.prop.for.marine, ~sex) # 
marginal.means.prop.for.marine <- as.data.frame(emmeans.prop.for.marine)
cbind(marginal.means.prop.for.marine[,c('sex','df')], plogis(as.matrix(marginal.means.prop.for.marine[,c('emmean','lower.CL','upper.CL')])))

# PREPARATION FOR FIGURE 5: discussion figure about explanation for large variation in male marine foraging
# plot proportion of foraging in marine waters for males only, per individual per year. 
males_prop_marine <- aggregate(cbind(duration_foraging/60,duration_foraging_marine/60)~birdID+sex, gps.breeding.data.behav[gps.breeding.data.behav$sex=='M'&(gps.breeding.data.behav$breeding.phase=='3.eggs'|gps.breeding.data.behav$breeding.phase=='4.chicks'),], sum)
males_prop_marine$prop_for_marine <- males_prop_marine$V2/males_prop_marine$V1
bird.data <- read.csv("data/raw/bird.data.csv", header=T) # reload bird data is biometry has now been added
bird.data <- read.csv("bird.data.csv", header=T) # code for NIOZ-server
males_prop_marine <- merge(males_prop_marine, bird.data[c(-12,-20),c('birdID','bodymass','headbill','head')]) # remove the biometry from the first two captures of 656
males_prop_marine$bill <- males_prop_marine$headbill - males_prop_marine$head
# analyse proportion of marine foraging of males in relation to headbill length and body mass, 
males_prop_marine$prop_for_marine_logit <- qlogis(males_prop_marine$prop_for_marine)
lm.prop.marine.bill <- lm(prop_for_marine_logit~bill, na.omit(males_prop_marine))
lm.prop.marine.mass <- lm(prop_for_marine_logit~bodymass, na.omit(males_prop_marine))
summary(lm.prop.marine.bill) # p=0.0371, R2=0.49, adj. R2 = 0.41
summary(lm.prop.marine.mass) # p=, R2=0.01, adj. R2 = -0.13

# Is there a correlation between the proportion of time spent foraging and the proportion of foraging time spent in marine foraging habitats among males, which you would expect if foraging in more distant freshwater habitats results in faster food intake (because of higher prey densities, or larger prey).
plot(prop_for~prop_for_marine, duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$sex=="M",])
duration_behaviour_males_phase_year <- duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$sex=="M",]
model.prop.for.vs.prop.for.mar.males <- lme(logit_prop_for~prop_for_marine, duration_behaviour_males_phase_year, random=~1|year/birdID, method="ML")
dredge(model.prop.for.vs.prop.for.mar.males) # some support for such a correlation... 
# However, it could also still mean that males foraging more in marine habitats have more chicks to take care of, which could similarly explain why they forage more. 
# This possibility could be excluded when only looking at the egg phase. Then the correlation disappears...
duration_behaviour_males_phase_year_eggs <- duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$sex=="M" & duration_behaviour_bird_phase_year$breeding.phase=='3.eggs',]
model.prop.for.vs.prop.for.mar.males.eggs <- lme(logit_prop_for~prop_for_marine, duration_behaviour_males_phase_year_eggs, random=~1|year/birdID, method="ML")
dredge(model.prop.for.vs.prop.for.mar.males.eggs) # really no correlation.   


########### FIGURE 5 #############
pdf("output/Fig5.pdf")
plot(prop_for_marine~bill, males_prop_marine, pch=21, cex=2, xlab='Bill length (mm)', ylab='Proportion marine foraging', cex.axis=1.2, cex.lab=1.3)
abline(lm(prop_for_marine~bill, na.omit(males_prop_marine)))
dev.off()
####### END OF FIGURE 5 #######################

# PREPARATION FOR ANALYSIS ON TIMING OF NEST ATTENDANCE AND FORAGING

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
  
# remove NA's:
gps.breeding.data.behav.sel <- na.omit(gps.breeding.data.behav[,c('birdID','date_time','hour_CEST','daynight','twilight_day_night','tidal_phase','breeding.phase','sex','at_nest','foraging','year','duration_behav','duration_nest_attendance','duration_foraging')])
gps.breeding.data.behav.sel$hour_f <- as.factor(gps.breeding.data.behav.sel$hour_CEST)
# calculate proportion of nest attendance and foraging per bird, hour, tidal phase and breeding phase (what about year?)
duration.behaviour.per.bird.hour.tidal.breeding.phase = aggregate(cbind(duration_behav/60,duration_nest_attendance/60,duration_foraging/60)~birdID+sex+hour_CEST+hour_f+daynight+twilight_day_night+tidal_phase+breeding.phase, gps.breeding.data.behav.sel, sum)
names(duration.behaviour.per.bird.hour.tidal.breeding.phase)[9:11] <- c("dur_tot","dur_at_nest","dur_for") 
# here, we use roughly the same criterium, that there should be at least 5 days of data per bird per breeding phase. Split into hours and 4 tidal phases (of approximately the same length), this means that there should be at least 1 hour of data per hour and tidal phase.  
hist(duration.behaviour.per.bird.hour.tidal.breeding.phase$dur_tot)
hist(duration.behaviour.per.bird.hour.tidal.breeding.phase$dur_tot[duration.behaviour.per.bird.hour.tidal.breeding.phase$dur_tot<10])
duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_at_nest <- duration.behaviour.per.bird.hour.tidal.breeding.phase$dur_at_nest/duration.behaviour.per.bird.hour.tidal.breeding.phase$dur_tot
duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_for <- duration.behaviour.per.bird.hour.tidal.breeding.phase$dur_for/duration.behaviour.per.bird.hour.tidal.breeding.phase$dur_tot
#duration.behaviour.per.bird.hour.tidal.breeding.phase <- duration.behaviour.per.bird.hour.tidal.breeding.phase[duration.behaviour.per.bird.hour.tidal.breeding.phase$dur_tot>2,]

duration.behaviour.per.bird.hour.tidal.breeding.phase$logit_at_nest <- qlogis(duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_at_nest)
duration.behaviour.per.bird.hour.tidal.breeding.phase$logit_foraging <- qlogis(duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_for)
min_at_nest <- min(duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_at_nest[duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_at_nest>0])
min_foraging <- min(duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_for[duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_for>0])
max_at_nest <- max(duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_at_nest[duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_at_nest<1])
max_foraging <- max(duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_for[duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_for<1])
duration.behaviour.per.bird.hour.tidal.breeding.phase$logit_at_nest[duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_at_nest==0] <- qlogis(min_at_nest)
duration.behaviour.per.bird.hour.tidal.breeding.phase$logit_foraging[duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_for==0] <- qlogis(min_foraging)
duration.behaviour.per.bird.hour.tidal.breeding.phase$logit_at_nest[duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_at_nest==1] <- qlogis(max_at_nest)
duration.behaviour.per.bird.hour.tidal.breeding.phase$logit_foraging[duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_for==1] <- qlogis(max_foraging)

### (4) TIMING OF NEST ATTENDANCE
duration.behaviour.per.bird.hour.tidal.eggs.chicks <- duration.behaviour.per.bird.hour.tidal.breeding.phase[duration.behaviour.per.bird.hour.tidal.breeding.phase$breeding.phase=='3.eggs'|duration.behaviour.per.bird.hour.tidal.breeding.phase$breeding.phase=='4.chicks',]
lm.timing.nest.attendance.hour <- lm(logit_at_nest~sex*breeding.phase*tidal_phase*hour_f, data=duration.behaviour.per.bird.hour.tidal.eggs.chicks, na.action="na.fail")
lm.timing.nest.attendance.daynight <- lm(logit_at_nest~sex*breeding.phase*tidal_phase*daynight, data=duration.behaviour.per.bird.hour.tidal.eggs.chicks, na.action="na.fail")
lm.timing.nest.attendance.daynight.twilight <- lm(logit_at_nest~sex*breeding.phase*tidal_phase*twilight_day_night, data=duration.behaviour.per.bird.hour.tidal.eggs.chicks, na.action="na.fail")
model.sel(lm.timing.nest.attendance.hour, lm.timing.nest.attendance.daynight, lm.timing.nest.attendance.daynight.twilight)
# check homogeneity of residuals in relation to fitted values and explanatory variables
windows()
plot(residuals(lm.timing.nest.attendance.hour)~predict(lm.timing.nest.attendance.hour)) 
qqnorm(resid(lm.timing.nest.attendance.hour))
plot(density(resid(lm.timing.nest.attendance.hour))) # more or less normally distributed
hist(duration.behaviour.per.bird.hour.tidal.eggs.chicks$logit_at_nest) # relatively many 0's and 1's
anova(lm.timing.nest.attendance.hour) # everything is significant, except the (near-significant) interaction between sex and breeding phase.
model.sel.timing.at.nest <- dredge(lm.timing.nest.attendance.hour) # the hour x sex x tidal_phase interaction and the hour x tidal_phase interaction are no longer supported. Only limited support for the breeding phase x sex interaction. 
TableS4 <- make.table.from.dredge.output(model.sel.timing.at.nest)
write.csv(TableS4, "output/TableS4 - Timing_nest_attendance.csv")
m.pars.timing.at.nest = get.models(model.sel.timing.at.nest,2)[[1]]
emmeans.timing.at.nest <- emmeans(m.pars.timing.at.nest, ~breeding.phase+hour_f+sex+tidal_phase+hour_f:sex+sex:tidal_phase) # 
marginal.means.timing.at.nest <- as.data.frame(emmeans.timing.at.nest)
marginal.means.timing.at.nest <- cbind(marginal.means.timing.at.nest[,c('sex','breeding.phase','hour_f','tidal_phase','df')], 
                                       plogis(as.matrix(marginal.means.timing.at.nest[,c('emmean','lower.CL','upper.CL')])))

# although the model with hour is best supported, for interpretation, I also run the daynight model, using birdID as random effect. 
m.timing.nest.attendance.daynight <- lme(logit_at_nest~sex*breeding.phase*tidal_phase*daynight, data=duration.behaviour.per.bird.hour.tidal.eggs.chicks, random=~1|birdID, na.action="na.fail", method="ML")
model.sel.timing.at.nest <- dredge(m.timing.nest.attendance.daynight)
m.pars.timing.at.nest = get.models(model.sel.timing.at.nest,1,method="REML")[[1]]
emmeans.timing.at.nest <- emmeans(m.pars.timing.at.nest, pairwise~breeding.phase + daynight + sex + tidal_phase + 
                                    breeding.phase:daynight + breeding.phase:sex + daynight:sex + 
                                    daynight:tidal_phase + sex:tidal_phase + breeding.phase:daynight:sex + 
                                    daynight:sex:tidal_phase) # 
pairwise.comp.timing.at.nest <- as.data.frame(emmeans.timing.at.nest$contrasts)
pairwise.comp.timing.at.nest[pairwise.comp.timing.at.nest$contrast%in%c(
  # male female comparison:
  '3.eggs day F low - 3.eggs day M low',
  '3.eggs night F low - 3.eggs night M low',
  '4.chicks day F low - 4.chicks day M low',
  '4.chicks night F low - 4.chicks night M low',
  # tide comparison of females during the day, egg phase:
  '3.eggs day F high - 3.eggs day F incoming',
  '3.eggs day F high - 3.eggs day F outgoing',
  '3.eggs day F high - 3.eggs day F low',
  '3.eggs day F incoming - 3.eggs day F outgoing',
  '3.eggs day F low - 3.eggs day F outgoing',
  '3.eggs day F low - 3.eggs day F incoming',
  # tide comparison of females during the day, chick phase:
  '4.chicks day F high - 4.chicks day F low',
  '4.chicks day F incoming - 4.chicks day F outgoing',
  '4.chicks day F low - 4.chicks day F outgoing',
  '4.chicks day F low - 4.chicks day F incoming')
  ,]
# among these pairwise comparisons, only the nest attendance of females during the day during incoming and outgoing tide are not sign. different.
marginal.means.timing.at.nest <- as.data.frame(emmeans.timing.at.nest)
cbind(marginal.means.timing.at.nest[,c('sex','breeding.phase','daynight','tidal_phase','df')], 
                                       plogis(as.matrix(marginal.means.timing.at.nest[,c('emmean','lower.CL','upper.CL')])))

### (5) TIMING OF FORAGING
# also includes the post-breeding phase (either successful or not)
lm.timing.foraging <- lm(logit_foraging~sex*breeding.phase*tidal_phase*hour_f, data=duration.behaviour.per.bird.hour.tidal.breeding.phase, na.action="na.fail")
# check homogeneity of residuals in relation to fitted values and explanatory variables
plot(residuals(lm.timing.foraging)~predict(lm.timing.foraging)) 
qqnorm(resid(lm.timing.foraging))
plot(density(resid(lm.timing.foraging))) # more or less normally distributed
hist(duration.behaviour.per.bird.hour.tidal.breeding.phase$logit_foraging) # many 0's, but never 1's. Perhaps use some sort of zero-inflated model, except that of course the logit transformation causes zeros to disappear... 

# lots of factors to compare, so easier to use anova to see what is significant:
anova(lm.timing.foraging)
model.sel.timing.foraging <- dredge(lm.timing.foraging) # the most parsimonious model from the dredge function indeed only excludes breeding.phase x hour x tidal.phase, hour x sex x tidal.phase and breeding.phase x hour x sex x tidal.phase
dim(model.sel.timing.foraging)
TableS5 <- make.table.from.dredge.output(model.sel.timing.foraging) # way to many models when the full 4-way interaction model is used. Perhaps simplify by only distinguishing day and night.
write.csv(TableS5, "output/TableS5 - Timing_foraging.csv")
m.pars.timing.foraging = get.models(model.sel.timing.foraging,1)[[1]]
emmeans.timing.foraging <- emmeans(m.pars.timing.foraging, ~breeding.phase+hour_f+sex+tidal_phase+hour_f:sex+sex:tidal_phase) # 
marginal.means.timing.foraging <- as.data.frame(emmeans.timing.foraging)
marginal.means.timing.foraging <- cbind(marginal.means.timing.foraging[,c('sex','breeding.phase','hour_f','tidal_phase','df')], 
                                        plogis(as.matrix(marginal.means.timing.foraging[,c('emmean','lower.CL','upper.CL')])))
