source("functions.R") # to have the most updated version of the functions
Sys.setenv(TZ='Europe/Amsterdam')
gps.breeding.data.behav <- gps.breeding.data.behav.sel

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

# Statistical analysis
# Discussion on whether (or not) to use p-values with mixed-effects models (https://stats.stackexchange.com/questions/22988/how-to-obtain-the-p-value-check-significance-of-an-effect-in-a-lme4-mixed-mode)
# I decided to go for model selection approach, as the use of p-values in mixed-effects models is arbritary, and AIC provides more useful information on the level of support for certain effects (instead of just a boundary value for whether to reject a null hypothesis.

# (1) PROPORTION OF TIME SPENT ATTENDING THE NEST (only comparing egg incubation and chick rearing phase)
duration_behaviour_bird_phase_year_egg_chicks <- duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$breeding.phase%in%c('3.eggs','4.chicks'),]
min_prop_nest <- min(duration_behaviour_bird_phase_year_egg_chicks$prop_nest[duration_behaviour_bird_phase_year_egg_chicks$prop_nest>0])
duration_behaviour_bird_phase_year_egg_chicks$prop_nest[duration_behaviour_bird_phase_year_egg_chicks$prop_nest==0] = min_prop_nest
duration_behaviour_bird_phase_year_egg_chicks$logit_prop_nest <- qlogis(duration_behaviour_bird_phase_year_egg_chicks$prop_nest)
m_prop_nest.phasexsex = lme(logit_prop_nest~breeding.phase*sex, data=duration_behaviour_bird_phase_year_egg_chicks, random=~1|year/birdID, method="ML") 
model.sel.prop.at.nest <- dredge(m_prop_nest.phasexsex)
TableS1 <- make.table.from.dredge.output(model.sel.prop.at.nest)
write.csv(TableS1, "output/TableS1 - Proportion_at_nest.csv")
sapply(get.models(model.sel.prop.at.nest, subset=T), predict, new.data=expand.grid(breeding.phase=c('3.eggs','4.breeding.phase'), sex=c('F','M'))) # predicted values for each model in the model selection table
# useful link how to model average predictions (though without confidence intervals): https://sites.google.com/site/rforfishandwildlifegrads/home/mumin_usage_examples?authuser=0 
# get predictions for the most parsimonious model, run with REML: 
# effects returned by predict() are conditional effects (i.e. these are conditioned on certain (reference) levels of factors), while emmeans() returns marginal means, since the effects are “marginalized” (or “averaged”) over the levels of factors.
# it seems to me that marginal means are preferred. 
m.pars.prop.at.nest = get.models(model.sel.prop.at.nest,1,method='REML')[[1]]
marginal.means.prop.nest <- as.data.frame(emmeans(m.pars.prop.at.nest, ~breeding.phase))
marginal.means.prop.nest <- cbind(marginal.means.prop.nest[,c('breeding.phase','df')], plogis(as.matrix(marginal.means.prop.nest[,c('emmean','lower.CL','upper.CL')])))

# (2) PROPORTION OF TIME SPENT FORAGING
duration_behaviour_bird_phase_year$logit_prop_for <- qlogis(duration_behaviour_bird_phase_year$prop_for)
m_prop_for.phasexsex = lme(logit_prop_for~breeding.phase*sex, data=duration_behaviour_bird_phase_year, random=~1|year/birdID, method="ML") 
model.sel.prop.for <- dredge(m_prop_for.phasexsex)
TableS2 <- make.table.from.dredge.output(model.sel.prop.for)
write.csv(TableS2, "output/TableS2 - Proportion_foraging.csv")
m.pars.prop.for = get.models(model.sel.prop.for,1,method='REML')[[1]]
emmeans.prop.for <- emmeans(m.pars.prop.for, pairwise~sex+breeding.phase) # sign difference within females: eggs<chicks<post+ ; chicks=post-, post+=post-, )  vs chicks, eggs vs  male.eggs vs female.eggs, female.eggs vs female.chicks, 
marginal.means.prop.for <- as.data.frame(emmeans.prop.for$emmeans)
cbind(marginal.means.prop.for, plogis(as.matrix(marginal.means.prop.for[,c('emmean','lower.CL','upper.CL')])))

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
