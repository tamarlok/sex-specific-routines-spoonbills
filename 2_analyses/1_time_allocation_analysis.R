source("functions.R") # to have the most updated version of the functions
Sys.setenv(TZ='Europe/Amsterdam')

gps.breeding.data.behav$birdyear = as.factor(paste(gps.breeding.data.behav$birdID, gps.breeding.data.behav$year, sep="_"))
table(gps.breeding.data.behav$breeding.phase)

gps.breeding.data.behav = gps.breeding.data.behav[order(gps.breeding.data.behav$birdID, gps.breeding.data.behav$date_time),]

table(gps.breeding.data.behav$birdID, gps.breeding.data.behav$yday_CEST, gps.breeding.data.behav$year)
names(gps.breeding.data.behav)

# remove NA's: (this was already done, as no rows are removed after running the below line)
gps.breeding.data.behav <- na.omit(gps.breeding.data.behav[,c('birdID','birdyear','date_time','yday_CEST','hour_CEST','month','week','daynight','twilight_day_night','sunset','sunrise','dusk','dawn','tidal_phase','habitat','habitat_type','habitat_salinity','land_water','hatch_day','day_rel_hatching','week_rel_hatching','breeding.phase','breeding.phase2','breeding.phase.nr','sex','behaviour','behaviour2', 'lat.nest','lon.nest','distance.to.nest','at_nest','foraging','foraging_marine','year','diel_rad','tidal_stage_rad','solar_stage_rad')])
gps.breeding.data.behav$hour_f <- as.factor(gps.breeding.data.behav$hour_CEST)
gps.breeding.data.behav$sex <- as.factor(gps.breeding.data.behav$sex)
gps.breeding.data.behav$birdID <- as.factor(gps.breeding.data.behav$birdID)
gps.breeding.data.behav$year <- as.factor(gps.breeding.data.behav$year)
gps.breeding.data.behav$breeding.phase <- as.factor(gps.breeding.data.behav$breeding.phase)
gps.breeding.data.behav$sexxbp = interaction(gps.breeding.data.behav$sex, gps.breeding.data.behav$breeding.phase)
gps.breeding.data.behav.phase34 <- gps.breeding.data.behav[gps.breeding.data.behav$breeding.phase%in%c('3.eggs','4.chicks'),]

# Statistical analysis
# Discussion on whether (or not) to use p-values with mixed-effects models (https://stats.stackexchange.com/questions/22988/how-to-obtain-the-p-value-check-significance-of-an-effect-in-a-lme4-mixed-mode)
# I decided to go for model selection approach, as the use of p-values in mixed-effects models is arbritary, and AIC provides more useful information on the level of support for certain effects (instead of just a boundary value for whether to reject a null hypothesis).

# (1) PROBABILITY OF ATTENDING THE NEST (only comparing egg incubation and chick rearing phase)

# trying glmer on raw data; this ignores differences in duration associated with GPS-behaviour fixes. Alternatively, we could also exclude points associated with durations >10 minutes.

a = Sys.time()
glmer.nestprob <- glmer(at_nest~breeding.phase*sex+(1|year)+(1|birdID), gps.breeding.data.behav.phase34, family='binomial', na.action='na.fail', nAGQ=0) # nACQ=0 makes convergence about 4 times faster. 
Sys.time() - a
glmer.nestprob # (Intercept)=0.01051

# Check model assumptions for glmer models
# https://stats.stackexchange.com/questions/524376/testing-glmer-model-assumptions-optionally-in-r
# using package DHARMa:
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#installing-loading-and-citing-the-package
# (1) check for over- or underdispersion of the data:
simOutput.glmer.nestprob <- simulateResiduals(glmer.nestprob, plot=F)
residuals(simOutput.glmer.nestprob)
windows()
plot(simOutput.glmer.nestprob)
# plots can also be called separately:
plotQQunif(simOutput.glmer.nestprob)
plotResiduals(simOutput.glmer.nestprob)

# check for homogeneity of residuals in relation to explanatory variables:
modsel.nestprob <- dredge(glmer.nestprob)
Table1a <- make.table.from.dredge.output(modsel.nestprob)
Table1a
write.csv(Table1a, "output/Table3a - Proportion_at_nest with glmer.csv")
m.pars.nestprob = get.models(modsel.nestprob,1)[[1]] # with glmer, you apparently cannot choose between ML and REML
marginal.emmeans.nestprob <- emmeans(m.pars.nestprob, pairwise~breeding.phase*sex)
marginal.emmeans.nestprob.df <- as.data.frame(marginal.emmeans.nestprob$emmeans)
marginal.emmeans.nestprob.df  <- cbind(marginal.emmeans.nestprob.df[,c('sex','breeding.phase','df')], plogis(as.matrix(marginal.emmeans.nestprob.df[,c('emmean','asymp.LCL','asymp.UCL')])))
marginal.emmeans.nestprob.df
pairs(marginal.emmeans.nestprob)
pairs(marginal.emmeans.nestprob, by='sex')
pairs(marginal.emmeans.nestprob, by='breeding.phase') # no sex differences in nest attendance during either incubation or chick-rearing. 

# although the interaction between breeding phase and sex is supported, the pairwise compairsons between the sexes within breeding phases are not signifciant (p=0.71 during incubation and p=0.084 during chick-rearing). Both sexes attend the nest more during egg incubation than during chick-rearing. females attend the nest slightly more than males. The somehwat higher attendance of females during chick-rearing may be caused by the fact that females are often in the colony during the day when it is high tide (after having fed the chicks). They may also then attend the nest, but the male does not leave as he will take over again when the tide starts going out. 

# (2) PROBABILITY OF FORAGING

glmer.forprob <- glmer(foraging~breeding.phase*sex+(1|year)+(1|birdID), gps.breeding.data.behav, family='binomial', na.action='na.fail', nAGQ=0) 
summary(glmer.forprob)
modsel.forprob <- dredge(glmer.forprob)

Table1b <- make.table.from.dredge.output(modsel.forprob)
write.csv(Table1b, "output/Table1b - Proportion_foraging with glmer.csv")

marginal.means.forprob <- emmeans(glmer.forprob, ~breeding.phase*sex) 
marginal.means.forprob.df <- as.data.frame(marginal.means.forprob)
marginal.means.forprob.df  <- cbind(marginal.means.forprob.df[,c('sex','breeding.phase','df')], plogis(as.matrix(marginal.means.forprob.df[,c('emmean','asymp.LCL','asymp.UCL')])))
marginal.means.forprob.df
forprob.phasecomp = pairs(marginal.means.forprob, by='sex') # this compares "only" 5 estimates (within each sex)
forprob.phasecomp = as.data.frame(forprob.phasecomp)
forprob.phasecomp$z.ratio = round(forprob.phasecomp$z.ratio,2)
forprob.phasecomp$sign = ifelse(forprob.phasecomp$p.value<0.001, "***",
                                ifelse(forprob.phasecomp$p.value<0.01, "**",
                                       ifelse(forprob.phasecomp$p.value<0.05, "*", "n.s.")))

# extract female comparisons and make table out of it:
forprob.phasecomp.F = forprob.phasecomp[forprob.phasecomp$sex=="F",]
forprob.phasecomp.F.z.ratio = rbind(forprob.phasecomp.F$z.ratio[1:4],
      c("",forprob.phasecomp.F$z.ratio[5:7]),
      c("","",forprob.phasecomp.F$z.ratio[8:9]),
      c("","","",forprob.phasecomp.F$z.ratio[10]))
forprob.phasecomp.F.sign = rbind(forprob.phasecomp.F$sign[1:4],
                                        c("",forprob.phasecomp.F$sign[5:7]),
                                        c("","",forprob.phasecomp.F$sign[8:9]),
                                        c("","","",forprob.phasecomp.F$sign[10]))
forprob.phasecomp.F.z.ratio.sign = matrix(paste(forprob.phasecomp.F.z.ratio, forprob.phasecomp.F.sign, sep=""), ncol=4)
write.table(forprob.phasecomp.F.z.ratio.sign, 'clipboard', sep='\t')

# extract female comparisons and make table out of it:
forprob.phasecomp.M = forprob.phasecomp[forprob.phasecomp$sex=="M",]
forprob.phasecomp.M.z.ratio = rbind(forprob.phasecomp.M$z.ratio[1:4],
                                  c("",forprob.phasecomp.M$z.ratio[5:7]),
                                  c("","",forprob.phasecomp.M$z.ratio[8:9]),
                                  c("","","",forprob.phasecomp.M$z.ratio[10]))
forprob.phasecomp.M.sign = rbind(forprob.phasecomp.M$sign[1:4],
                               c("",forprob.phasecomp.M$sign[5:7]),
                               c("","",forprob.phasecomp.M$sign[8:9]),
                               c("","","",forprob.phasecomp.M$sign[10]))
forprob.phasecomp.M.z.ratio.sign = matrix(paste(forprob.phasecomp.M.z.ratio, forprob.phasecomp.M.sign, sep=""), ncol=4)
write.table(forprob.phasecomp.M.z.ratio.sign, 'clipboard', sep='\t')


pairs(marginal.means.forprob) # while this compares 10 estimates, it also makes irrelevant comparisons (like chicks.F vs eggs.M)

# for females: eggs<pre<chicks<(post.unsuc/post.suc)
# for males: prop foraging is lowest and similar during pre-breeding and egg phase. (pre+egg)<post.unsuc.<chicks<post.suc;

forprob.sexcomp = pairs(marginal.means.forprob, by='breeding.phase')
forprob.sexcomp = as.data.frame(forprob.sexcomp)
forprob.sexcomp$sign = ifelse(forprob.sexcomp$p.value<0.001, "***",
                              ifelse(forprob.sexcomp$p.value<0.01, "**",
                                     ifelse(forprob.sexcomp$p.value<0.05, "*", "n.s.")))
write.table(paste(round(forprob.sexcomp$z.ratio, 2), forprob.sexcomp$sign, sep=""), 'clipboard', sep='\t')
# in all breeding phases, females forage more than males. The difference is largest during pre- en unsuccessful post-breeding .


# (3) PROPORTION OF TIME SPENT FORAGING IN MARINE (or brackish) HABITAT
gps.breeding.data.behav.marfor <- gps.breeding.data.behav[gps.breeding.data.behav$foraging==1,]
glmer.marforprob <- glmer(foraging_marine~breeding.phase*sex+(1|birdID)+(1|year), gps.breeding.data.behav.marfor, family='binomial', na.action='na.fail', nAGQ=0) 
modsel.marforprob <- dredge(glmer.marforprob)

Table1c <- make.table.from.dredge.output(modsel.marforprob)
write.csv(Table1c, "output/Table1c - Proportion_foraging_marine with glmer.csv")

marginal.emmeans.marforprob <- emmeans(glmer.marforprob, ~breeding.phase*sex)
marginal.emmeans.marforprob.df <- as.data.frame(marginal.emmeans.marforprob)
marginal.emmeans.marforprob.df  <- cbind(marginal.emmeans.marforprob.df[,c('sex','breeding.phase','df')], plogis(as.matrix(marginal.emmeans.marforprob.df[,c('emmean','asymp.LCL','asymp.UCL')])))
marginal.emmeans.marforprob.df
marginal.emmeans.marforprob$contrasts[c(1:4,10:12,18:19,25,5,14,22,29,35:45),] 
marforprob.phasecomp <- as.data.frame(pairs(marginal.emmeans.marforprob, by='sex'))
marforprob.phasecomp$z.ratio <- round(marforprob.phasecomp$z.ratio, 2)
marforprob.phasecomp$sign <- ifelse(marforprob.phasecomp$p.value<0.05, "*","")
# extract female comparisons and make table out of it:
marforprob.phasecomp.F = marforprob.phasecomp[marforprob.phasecomp$sex=="F",]
marforprob.phasecomp.F.z.ratio = rbind(marforprob.phasecomp.F$z.ratio[1:4],
                                    c("",marforprob.phasecomp.F$z.ratio[5:7]),
                                    c("","",marforprob.phasecomp.F$z.ratio[8:9]),
                                    c("","","",marforprob.phasecomp.F$z.ratio[10]))
marforprob.phasecomp.F.sign = rbind(marforprob.phasecomp.F$sign[1:4],
                                 c("",marforprob.phasecomp.F$sign[5:7]),
                                 c("","",marforprob.phasecomp.F$sign[8:9]),
                                 c("","","",marforprob.phasecomp.F$sign[10]))
marforprob.phasecomp.F.z.ratio.sign = matrix(paste(marforprob.phasecomp.F.z.ratio, marforprob.phasecomp.F.sign, sep=""), ncol=4)
write.table(marforprob.phasecomp.F.z.ratio.sign, 'clipboard', sep='\t')
# females: (pre=post+=post-)<(eggs=chicks)
# extract female comparisons and make table out of it:
marforprob.phasecomp.M = marforprob.phasecomp[marforprob.phasecomp$sex=="M",]
marforprob.phasecomp.M.z.ratio = rbind(marforprob.phasecomp.M$z.ratio[1:4],
                                       c("",marforprob.phasecomp.M$z.ratio[5:7]),
                                       c("","",marforprob.phasecomp.M$z.ratio[8:9]),
                                       c("","","",marforprob.phasecomp.M$z.ratio[10]))
marforprob.phasecomp.M.sign = rbind(marforprob.phasecomp.M$sign[1:4],
                                    c("",marforprob.phasecomp.M$sign[5:7]),
                                    c("","",marforprob.phasecomp.M$sign[8:9]),
                                    c("","","",marforprob.phasecomp.M$sign[10]))
marforprob.phasecomp.M.z.ratio.sign = matrix(paste(marforprob.phasecomp.M.z.ratio, marforprob.phasecomp.M.sign, sep=""), ncol=4)
write.table(marforprob.phasecomp.M.z.ratio.sign, 'clipboard', sep='\t')

# males: (pre=chicks)>(eggs=post+=post-)
pairs(marginal.emmeans.marforprob, by='breeding.phase')
marforprob.sexcomp = pairs(marginal.emmeans.marforprob, by='breeding.phase')
marforprob.sexcomp = as.data.frame(marforprob.sexcomp)
marforprob.sexcomp$sign = ifelse(marforprob.sexcomp$p.value<0.001, "***",
                              ifelse(marforprob.sexcomp$p.value<0.01, "**",
                                   ifelse(marforprob.sexcomp$p.value<0.05, "*", "n.s.")))

write.table(paste(round(marforprob.sexcomp$z.ratio, 2), marforprob.sexcomp$sign, sep=""), 'clipboard', sep='\t')
# pre: females<males
# eggs: females<males
# chicks: females<males
# post+: females<males
# post-: females<males

# HABITAT USE IN RELATION TO WITHIN-SEX BODY SIZE AND MASS:
# PREPARATION FOR FIGURE 5: discussion figure about explanation for large variation in male marine foraging
# does the probability to forage in marine habitat depend on bill, tarsus or body mass?
bird.data <- read.csv("data/raw/bird.data.csv", header=T) # reload bird data is biometry has now been added
bird.data$bill <- bird.data$headbill - bird.data$head
prop.for.marine.biometrics = merge(gps.breeding.data.behav.marfor, bird.data[c(-12,-20),c('birdID','bodymass','bill','tarsus')], all.x=T)
prop.for.marine.biometrics.females <- prop.for.marine.biometrics[prop.for.marine.biometrics$sex=='F',]
prop.for.marine.biometrics.males <- prop.for.marine.biometrics[prop.for.marine.biometrics$sex=='M',]

# analysis of bill length:
prop.for.marine.bill.females <- prop.for.marine.biometrics.females[is.na(prop.for.marine.biometrics.females$bill)==F,]
glmer.marforprob.females.bill <- glmer(foraging_marine~bill+(1|birdID)+(1|year), prop.for.marine.bill.females, family='binomial', na.action='na.fail', nAGQ=0)
glmer.marforprob.females.nobill <- glmer(foraging_marine~1+(1|birdID)+(1|year), prop.for.marine.bill.females, family='binomial', na.action='na.fail', nAGQ=0)
anova(glmer.marforprob.females.bill, glmer.marforprob.females.nobill)
unique(prop.for.marine.bill.females$birdID)
prop.for.marine.bill.males <- prop.for.marine.biometrics.males[is.na(prop.for.marine.biometrics.males$bill)==F,]
glmer.marforprob.males.bill <- glmer(foraging_marine~bill+(1|birdID)+(1|year), prop.for.marine.bill.males, family='binomial', na.action='na.fail', nAGQ=0)
glmer.marforprob.males.nobill <- glmer(foraging_marine~1+(1|birdID)+(1|year), prop.for.marine.bill.males, family='binomial', na.action='na.fail', nAGQ=0)
anova(glmer.marforprob.males.bill, glmer.marforprob.males.nobill)
unique(prop.for.marine.bill.males$birdID)

# analysis of tarsus length:
prop.for.marine.tarsus.females <- prop.for.marine.biometrics.females[is.na(prop.for.marine.biometrics.females$tarsus)==F,]
glmer.marforprob.females.tarsus <- glmer(foraging_marine~tarsus+(1|birdID)+(1|year), prop.for.marine.tarsus.females, family='binomial', na.action='na.fail', nAGQ=0)
glmer.marforprob.females.notarsus <- glmer(foraging_marine~1+(1|birdID)+(1|year), prop.for.marine.tarsus.females, family='binomial', na.action='na.fail', nAGQ=0)
anova(glmer.marforprob.females.tarsus, glmer.marforprob.females.notarsus)
unique(prop.for.marine.tarsus.females$birdID)

prop.for.marine.tarsus.males <- prop.for.marine.biometrics.males[is.na(prop.for.marine.biometrics.males$tarsus)==F,]
glmer.marforprob.males.tarsus <- glmer(foraging_marine~tarsus+(1|birdID)+(1|year), prop.for.marine.tarsus.males, family='binomial', na.action='na.fail', nAGQ=0)
glmer.marforprob.males.notarsus <- glmer(foraging_marine~1+(1|birdID)+(1|year), prop.for.marine.tarsus.males, family='binomial', na.action='na.fail', nAGQ=0)
anova(glmer.marforprob.males.tarsus, glmer.marforprob.males.notarsus)
unique(prop.for.marine.tarsus.males$birdID)

# for Figure 7, calculate the mean proportion of marine foraging per bird:
birds_prop_for_marine <- aggregate(cbind(foraging,foraging_marine)~birdID+sex+bill+breeding.phase, prop.for.marine.biometrics[is.na(prop.for.marine.biometrics$bill)==F,], sum)
birds_prop_for_marine$prop_for_marine <- birds_prop_for_marine$foraging_marine/birds_prop_for_marine$foraging
birds_prop_for_marine[order(birds_prop_for_marine$birdID, birds_prop_for_marine$breeding.phase),] # 6068 foraged a lot in the freshwater Lauwersmeer during postbreeding, thereby changing the graph a little bit.
birds_prop_for_marine <- aggregate(cbind(foraging,foraging_marine)~birdID+sex+bill, prop.for.marine.biometrics[is.na(prop.for.marine.biometrics$bill)==F,], sum)
birds_prop_for_marine$prop_for_marine <- birds_prop_for_marine$foraging_marine/birds_prop_for_marine$foraging

# PREPARATION FOR ANALYSIS ON TIMING OF NEST ATTENDANCE AND FORAGING

### (4) TIMING OF NEST ATTENDANCE

# check match between hour_CEST and diel_rad:
unique(gps.breeding.data.behav.phase34[,c('hour_CEST','diel_rad')])

# useful info online:
# https://stats.stackexchange.com/questions/145527/model-selection-and-comparison-in-gamm-using-r-mgcv

# using generalized additive mixed modelling
bam.nest.attendance.full <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") 

bam.nest.attendance.without_4way_interaction <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML")
AIC(bam.nest.attendance.full, bam.nest.attendance.without_4way_interaction) # AIC increases by nearly 200 points when 4way interaction is removed. Therefore, full model is best-supported. 

# Compare all 32 possible models (with in all models, the main effectsexxbp (i.e. the interaction between sex and breeding phase) kept in):
# 4 models with one two-way interaction:
bam.nest.bxt <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #1
# check that K=10 by default:
bam.nest.bxt.K10 <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase, k=10)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #1
bam.nest.bxt.K20 <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase, k=20)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #1
bam.nest.bxt.K20.birdK20 <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase, k=20)+s(birdID, bs='re', k=20)+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #1
bam.nest.bxt.K20.birdK10 <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase, k=20)+s(birdID, bs='re', k=10)+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #1
AIC(bam.nest.bxt, bam.nest.bxt.K10, bam.nest.bxt.K20, bam.nest.bxt.K20.birdK20, bam.nest.bxt.K20.birdK10) # defining K for random effects seems not to do anything. This is not surprising, as no smoother should be involved for estimating the random effect.
# The default indeed seems to be K=10. Check if this is also the case when there is a te smoother.
bam.nest.bxd <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #2
bam.nest.sxt <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #3
bam.nest.sxd <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #4
# 6 models with two two-way interactions:
bam.nest.bxt.bxd <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #5
bam.nest.bxt.sxt <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #6
bam.nest.bxt.sxd <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #7
bam.nest.bxd.sxt <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #8
bam.nest.bxd.sxd <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #9
bam.nest.sxt.sxd <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=sex)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #10
# 4 models with 3-way interaction bxsxt (and two or more two-way interactions)
bam.nest.bxsxt <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #11
bam.nest.bxsxt.bxd <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(diel_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #12
bam.nest.bxsxt.sxd <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #13
bam.nest.bxsxt.bxd.sxd <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(diel_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #14
# 4 models with 3-way interaction bxsxd (and two or more two-way interactions)
bam.nest.bxsxd <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #15
bam.nest.bxsxd.bxt <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #16
bam.nest.bxsxd.sxt <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #17
bam.nest.bxsxd.bxt.sxt <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #18
# 4 models with 3-way interaction bxtxd
bam.nest.bxtxd <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #19

# check the K=10 for the te smoother:
bam.nest.bxtxd.teK10 <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase, k=10)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML")
bam.nest.bxtxd.tideK10 <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=breeding.phase, k=10)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML")
AIC(bam.nest.bxtxd, bam.nest.bxtxd.tideK10, bam.nest.bxtxd.teK10)

bam.nest.bxtxd.sxt <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #20
bam.nest.bxtxd.sxd <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #21
bam.nest.bxtxd.sxt.sxd <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=sex)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #22
# 4 models with 3-way interaction sxtxd
bam.nest.sxtxd <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sex)+s(tidal_stage_rad, bs='cc', by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #23
bam.nest.sxtxd.bxt <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sex)+s(tidal_stage_rad, bs='cc', by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #24
bam.nest.sxtxd.bxd <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sex)+s(tidal_stage_rad, bs='cc', by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(diel_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #25
bam.nest.sxtxd.bxt.bxd <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sex)+s(tidal_stage_rad, bs='cc', by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #26
# 4 models with two 3-way interactions (and all two-way interactions)
bam.nest.bxsxt.bxsxd.bxtxd <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(diel_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #27
bam.nest.bxsxt.bxsxd.sxtxd <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(diel_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #28
bam.nest.bxsxt.bxtxd.sxtxd <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(diel_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #29
bam.nest.bxsxd.bxtxd.sxtxd <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #30
# model with all four 3-way interactions:
bam.nest.bxsxt.bxsxd.bxtxd.sxtxd <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #31

# 4-way model:
bam.nest.bxsxdxt <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #32

#rm(bam.nest.model.list)
model.names <- ls()[grep("bam.nest",ls())]
bam.nest.model.list <- mget(model.names)

# retrieve deviance and AIC for each model
AIC_deviance <- function(x) {
  K = sum(x$edf)
  aic_value = x$aic
  aic_value_alt = AIC(x)
  AICc_value = AICc(x)
  round(c(K=K, AIC1=aic_value, AIC2=aic_value_alt, AICc=AICc_value),2)
}
K_aic_values <- t(sapply(bam.nest.model.list,AIC_deviance))
model_aic_df_nesting <- as.data.frame(model_aic_df_nesting)
model_aic_df_nesting <- model_aic_df_nesting[order(model_aic_df_nesting$AICc),] # the full model is the best. 
model_aic_df_nesting$Model <- substr(rownames(model_aic_df_nesting),10,40)
model_aic_df_nesting$dAICc = round(model_aic_df_nesting$AICc-min(model_aic_df_nesting$AICc),2)
write.csv(model_aic_df_nesting[,c('Model','K','dAICc')], "output/TableS2_modsel_nest_attendance_rhythm.csv")
# AIC difference between top and 2nd model:
model_aic_df_nesting$AICc[2]-model_aic_df_nesting$AICc[1]

### (5) TIMING OF FORAGING - also including the pre- and post-breeding phase! 
bam.foraging.full <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML")
bam.foraging.without_4way_interaction <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML")
AIC(bam.foraging.full, bam.foraging.without_4way_interaction) # AIC increases by nearly 400 points when 4way interaction is removed. Therefore, full model is best-supported. 

# Compare all 32 possible models (with in all models, the main effectsexxbp (i.e. the interaction between sex and breeding phase) kept in):
# 4 models with one two-way interaction:
bam.foraging.bxt <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #1
bam.foraging.bxd <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #2
bam.foraging.sxt <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #3
bam.foraging.sxd <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #4
# 6 models with two two-way interactions:
bam.foraging.bxt.bxd <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #5
bam.foraging.bxt.sxt <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #6
bam.foraging.bxt.sxd <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #7
bam.foraging.bxd.sxt <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #8
bam.foraging.bxd.sxd <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #9
bam.foraging.sxt.sxd <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=sex)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #10

# 4 models with 3-way interaction bxsxt (and two or more two-way interactions)
bam.foraging.bxsxt <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #11
bam.foraging.bxsxt.bxd <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(diel_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #12
bam.foraging.bxsxt.sxd <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #13
bam.foraging.bxsxt.bxd.sxd <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(diel_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #14

# 4 models with 3-way interaction bxsxd (and two or more two-way interactions)
bam.foraging.bxsxd <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #15
bam.foraging.bxsxd.bxt <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #16
bam.foraging.bxsxd.sxt <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #17
bam.foraging.bxsxd.bxt.sxt <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #18

# 4 models with 3-way interaction bxtxd
bam.foraging.bxtxd <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #19
bam.foraging.bxtxd.sxt <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #20
bam.foraging.bxtxd.sxd <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #21
bam.foraging.bxtxd.sxt.sxd <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=sex)+s(diel_rad, bs='cc', by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #22

# 4 models with 3-way interaction sxtxd
bam.foraging.sxtxd <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sex)+s(tidal_stage_rad, bs='cc', by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #23
bam.foraging.sxtxd.bxt <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sex)+s(tidal_stage_rad, bs='cc', by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #24
bam.foraging.sxtxd.bxd <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sex)+s(tidal_stage_rad, bs='cc', by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(diel_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #25
bam.foraging.sxtxd.bxt.bxd <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sex)+s(tidal_stage_rad, bs='cc', by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #26

# 4 models with two 3-way interactions (and all two-way interactions)
bam.foraging.bxsxt.bxsxd.bxtxd <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(diel_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #27
bam.foraging.bxsxt.bxsxd.sxtxd <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(diel_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #28
bam.foraging.bxsxt.bxtxd.sxtxd <- bam(foraging~sexxbp+s(tidal_stage_rad, bs='cc', by=sexxbp)+s(diel_rad, bs='cc', by=breeding.phase)+s(diel_rad, bs='cc', by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #29
bam.foraging.bxsxd.bxtxd.sxtxd <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(tidal_stage_rad, bs='cc', by=sex)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #30

# model with all four 3-way interactions:
bam.foraging.bxsxt.bxsxd.bxtxd.sxtxd <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=breeding.phase)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sex)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #31

# 4-way model:
bam.foraging.bxsxdxt <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="ML") #32

# run the full 4-way model with more degrees of freedom for the smoothers:

rm(model.names.bam.foraging, bam_foraging_aic_df)
model.names.bam.foraging <- ls()[grep("bam.foraging",ls())]
bam.model.list.foraging <- mget(model.names.bam.foraging)

model_aic_df_foraging <- as.data.frame(t(sapply(bam.model.list.foraging,AIC_deviance)))
model_aic_df_foraging <- model_aic_df_foraging[order(model_aic_df_foraging$AICc),] # the full model is the best. 
model_aic_df_foraging$Model <- substr(rownames(model_aic_df_foraging),10,40)
model_aic_df_foraging$dAICc = round(model_aic_df_foraging$AICc-min(model_aic_df_foraging$AICc),2)
model_aic_df_foraging # model 2 and 3 are exactly the same in terms of K and AIC1 (but not in AIC2 and AICc as these are corrected in some way...)
model_aic_df_foraging <- model_aic_df_foraging[-2,] # remove the 2nd model
model_aic_df_foraging$Model <- substr(model_aic_df_foraging$Model,5,40)
write.csv(model_aic_df_foraging[,c('Model','K','dAICc')], "output/TableS3_modsel_foraging_rhythm.csv")
# AIC difference between top and 2nd model:
model_aic_df_nesting$AICc[2]-model_aic_df_nesting$AICc[1]

# check convergence of complex models:
bam.foraging.bxsxdxt$converged
bam.foraging.bxsxt.bxsxd.bxtxd.sxtxd$converged

# run best-supported models with REML estimation:
bam.nest.bxsxdxt.REML <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="REML") #32
bam.foraging.bxsxdxt.REML <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="REML") #32

bam.foraging.bxsxdxt.REML <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="REML") #32
bam.foraging.full.k10.REML <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp, k=10)+s(tidal_stage_rad, bs='cc', by=sexxbp, k=10)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sexxbp, k=10)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="REML")


save.image("data/tmp/time.allocation.results.20240206.RData")
