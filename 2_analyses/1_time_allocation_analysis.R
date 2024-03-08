# reload bird.data, breeding.data and gps.breeding.data.behav (to be able to start running the script from this point onward)
bird.data <- read.csv("data/bird.data.csv")
bird.data$start_deployment <- dmy(bird.data$start_deployment, tz='UTC')
bird.data$year.start <- year(bird.data$start_deployment)
bird.data$end_deployment <- dmy(bird.data$end_deployment, tz='UTC')
bird.data$year.end <- year(bird.data$end_deployment)
birds = bird.data$birdID

breeding.data <- read.csv("data/breeding.data.csv")
breeding.data <- breeding.data[breeding.data$used==1,] # only select birdyears with suitable and reliable data

gps.breeding.data.behav <- read.csv("data/gps.breeding.data.behav.csv")
# end of reloading

gps.breeding.data.behav$sex <- as.factor(gps.breeding.data.behav$sex)
gps.breeding.data.behav$birdID <- as.factor(gps.breeding.data.behav$birdID)
gps.breeding.data.behav$year <- as.factor(gps.breeding.data.behav$year)
gps.breeding.data.behav$breeding.phase <- as.factor(gps.breeding.data.behav$breeding.phase)

# (re)calculate additional columns
gps.breeding.data.behav$birdyear = as.factor(paste(gps.breeding.data.behav$birdID, gps.breeding.data.behav$year, sep="_"))
gps.breeding.data.behav$yday_CEST = yday(gps.breeding.data.behav$date_time_CEST)
gps.breeding.data.behav$hour_CEST = hour(gps.breeding.data.behav$date_time_CEST)
gps.breeding.data.behav$hour_f <- as.factor(gps.breeding.data.behav$hour_CEST)
gps.breeding.data.behav$sexxbp = interaction(gps.breeding.data.behav$sex, gps.breeding.data.behav$breeding.phase)
gps.breeding.data.behav$breeding.phase.nr = as.numeric(substr(gps.breeding.data.behav$breeding.phase,1,1))

# other categorizations of habitat:
# categorize habitats as schier, mainland, wadden, or rest:
gps.breeding.data.behav$habitat_type = 'schier'
gps.breeding.data.behav$habitat_type[gps.breeding.data.behav$habitat=='LM_zoet'|gps.breeding.data.behav$habitat=='LM_land'|gps.breeding.data.behav$habitat=='wal_rest_zoet'|gps.breeding.data.behav$habitat=='wal_rest_land'] ='mainland'
gps.breeding.data.behav$habitat_type[gps.breeding.data.behav$habitat=='waddenzee'|gps.breeding.data.behav$habitat=='Wad_Kweldergeul_Brak'] ='wadden'
gps.breeding.data.behav$habitat_type[gps.breeding.data.behav$habitat=='Eilanden_Rest'|gps.breeding.data.behav$habitat=='Noordzee'] ='rest'
gps.breeding.data.behav$habitat_type[is.na(gps.breeding.data.behav$habitat)] = NA

# categorize habitats as marine, brackish, freshwater, land, or NA
gps.breeding.data.behav$habitat_salinity = NA
gps.breeding.data.behav$habitat_salinity[gps.breeding.data.behav$habitat=='Schier_Kwelder'|gps.breeding.data.behav$habitat=='wal_rest_land'|gps.breeding.data.behav$habitat=='LM_land'|gps.breeding.data.behav$habitat=='Schier_Land_Rest'|gps.breeding.data.behav$habitat=='Eilanden_Rest'] = 'land'
gps.breeding.data.behav$habitat_salinity[gps.breeding.data.behav$habitat=='waddenzee'|gps.breeding.data.behav$habitat=="Noordzee"] = 'marine'
gps.breeding.data.behav$habitat_salinity[gps.breeding.data.behav$habitat=='Schier_brak'|gps.breeding.data.behav$habitat=='Wad_Kweldergeul_Brak'] = 'brackish'
gps.breeding.data.behav$habitat_salinity[gps.breeding.data.behav$habitat=='LM_zoet'|gps.breeding.data.behav$habitat=='wal_rest_zoet'|gps.breeding.data.behav$habitat=='Schier_Zoet'] = 'freshwater'

# make column "behaviour2" that combines behaviour with habitat:
gps.breeding.data.behav$behaviour2 = gps.breeding.data.behav$behaviour
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour=='foraging'] = paste(gps.breeding.data.behav$behaviour[gps.breeding.data.behav$behaviour=='foraging'], gps.breeding.data.behav$habitat_salinity[gps.breeding.data.behav$behaviour=='foraging'], sep='_')
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour=='resting'] = paste(gps.breeding.data.behav$behaviour[gps.breeding.data.behav$behaviour=='resting'], gps.breeding.data.behav$habitat_type[gps.breeding.data.behav$behaviour=='resting'], sep='_')
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$distance.to.nest<5] = 'at_nest'

# turn foraging_land into "other"  
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour2=="foraging_land"] <- "other" 

# create second breeding phase column in which post-breeding successful and unsuccessful are pooled:
gps.breeding.data.behav$breeding.phase2 <- as.character(gps.breeding.data.behav$breeding.phase)
gps.breeding.data.behav$breeding.phase2[gps.breeding.data.behav$breeding.phase.nr==5|gps.breeding.data.behav$breeding.phase.nr==6] <- "5.post-breeding"

### add binary columns for whether bird is attending the nest, foraging or foraging in marine water: 
gps.breeding.data.behav$at_nest <- 0
gps.breeding.data.behav$at_nest[gps.breeding.data.behav$behaviour2=='at_nest'] <- 1
gps.breeding.data.behav$foraging <- 0
gps.breeding.data.behav$foraging[gps.breeding.data.behav$behaviour2=='foraging_freshwater'|gps.breeding.data.behav$behaviour2=='foraging_brackish'|gps.breeding.data.behav$behaviour2=='foraging_marine'] <- 1 # only foraging in water (not on land) is considered as foraging
gps.breeding.data.behav$foraging_marine <- 0
gps.breeding.data.behav$foraging_marine[gps.breeding.data.behav$behaviour2=='foraging_brackish'|gps.breeding.data.behav$behaviour2=='foraging_marine'] <- 1

# create df with only data during the incubation and chick-rearing phase:
gps.breeding.data.behav.phase34 <- gps.breeding.data.behav[gps.breeding.data.behav$breeding.phase%in%c('3.eggs','4.chicks'),]

# Statistical analysis

# (1) PROBABILITY OF ATTENDING THE NEST (only comparing egg incubation and chick rearing phase)

glmer.nestprob <- glmer(at_nest~breeding.phase*sex+(1|year)+(1|birdID), gps.breeding.data.behav.phase34, family='binomial', na.action='na.fail', nAGQ=0) # nACQ=0 makes convergence about 4 times faster. 

# Check model assumptions for glmer models
# check for over- or underdispersion of the data:
simOutput.glmer.nestprob <- simulateResiduals(glmer.nestprob, plot=F)
residuals(simOutput.glmer.nestprob)
windows()
plot(simOutput.glmer.nestprob)
plotQQunif(simOutput.glmer.nestprob)
plotResiduals(simOutput.glmer.nestprob)

# check for homogeneity of residuals in relation to explanatory variables:
modsel.nestprob <- dredge(glmer.nestprob)
Table1a <- make.table.from.dredge.output(modsel.nestprob)
Table1a
write.csv(Table1a, "output/Table1a - Proportion_at_nest.csv")
m.pars.nestprob = get.models(modsel.nestprob,1)[[1]] # with glmer, you apparently cannot choose between ML and REML
marginal.emmeans.nestprob <- emmeans(m.pars.nestprob, pairwise~breeding.phase*sex)
marginal.emmeans.nestprob.df <- as.data.frame(marginal.emmeans.nestprob$emmeans)
marginal.emmeans.nestprob.df  <- cbind(marginal.emmeans.nestprob.df[,c('sex','breeding.phase','df')], plogis(as.matrix(marginal.emmeans.nestprob.df[,c('emmean','asymp.LCL','asymp.UCL')])))
marginal.emmeans.nestprob.df
pairs(marginal.emmeans.nestprob)

# (2) PROBABILITY OF FORAGING

glmer.forprob <- glmer(foraging~breeding.phase*sex+(1|year)+(1|birdID), gps.breeding.data.behav, family='binomial', na.action='na.fail', nAGQ=0) 
summary(glmer.forprob)
modsel.forprob <- dredge(glmer.forprob)

Table1b <- make.table.from.dredge.output(modsel.forprob)
write.csv(Table1b, "output/Table1b - Proportion_foraging.csv")

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

forprob.sexcomp = pairs(marginal.means.forprob, by='breeding.phase')
forprob.sexcomp = as.data.frame(forprob.sexcomp)
forprob.sexcomp$sign = ifelse(forprob.sexcomp$p.value<0.001, "***",
                              ifelse(forprob.sexcomp$p.value<0.01, "**",
                                     ifelse(forprob.sexcomp$p.value<0.05, "*", "n.s.")))
write.table(paste(round(forprob.sexcomp$z.ratio, 2), forprob.sexcomp$sign, sep=""), 'clipboard', sep='\t')

# (3) PROPORTION OF TIME SPENT FORAGING IN MARINE (or brackish) HABITAT
gps.breeding.data.behav.marfor <- gps.breeding.data.behav[gps.breeding.data.behav$foraging==1,]
glmer.marforprob <- glmer(foraging_marine~breeding.phase*sex+(1|birdID)+(1|year), gps.breeding.data.behav.marfor, family='binomial', na.action='na.fail', nAGQ=0) 
modsel.marforprob <- dredge(glmer.marforprob)

Table1c <- make.table.from.dredge.output(modsel.marforprob)
write.csv(Table1c, "output/Table1c - Proportion_foraging_marine.csv")

marginal.emmeans.marforprob <- emmeans(glmer.marforprob, ~breeding.phase*sex)
marginal.emmeans.marforprob.df <- as.data.frame(marginal.emmeans.marforprob)
marginal.emmeans.marforprob.df  <- cbind(marginal.emmeans.marforprob.df[,c('sex','breeding.phase','df')], plogis(as.matrix(marginal.emmeans.marforprob.df[,c('emmean','asymp.LCL','asymp.UCL')])))
marginal.emmeans.marforprob.df
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

pairs(marginal.emmeans.marforprob, by='breeding.phase')
marforprob.sexcomp = pairs(marginal.emmeans.marforprob, by='breeding.phase')
marforprob.sexcomp = as.data.frame(marforprob.sexcomp)
marforprob.sexcomp$sign = ifelse(marforprob.sexcomp$p.value<0.001, "***",
                              ifelse(marforprob.sexcomp$p.value<0.01, "**",
                                   ifelse(marforprob.sexcomp$p.value<0.05, "*", "n.s.")))

write.table(paste(round(marforprob.sexcomp$z.ratio, 2), marforprob.sexcomp$sign, sep=""), 'clipboard', sep='\t')

# HABITAT USE IN RELATION TO WITHIN-SEX BILL AND TARSUS LENGTH
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
birds_prop_for_marine[order(birds_prop_for_marine$birdID, birds_prop_for_marine$breeding.phase),] 
birds_prop_for_marine <- aggregate(cbind(foraging,foraging_marine)~birdID+sex+bill, prop.for.marine.biometrics[is.na(prop.for.marine.biometrics$bill)==F,], sum)
birds_prop_for_marine$prop_for_marine <- birds_prop_for_marine$foraging_marine/birds_prop_for_marine$foraging

######################################################
## TIMING OF NEST ATTENDANCE AND FORAGING ANALYSIS ###
######################################################

### (4) TIMING OF NEST ATTENDANCE

# Compare all 32 possible models (with in all models, the main effectsexxbp (i.e. the interaction between sex and breeding phase) kept in):
# 4 models with one two-way interaction:
bam.nest.bxt <- bam(at_nest~sexxbp+s(tidal_stage_rad, bs='cc', by=breeding.phase)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #1 # K=10 by default
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

# 4-way model
bam.nest.bxsxdxt <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="ML") #32

model.names <- ls()[grep("bam.nest",ls())]
bam.nest.model.list <- mget(model.names)

# retrieve deviance and AIC for each model
K_aic_values <- t(sapply(bam.nest.model.list,AIC_deviance))
model_aic_df_nesting <- as.data.frame(model_aic_df_nesting)
model_aic_df_nesting <- model_aic_df_nesting[order(model_aic_df_nesting$AICc),] # the full model is the best. 
model_aic_df_nesting$Model <- substr(rownames(model_aic_df_nesting),10,40)
model_aic_df_nesting$dAICc = round(model_aic_df_nesting$AICc-min(model_aic_df_nesting$AICc),2)
write.csv(model_aic_df_nesting[,c('Model','K','dAICc')], "output/TableS2_modsel_nest_attendance_rhythm.csv")
# AIC difference between top and 2nd model:
model_aic_df_nesting$AICc[2]-model_aic_df_nesting$AICc[1]

### (5) TIMING OF FORAGING - also including the pre- and post-breeding phase 

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

# run best-supported models with REML estimation:
bam.nest.bxsxdxt.REML <- bam(at_nest~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav.phase34, family='binomial', method="REML")
bam.foraging.bxsxdxt.REML <- bam(foraging~sexxbp+s(diel_rad, bs='cc', by=sexxbp)+s(tidal_stage_rad, bs='cc', by=sexxbp)+te(diel_rad, tidal_stage_rad, bs=c('cc','cc'), by=sexxbp)+s(birdID, bs='re')+s(year, bs='re'), data=gps.breeding.data.behav, family='binomial', method="REML")