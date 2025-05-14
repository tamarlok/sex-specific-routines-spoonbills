cols.behaviour2 = c('chartreuse4','darkolivegreen3','khaki','gold','orange','red','lightblue','cadetblue','blue')
cols.behaviour2.pooled = c('chartreuse4','gold','gold','gold','orange','red','blue','blue','blue')
breeding.phase.cols = c('grey','white','darkorange','dodgerblue','darkolivegreen3','red') # colours for the different breeding.phases for plotting

# number of bird days:
birddays <- unique(gps.breeding.data.behav[,c('birdID','year','yday_CEST','breeding.phase','breeding.phase2')])
dim(birddays) # 3115 birddays with nearly complete data (47 or 48 samples per 24h)
length(table(gps.breeding.data.behav$birdyear)) # 41 birdyears
# sample size per bird year and breeding phase:
sample_size_birdyear_phase = as.data.frame(table(paste(birddays$birdID, birddays$year, sep="_"), birddays$breeding.phase2))
names(sample_size_birdyear_phase)=c('birdyear','breeding.phase2','ndays')

dur.behav.hr.xtabs = xtabs(freq~behaviour2+hour_CEST+sex+breeding.phase2, data=gps.breeding.data.behav)
rownames(dur.behav.hr.xtabs)
unique(paste(gps.breeding.data.behav$birdID, gps.breeding.data.behav$sex))
dur.behav.hr.m.pre = as.matrix(prop.table(dur.behav.hr.xtabs[,,'M','1.pre-breeding'], 2))
dur.behav.hr.f.pre = as.matrix(prop.table(dur.behav.hr.xtabs[,,'F','1.pre-breeding'], 2))
dur.behav.hr.m.eggs = as.matrix(prop.table(dur.behav.hr.xtabs[,,'M','3.eggs'], 2))
dur.behav.hr.f.eggs = as.matrix(prop.table(dur.behav.hr.xtabs[,,'F','3.eggs'], 2))
dur.behav.hr.m.chicks = as.matrix(prop.table(dur.behav.hr.xtabs[,,'M','4.chicks'], 2))
dur.behav.hr.f.chicks = as.matrix(prop.table(dur.behav.hr.xtabs[,,'F','4.chicks'], 2))
dur.behav.hr.m.post = as.matrix(prop.table(dur.behav.hr.xtabs[,,'M','5.post-breeding'], 2))
dur.behav.hr.f.post = as.matrix(prop.table(dur.behav.hr.xtabs[,,'F','5.post-breeding'], 2))

####################################
############## FIGURE 2 ############
####################################
postscript("output/Fig2.eps",width=10,height=8)
layout(matrix(1:8, byrow=T, ncol=2))
par(mar=c(1,1,1.2,0), oma=c(5,5,4,15))
barplot(dur.behav.hr.f.pre[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('FEMALE', 3, 0.6, cex=1)
barplot(dur.behav.hr.m.pre[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n', yaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('PRE-BREEDING', 4, -0.5, cex=0.9)
mtext('MALE', 3, 0.6, cex=1)
barplot(dur.behav.hr.f.eggs[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
barplot(dur.behav.hr.m.eggs[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n', yaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('INCUBATION', 4, -0.5, cex=0.9)
barplot(dur.behav.hr.f.chicks[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n', cex.axis=1.3, border=NA, space=c(0.1,0.1))
barplot(dur.behav.hr.m.chicks[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n', yaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('CHICK-REARING', 4, -0.5, cex=.9)
barplot(dur.behav.hr.f.post[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1 , cex.axis=1.3, cex=1, border=NA, space=c(0.1,0.1))
barplot(dur.behav.hr.m.post[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1 ,yaxt='n' , cex.axis=1.3, cex=1, border=NA, space=c(0.1,0.1))
mtext('POST-BREEDING', 4, -0.5, cex=.9)
mtext('Time of day in hours (local time)', 1, 3, cex=1.2, outer=T)
mtext('Time allocation', 2, 3, cex=1.2, outer=T)
legend(29, 1.3, c('foraging marine', 'foraging brackish', 'foraging fresh', 'flying', 'other', 'resting tidal flats', 'resting mainland', 'resting island', 'nest attendance'), pch=15, col=rev(cols.behaviour2), bty='n', xpd=NA, cex=1.5)
dev.off()

# Preparation for Figure 5 #
# calculate duration of attending the nest and (marine) foraging per bird per breeding phase
duration_behaviour_bird_phase_year = aggregate(cbind(1,at_nest,foraging,foraging_marine)~year+birdID+sex+breeding.phase, gps.breeding.data.behav, sum)
names(duration_behaviour_bird_phase_year)[5:8] = c("dur_tot","dur_at_nest","dur_foraging","dur_foraging_marine")
duration_behaviour_bird_phase_year = duration_behaviour_bird_phase_year[order(duration_behaviour_bird_phase_year$year, duration_behaviour_bird_phase_year$birdID),]
duration_behaviour_bird_phase_year$prop_nest <- duration_behaviour_bird_phase_year$dur_at_nest/duration_behaviour_bird_phase_year$dur_tot
duration_behaviour_bird_phase_year$prop_for <- duration_behaviour_bird_phase_year$dur_foraging/duration_behaviour_bird_phase_year$dur_tot
duration_behaviour_bird_phase_year$prop_for_marine <- duration_behaviour_bird_phase_year$dur_foraging_marine/duration_behaviour_bird_phase_year$dur_foraging

############## FIGURE 5 ###############
postscript("output/Fig5.eps",width=10,height=8)
# windows(10,8)
layout(1:2)
par(mar=c(1,1,0,0), oma=c(4,4,3,7))
# PANEL A: proportion of time spent foraging ~ sex + breeding phase
ymin=0.1
ymax=0.6
boxplot(prop_for~breeding.phase, data=duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$sex=="F",], xaxt='n', yaxt='n', col="lightcoral", medcol="coral4", whiskcol="coral4", outcol="coral4", staplecol="coral4", boxcol="coral4", xlab='', ylab='', boxwex=0.25, ylim=c(ymin,ymax), xlim=c(0.8,5.5))
boxplot(prop_for~breeding.phase, data=duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", medcol="lightskyblue4", whiskcol="lightskyblue4", outcol="lightskyblue4", staplecol="lightskyblue4", boxcol="lightskyblue4", ylab='', at=1:5+0.3, boxwex=0.25, add=T)
axis(2, at=seq(0.1,0.5,0.1), las=1)
plot.sign.prop.for(ymin, ymax, yrel=0.98, signs=forprob.sexcomp$sign, dist.text=0.03, fontsize=1) # plot significance of pairwise comparisons between sexes
text(.85,ymin+(ymax-ymin)*0.03,"(a)")
text(1:5, ymin+(ymax-ymin)*0.9, c("b","a","c","d","d"), col="lightcoral", adj=c(0.5,0))
text(1:5+0.3, ymin+(ymax-ymin)*0.9, c("a","a","c","d","b"), col="lightskyblue", adj=c(0.5,0))
mtext("Foraging probability",2,3,cex=1.2)
# show sample sizes! 
N.prop.for.phase.sex <- table(duration_behaviour_bird_phase_year$breeding.phase, duration_behaviour_bird_phase_year$sex)
axis(3, at=1:5, N.prop.for.phase.sex[,1], tick=F, line=-0.5, cex.axis=1.1)
axis(3, at=1:5+0.3, N.prop.for.phase.sex[,2], tick=F, line=-0.5, cex.axis=1.1)
axis(3, at=0.7, "N =", tick=F, line=-0.5, cex.axis=1.1)
# PANEL B: proportion of foraging time in marine waters ~ sex + breeding phase
ymin=0
ymax=1.2
boxplot(prop_for_marine~breeding.phase, data=duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$sex=="F",], xaxt='n', yaxt='n', col="lightcoral", medcol="coral4", whiskcol="coral4", outcol="coral4", staplecol="coral4", boxcol="coral4", ylab='', boxwex=0.25, ylim=c(ymin,ymax), xlim=c(0.8,5.5), las=1, cex.axis=1, xlab="")
boxplot(prop_for_marine~breeding.phase, data=duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", medcol="lightskyblue4", whiskcol="lightskyblue4", outcol="lightskyblue4", staplecol="lightskyblue4", boxcol="lightskyblue4", ylab='', at=1:5+0.3, boxwex=0.25, add=T)
plot.sign.prop.for(ymin, ymax, yrel=0.98, signs=marforprob.sexcomp$sign, dist.text=0.03, fontsize=1) # plot significance of pairwise comparisons between sexes
text(.85,ymin+(ymax-ymin)*0.03,"(b)")
text(1:5, ymin+(ymax-ymin)*0.9, c("ab","b","b","a","b"), col="lightcoral", adj=c(0.5,0))
text(1:5+0.3, ymin+(ymax-ymin)*0.9, c("c","b","c","b","a"), col="lightskyblue", adj=c(0.5,0))
axis(1, at=1:5+0.15, labels=FALSE)
axis(1, at=1:5+0.15, labels=c('pre-','incubation', 'chick-','post-', 'post-'), cex.axis=1, line=-0.5, tick=F)
axis(1, at=1:5+0.15, labels=c('breeding','','rearing','breeding +', 'breeding -'), cex.axis=1, line=0.3, tick=F)
axis(2, at=seq(0,1,0.2), las=1)
mtext("Marine foraging probability",2,3,cex=1.2)
mtext("Breeding phase",1,3,cex=1.2)
legend(5.8,0.3, legend=c("female","male"), pch=22, pt.bg = c("lightcoral","lightskyblue"), col=c("coral4","lightskyblue4"), cex=1, xpd=NA, bty='n')
dev.off()
### END FIGURE 3 ###

# data preparation
gps.breeding.data.behav$freq <- 1
# calculate proportion of nest attendance and foraging per bird, hour, tidal phase and breeding phase (what about year?)
# redefine tidal phase from the circular variable tidal_stage_rad, where low, incoming, high and outgoing tide are defined as four tidal stages where:
# low tide: >0.75pi and <=1.25pi
# incoming: >1.25pi and <=1.75pi
# high: >1.75pi or <=0.25pi
# outgoing: >0.25pi and <=0.75pi
gps.breeding.data.behav$tidal_phase_cat <- 0
gps.breeding.data.behav$tidal_phase_cat[gps.breeding.data.behav$tidal_stage_rad>0.75*pi & gps.breeding.data.behav$tidal_stage_rad<=1.25*pi] <- pi
gps.breeding.data.behav$tidal_phase_cat[gps.breeding.data.behav$tidal_stage_rad>1.25*pi & gps.breeding.data.behav$tidal_stage_rad<=1.75*pi] <- 1.5*pi
gps.breeding.data.behav$tidal_phase_cat[gps.breeding.data.behav$tidal_stage_rad>0.25*pi & gps.breeding.data.behav$tidal_stage_rad<=0.75*pi] <- 0.5*pi
# check whether this went more or less right:
table(gps.breeding.data.behav$tidal_phase_cat, gps.breeding.data.behav$tidal_phase) # this indeed goes correctly

duration.behaviour.per.bird.hour.tidal.breeding.phase = aggregate(cbind(freq,at_nest,foraging)~birdID+sex+hour_CEST+hour_f+tidal_phase_cat+breeding.phase, gps.breeding.data.behav, sum)
names(duration.behaviour.per.bird.hour.tidal.breeding.phase)[7:9] <- c("dur_tot","dur_at_nest","dur_for") 
duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_at_nest <- duration.behaviour.per.bird.hour.tidal.breeding.phase$dur_at_nest/duration.behaviour.per.bird.hour.tidal.breeding.phase$dur_tot
duration.behaviour.per.bird.hour.tidal.breeding.phase$prop_for <- duration.behaviour.per.bird.hour.tidal.breeding.phase$dur_for/duration.behaviour.per.bird.hour.tidal.breeding.phase$dur_tot
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

# calculate means and 95% CI on the basis of the raw (averaged) data (assuming a normal distribution, see: https://www.statisticshowto.com/probability-and-statistics/confidence-interval/#CISample ) :
mean.prop.at.nest.sex.hour.tidal.phase <- aggregate(logit_at_nest~sex+breeding.phase+tidal_phase_cat+hour_CEST, duration.behaviour.per.bird.hour.tidal.breeding.phase, mean)
sd.prop.at.nest.sex.hour.tidal.phase <- aggregate(logit_at_nest~sex+breeding.phase+tidal_phase_cat+hour_CEST, duration.behaviour.per.bird.hour.tidal.breeding.phase, sd)
duration.behaviour.per.bird.hour.tidal.breeding.phase$freq=1
N.prop.at.nest.sex.hour.tidal.phase <- aggregate(freq~sex+breeding.phase+tidal_phase_cat+hour_CEST, duration.behaviour.per.bird.hour.tidal.breeding.phase, sum)
sd.prop.at.nest.sex.hour.tidal.phase$se <- sd.prop.at.nest.sex.hour.tidal.phase$logit_at_nest/sqrt(N.prop.at.nest.sex.hour.tidal.phase$freq)
mean.prop.at.nest.sex.hour.tidal.phase$lower.CL <- mean.prop.at.nest.sex.hour.tidal.phase$logit_at_nest - 1.96 * sd.prop.at.nest.sex.hour.tidal.phase$se
mean.prop.at.nest.sex.hour.tidal.phase$upper.CL <- mean.prop.at.nest.sex.hour.tidal.phase$logit_at_nest + 1.96 * sd.prop.at.nest.sex.hour.tidal.phase$se
# translate to proportion instead of logit
mean.prop.at.nest.sex.hour.tidal.phase$prop.at.nest <- plogis(mean.prop.at.nest.sex.hour.tidal.phase$logit_at_nest)
mean.prop.at.nest.sex.hour.tidal.phase$lower.CL <- plogis(mean.prop.at.nest.sex.hour.tidal.phase$lower.CL)
mean.prop.at.nest.sex.hour.tidal.phase$upper.CL <- plogis(mean.prop.at.nest.sex.hour.tidal.phase$upper.CL)

# calculate mean periods of night, dusk, dawn and day for the different breeding periods:
# add sunset, sunrise, dusk and dawn (which is location-specific, but varies very little between locations as they are very close):
coordinates(gps.breeding.data.behav)  <- ~longitude + latitude  # to make it a spatialpoints dataframe
gps.breeding.data.behav$sunrise=sunriset(coordinates(gps.breeding.data.behav), gps.breeding.data.behav$date_time_CEST, POSIXct.out=TRUE, direction="sunrise")[,2]
gps.breeding.data.behav$sunset=sunriset(coordinates(gps.breeding.data.behav), gps.breeding.data.behav$date_time_CEST, POSIXct.out=TRUE, direction="sunset")[,2]
gps.breeding.data.behav$dusk=crepuscule(coordinates(gps.breeding.data.behav), gps.breeding.data.behav$date_time_CEST, solarDep = 6,POSIXct.out=TRUE, direction="dusk")[,2]
gps.breeding.data.behav$dawn=crepuscule(coordinates(gps.breeding.data.behav), gps.breeding.data.behav$date_time_CEST, solarDep = 6, POSIXct.out=TRUE, direction="dawn")[,2]
# turn back into data frame:
gps.breeding.data.behav <- as.data.frame(gps.breeding.data.behav)
# calculate average time of sunset, sunrise, dusk and dawn per breeding phase:
sunset_pre <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$sunset[gps.breeding.data.behav$breeding.phase=='1.pre-breeding'])))
sunrise_pre <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$sunrise[gps.breeding.data.behav$breeding.phase=='1.pre-breeding'])))
dawn_pre <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$dawn[gps.breeding.data.behav$breeding.phase=='1.pre-breeding'])))
dusk_pre <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$dusk[gps.breeding.data.behav$breeding.phase=='1.pre-breeding'])))
sunset_eggs <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$sunset[gps.breeding.data.behav$breeding.phase=='3.eggs'])))
sunrise_eggs <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$sunrise[gps.breeding.data.behav$breeding.phase=='3.eggs'])))
dawn_eggs <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$dawn[gps.breeding.data.behav$breeding.phase=='3.eggs'])))
dusk_eggs <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$dusk[gps.breeding.data.behav$breeding.phase=='3.eggs'])))
sunset_chicks <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$sunset[gps.breeding.data.behav$breeding.phase=='4.chicks'])))
sunrise_chicks <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$sunrise[gps.breeding.data.behav$breeding.phase=='4.chicks'])))
dawn_chicks <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$dawn[gps.breeding.data.behav$breeding.phase=='4.chicks'])))
dusk_chicks <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$dusk[gps.breeding.data.behav$breeding.phase=='4.chicks'])))
sunset_post.breeding <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$sunset[gps.breeding.data.behav$breeding.phase%in%c('5.post.breeding.successful','6.post.breeding.unsuccessful')])))
sunrise_post.breeding <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$sunrise[gps.breeding.data.behav$breeding.phase%in%c('5.post.breeding.successful','6.post.breeding.unsuccessful')])))
dawn_post.breeding <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$dawn[gps.breeding.data.behav$breeding.phase%in%c('5.post.breeding.successful','6.post.breeding.unsuccessful')])))
dusk_post.breeding <- seconds_to_period(mean(hms::as_hms(gps.breeding.data.behav$dusk[gps.breeding.data.behav$breeding.phase%in%c('5.post.breeding.successful','6.post.breeding.unsuccessful')])))

# tide, breeding phase and hour labels:
tide.table = data.frame(tidal_stage_rad=c(0,0.5*pi,pi,1.5*pi), label=c('high tide','outgoing','low tide','incoming'))
breeding.phase.table = data.frame(breeding.phase=c('1.pre-breeding','3.eggs','4.chicks','5.post.breeding.successful','6.post.breeding.unsuccessful'), label=c('pre-breeding','inucbation','chick-rearing','post-breeding +','post-breeding -'), label.short = c('pre-breeding','inucbation','chick-rearing','post +','post -'))

### Now make a figure with adding the estimated smoothing curves from the bam model, plotting on 16 separate panels for readability:
mean.prop.at.nest.sex.hour.tidal.phase$diel_rad = mean.prop.at.nest.sex.hour.tidal.phase$hour_CEST/24*2*pi
x.labels = unique(mean.prop.at.nest.sex.hour.tidal.phase[,c('hour_CEST','diel_rad')])

# predicted values for bam-model whilst ignoring random effects:
#df.diel.tide.sex.phase <- expand.grid(diel_rad=seq(0,2*pi,0.01*pi), tidal_stage_rad=c(0.5*pi,pi,1.5*pi), sex=factor(c("F","M")), breeding.phase=factor(c("1.pre-breeding","3.eggs","4.chicks","5.post.breeding.successful","6.post.breeding.unsuccessful")), birdID=factor(6284), year=2016)
df.diel.tide.sex.phase <- expand.grid(diel_rad=seq(0,2*pi,0.01*pi), tidal_stage_rad=seq(0,1.99*pi,0.01*pi), sex=factor(c("F","M")), breeding.phase=factor(c("1.pre-breeding","3.eggs","4.chicks","5.post.breeding.successful","6.post.breeding.unsuccessful")), birdID=factor(6284), year=2016)
df.diel.tide.sex.phase$sexxbp <- interaction(df.diel.tide.sex.phase$sex, df.diel.tide.sex.phase$breeding.phase)
df.diel.tide.sex.phase34 <- df.diel.tide.sex.phase[df.diel.tide.sex.phase$breeding.phase%in%c("3.eggs","4.chicks"),]
df.diel.tide.sex.phase34$sexxbp <- as.factor(as.character(df.diel.tide.sex.phase34$sexxbp))
pd.bam = predict(bam.nest.bxsxdxt.REML, newdata=df.diel.tide.sex.phase34, type='response', se.fit=T, exclude=c('s(birdID)','s(year)')) # I excluded the random effect terms, causing them to be set to 0 during predicting 
df.diel.tide.sex.phase34$fit <- pd.bam$fit
df.diel.tide.sex.phase34$se.fit <- pd.bam$se.fit

# predict using link function, then calculate 95% CI and then translate to real (response) scale:
pd.bam.link = predict(bam.nest.bxsxdxt.REML, newdata=df.diel.tide.sex.phase34, type='link', se.fit=T, exclude=c('s(birdID)','s(year)'))
pd.bam.link$li = pd.bam.link$fit-1.96*pd.bam.link$se.fit
pd.bam.link$ui = pd.bam.link$fit+1.96*pd.bam.link$se.fit
pd.bam.link$fit.real = plogis(pd.bam.link$fit)
pd.bam.link$li.real = plogis(pd.bam.link$li)
pd.bam.link$ui.real = plogis(pd.bam.link$ui)
df.diel.tide.sex.phase34$fit.real <- pd.bam.link$fit.real
df.diel.tide.sex.phase34$li.real <- pd.bam.link$li.real
df.diel.tide.sex.phase34$ui.real <- pd.bam.link$ui.real

windows()
plotnr=0
layout(matrix(1:16, nrow=4, byrow=T))
par(mar=c(1,1,0,0), oma=c(3,3,1,1))
for (tide in c(0,0.5*pi,pi,1.5*pi)) {
  for (breeding.phase in c('3.eggs','4.chicks')) {
    for (sex in c("F","M")) {
      plotnr=plotnr+1
      df.predict = df.diel.tide.sex.phase34[round(df.diel.tide.sex.phase34$tidal_stage_rad,2)==round(tide,2) & 
                        df.diel.tide.sex.phase34$breeding.phase==breeding.phase & df.diel.tide.sex.phase34$sex==sex,]
      df.data = mean.prop.at.nest.sex.hour.tidal.phase[mean.prop.at.nest.sex.hour.tidal.phase$tidal_phase_cat==tide & 
                        mean.prop.at.nest.sex.hour.tidal.phase$breeding.phase==breeding.phase & mean.prop.at.nest.sex.hour.tidal.phase$sex==sex,]
      plot(fit~diel_rad, df.predict, xlim=c(0,2*pi), ylim=c(0,1), xaxt='n', yaxt='n', type='n', yaxs='i')
      polygon(c(df.predict$diel_rad, rev(df.predict$diel_rad)), c(df.predict$ui.real, rev(df.predict$li.real)), col='grey', border=NA)
      lines(fit~diel_rad, df.predict, lwd=2)
      plotCI(df.data$diel_rad, df.data$prop.at.nest, li=df.data$lower.CL, ui=df.data$upper.CL, sfrac=0, gap=0, pch=21, pt.bg='white', add=T)
      if (plotnr>12) axis(1, at=x.labels$diel_rad[c(seq(1,23,5))], labels=x.labels$hour_CEST[c(seq(1,23,5))])
    }
  }  
}

plot.dark.hours.rad.phase <- function(sunrise,sunset,dusk,dawn){
  polygon(x=c(-0.05,-0.05,rep(as.numeric(dawn)/86400*2*pi,2)), y=c(0,1,1,0), col='grey40', border=NA)
  polygon(x=c(rep(as.numeric(dawn)/86400*2*pi,2),rep(as.numeric(sunrise)/86400*2*pi,2)), y=c(0,1,1,0), col='grey80', border=NA)
  polygon(x=c(rep(as.numeric(sunset)/86400*2*pi,2),rep(as.numeric(dusk)/86400*2*pi,2)), y=c(0,1,1,0), col='grey80', border=NA)
  polygon(x=c(rep(as.numeric(dusk)/86400*2*pi,2),24,24), y=c(0,1,1,0), col='grey40', border=NA)
}


### START FIGURE 3 ###
cairo_ps("output/Fig3.eps",width=10,height=6)
#windows(12,8)
plotnr=0
layout(matrix(1:4, nrow=2, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4,3,10))
#for (tide in c(0,0.5*pi,pi,1.5*pi)) {
for (breeding.phase in c('3.eggs','4.chicks')) {
  for (tide in c(pi,0)) {
    top.panel.text = toupper(tide.table$label[tide.table$tidal_stage_rad==tide])
    yaxis.text = toupper(breeding.phase.table$label[breeding.phase.table$breeding.phase==breeding.phase])
    plotnr=plotnr+1
    df.predict = df.diel.tide.sex.phase34[round(df.diel.tide.sex.phase34$tidal_stage_rad,2)==round(tide,2) & 
                                              df.diel.tide.sex.phase34$breeding.phase==breeding.phase,]
    df.predict.F <- df.predict[df.predict$sex=="F",]
    df.predict.M <- df.predict[df.predict$sex=="M",]
    df.data = mean.prop.at.nest.sex.hour.tidal.phase[mean.prop.at.nest.sex.hour.tidal.phase$tidal_phase_cat==tide & 
                                                         mean.prop.at.nest.sex.hour.tidal.phase$breeding.phase==breeding.phase,]
    df.data.F <- df.data[df.data$sex=="F",]
    df.data.M <- df.data[df.data$sex=="M",]
    plot(fit~diel_rad, df.predict, xlim=c(-0.05,2*pi), ylim=c(0,1), xaxt='n', yaxt='n', type='n', xaxs='i', yaxs='i')
    if (breeding.phase=='3.eggs') plot.dark.hours.rad.phase(sunrise_eggs, sunset_eggs, dusk_eggs, dawn_eggs)
    if (breeding.phase=='4.chicks') plot.dark.hours.rad.phase(sunrise_chicks, sunset_chicks, dusk_chicks, dawn_chicks)
    polygon(c(df.predict.M$diel_rad, rev(df.predict.M$diel_rad)), c(df.predict.M$ui.real, rev(df.predict.M$li.real)), col=adjustcolor('lightskyblue',alpha=0.7), border=NA) #lightcoral
    polygon(c(df.predict.F$diel_rad, rev(df.predict.F$diel_rad)), c(df.predict.F$ui.real, rev(df.predict.F$li.real)), col=adjustcolor('lightcoral',0.7), border=NA) #lightcoral
    lines(fit~diel_rad, df.predict.M, col=adjustcolor('lightskyblue4',0.7), lwd=2)
    lines(fit~diel_rad, df.predict.F, col=adjustcolor('coral4',0.7), lwd=2)
    plotCI(df.data.M$diel_rad-0.01, df.data.M$prop.at.nest, li=df.data.M$lower.CL, ui=df.data.M$upper.CL, sfrac=0, gap=0, pch=21, pt.bg='white', col='lightskyblue4', add=T)
    plotCI(df.data.F$diel_rad+0.01, df.data.F$prop.at.nest, li=df.data.F$lower.CL, ui=df.data.F$upper.CL, sfrac=0, gap=0, pch=21, pt.bg='white', col='coral4', add=T)
    points(df.data.M$diel_rad-0.01, df.data.M$prop.at.nest, pch=21, bg='white', col='lightskyblue4')
    if (plotnr>2) axis(1, at=x.labels$diel_rad[c(seq(1,23,5))], labels=x.labels$hour_CEST[c(seq(1,23,5))])
    if (plotnr%in%c(1,3)) axis(2, at=seq(0,1,0.2), las=1)
    if (plotnr%in%c(2,4)) mtext(yaxis.text, 4, line=1)
    if (plotnr%in%1:2) mtext(top.panel.text, 3, line=1)
    box()
  }  
}
mtext("Time of the day (hour in CEST)",1,outer=T,line=2)
mtext("Probability of nest attendance",2,outer=T,line=2.5)
legend(2.3*pi,0.25,c("female","male"), pch=15, col=c("lightcoral","lightskyblue"), xpd=NA, cex=1.2)
dev.off()
### END FIGURE 3 ###

### FIGURE S2: nest attendance in relation to the tide: 
hours_to_plot <- seq(0,21,3)
n_hours <- length(hours_to_plot)
pdf("output/FigS2_nest_tide.pdf",10,4)
#windows(10,4)
plotnr=0
layout(matrix(1:(2*n_hours), nrow=2, byrow=T))
par(mar=c(1,1,0,1), oma=c(4,3,3,12))
for (breeding.phase in c('3.eggs','4.chicks')) {
  for (hour in hours_to_plot) {
    hour_rad <- hour/24*2*pi
    top.panel.text = paste(hour,":00",sep="")
    yaxis.text = toupper(breeding.phase.table$label[breeding.phase.table$breeding.phase==breeding.phase])
    plotnr=plotnr+1
    df.predict = df.diel.tide.sex.phase34[round(df.diel.tide.sex.phase34$diel_rad,2)==round(hour_rad,2) & 
                                            df.diel.tide.sex.phase34$breeding.phase==breeding.phase,]
    df.predict.F <- df.predict[df.predict$sex=="F",]
    df.predict.M <- df.predict[df.predict$sex=="M",]
    plot(fit.real~tidal_stage_rad, df.predict, xlim=c(-0.05,2*pi), ylim=c(0,1), xaxt='n', yaxt='n', type='n', xaxs='i', yaxs='i')
    polygon(c(df.predict.M$tidal_stage_rad, rev(df.predict.M$tidal_stage_rad)), c(df.predict.M$ui.real, rev(df.predict.M$li.real)), col=adjustcolor('lightskyblue',alpha=0.7), border=NA) #lightcoral
    polygon(c(df.predict.F$tidal_stage_rad, rev(df.predict.F$tidal_stage_rad)), c(df.predict.F$ui.real, rev(df.predict.F$li.real)), col=adjustcolor('lightcoral',0.7), border=NA) #lightcoral
    lines(fit.real~tidal_stage_rad, df.predict.M, col=adjustcolor('lightskyblue4',0.7), lwd=2)
    lines(fit.real~tidal_stage_rad, df.predict.F, col=adjustcolor('coral4',0.7), lwd=2)
    if (plotnr>n_hours) axis(1, at=c(0,pi,2*pi), labels=c('high','low','high'))
    if (plotnr%in%seq(1,2*n_hours,n_hours)) axis(2, at=c(0,1), las=1, cex.axis=1.3)
    if (plotnr%in%seq(n_hours,2*n_hours,n_hours)) mtext(yaxis.text, 4, line=1)
    if (plotnr%in%1:n_hours) mtext(top.panel.text, 3, line=1)
    box()
  }  
}
mtext("Tidal phase",1,outer=T,line=2, cex.lab=1.3)
mtext("Probability of nest attendance",2,outer=T,line=1.5, cex.lab=1.3)
legend(3*pi,0.4,c("female","male"), pch=15, col=c("lightcoral","lightskyblue"), xpd=NA, cex=1.5)
dev.off()
### END FIGURE S2 ###

### FIGURE 4 - TIMING OF FORAGING ###
mean.prop.foraging.sex.hour.tidal.phase <- aggregate(logit_foraging~sex+breeding.phase+tidal_phase_cat+hour_CEST, duration.behaviour.per.bird.hour.tidal.breeding.phase, mean)
sd.prop.foraging.sex.hour.tidal.phase <- aggregate(logit_foraging~sex+breeding.phase+tidal_phase_cat+hour_CEST, duration.behaviour.per.bird.hour.tidal.breeding.phase, sd)
N.prop.foraging.sex.hour.tidal.phase <- aggregate(freq~sex+breeding.phase+tidal_phase_cat+hour_CEST, duration.behaviour.per.bird.hour.tidal.breeding.phase, sum)
sd.prop.foraging.sex.hour.tidal.phase$se <- sd.prop.foraging.sex.hour.tidal.phase$logit_foraging/sqrt(N.prop.foraging.sex.hour.tidal.phase$freq)
mean.prop.foraging.sex.hour.tidal.phase$lower.CL <- mean.prop.foraging.sex.hour.tidal.phase$logit_foraging - 1.96 * sd.prop.foraging.sex.hour.tidal.phase$se
mean.prop.foraging.sex.hour.tidal.phase$upper.CL <- mean.prop.foraging.sex.hour.tidal.phase$logit_foraging + 1.96 * sd.prop.foraging.sex.hour.tidal.phase$se
# translate to proportion instead of logit
mean.prop.foraging.sex.hour.tidal.phase$prop.foraging <- plogis(mean.prop.foraging.sex.hour.tidal.phase$logit_foraging)
mean.prop.foraging.sex.hour.tidal.phase$lower.CL <- plogis(mean.prop.foraging.sex.hour.tidal.phase$lower.CL)
mean.prop.foraging.sex.hour.tidal.phase$upper.CL <- plogis(mean.prop.foraging.sex.hour.tidal.phase$upper.CL)
mean.prop.foraging.sex.hour.tidal.phase$diel_rad = mean.prop.foraging.sex.hour.tidal.phase$hour_CEST/24*2*pi

# predict using link function, then calculate 95% CI and then translate to real (response) scale:
pd.bam.link = predict(bam.foraging.bxsxdxt.REML, newdata=df.diel.tide.sex.phase, type='link', se.fit=T, exclude=c('s(birdID)','s(year)'))
pd.bam.link$li = pd.bam.link$fit-1.96*pd.bam.link$se.fit
pd.bam.link$ui = pd.bam.link$fit+1.96*pd.bam.link$se.fit
pd.bam.link$fit.real = plogis(pd.bam.link$fit)
pd.bam.link$li.real = plogis(pd.bam.link$li)
pd.bam.link$ui.real = plogis(pd.bam.link$ui)
df.diel.tide.sex.phase$fit.real <- pd.bam.link$fit.real
df.diel.tide.sex.phase$li.real <- pd.bam.link$li.real
df.diel.tide.sex.phase$ui.real <- pd.bam.link$ui.real

### START FIGURE 4 ###
cairo_ps("output/Fig4.eps",width=6,height=10)
#windows(8,10)
plotnr=0
layout(matrix(1:5, nrow=5, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4,3,12))
for (breeding.phase in c('1.pre-breeding','3.eggs','4.chicks','5.post.breeding.successful','6.post.breeding.unsuccessful')) {
    yaxis.text = toupper(breeding.phase.table$label[breeding.phase.table$breeding.phase==breeding.phase])
    plotnr=plotnr+1
    df.predict = df.diel.tide.sex.phase[round(df.diel.tide.sex.phase$tidal_stage_rad,2)==round(pi,2) & 
                                          df.diel.tide.sex.phase$breeding.phase==breeding.phase,]
    df.predict.F <- df.predict[df.predict$sex=="F",]
    df.predict.M <- df.predict[df.predict$sex=="M",]
    df.data = mean.prop.foraging.sex.hour.tidal.phase[mean.prop.foraging.sex.hour.tidal.phase$tidal_phase_cat==pi & 
                                                        mean.prop.foraging.sex.hour.tidal.phase$breeding.phase==breeding.phase,]
    df.data.F <- df.data[df.data$sex=="F",]
    df.data.M <- df.data[df.data$sex=="M",]
    plot(fit.real~diel_rad, df.predict, xlim=c(-0.05,2*pi), ylim=c(0,1), xaxt='n', yaxt='n', type='n', xaxs='i', yaxs='i')
    if (breeding.phase=='1.pre-breeding') plot.dark.hours.rad.phase(sunrise_pre, sunset_pre, dusk_pre, dawn_pre)
    if (breeding.phase=='3.eggs') plot.dark.hours.rad.phase(sunrise_eggs, sunset_eggs, dusk_eggs, dawn_eggs)
    if (breeding.phase=='4.chicks') plot.dark.hours.rad.phase(sunrise_chicks, sunset_chicks, dusk_chicks, dawn_chicks)
    if (breeding.phase%in%c('5.post.breeding.successful','6.post.breeding.unsuccessful')) plot.dark.hours.rad.phase(sunrise_post.breeding, sunset_post.breeding, dusk_post.breeding, dawn_post.breeding)
    polygon(c(df.predict.M$diel_rad, rev(df.predict.M$diel_rad)), c(df.predict.M$ui.real, rev(df.predict.M$li.real)), col=adjustcolor('lightskyblue',alpha=0.7), border=NA) #lightcoral
    polygon(c(df.predict.F$diel_rad, rev(df.predict.F$diel_rad)), c(df.predict.F$ui.real, rev(df.predict.F$li.real)), col=adjustcolor('lightcoral',0.7), border=NA) #lightcoral
    lines(fit.real~diel_rad, df.predict.M, col=adjustcolor('lightskyblue4',0.7), lwd=2)
    lines(fit.real~diel_rad, df.predict.F, col=adjustcolor('coral4',0.7), lwd=2)
    plotCI(df.data.M$diel_rad-0.01, df.data.M$prop.foraging, li=df.data.M$lower.CL, ui=df.data.M$upper.CL, sfrac=0, gap=0, pch=21, pt.bg='white', col='lightskyblue4', add=T)
    plotCI(df.data.F$diel_rad+0.01, df.data.F$prop.foraging, li=df.data.F$lower.CL, ui=df.data.F$upper.CL, sfrac=0, gap=0, pch=21, pt.bg='white', col='coral4', add=T)
    points(df.data.M$diel_rad-0.01, df.data.M$prop.foraging, pch=21, bg='white', col='lightskyblue4')
    if (plotnr>4) axis(1, at=x.labels$diel_rad[c(seq(1,23,5))], labels=x.labels$hour_CEST[c(seq(1,23,5))], cex.axis=1.3)
    axis(2, at=seq(0,1,0.2), las=1, cex.axis=1.3)
    mtext(yaxis.text, 4, line=1)
    box()
}
mtext("Time of the day (hour in CEST)",1,outer=T,line=2, cex.lab=1.3)
mtext("Probability of foraging during low tide",2,outer=T,line=2.5, cex.lab=1.3)
legend(2.3*pi,0.25,c("female","male"), pch=15, col=c("lightcoral","lightskyblue"), xpd=NA, cex=1.5)
dev.off()
### END FIGURE 4 ###

### FIGURE S3: foraging probability in relation to the diel and tidal phase, per breeding phase
pdf("output/FigS3_foraging_vs_tide.pdf",10,6)
plotnr=0
layout(matrix(1:40, nrow=5, byrow=T))
par(mar=c(1,1,0,1), oma=c(4,3,3,12))
for (breeding.phase in c('1.pre-breeding','3.eggs','4.chicks','5.post.breeding.successful','6.post.breeding.unsuccessful')) {
  for (hour in c(0,3,6,9,12,15,18,21)) {
    hour_rad <- hour/24*2*pi
    top.panel.text = paste(hour,":00",sep="")
    yaxis.text = toupper(breeding.phase.table$label.short[breeding.phase.table$breeding.phase==breeding.phase])
    plotnr=plotnr+1
    df.predict = df.diel.tide.sex.phase[round(df.diel.tide.sex.phase$diel_rad,2)==round(hour_rad,2) & 
                                          df.diel.tide.sex.phase$breeding.phase==breeding.phase,]
    df.predict.F <- df.predict[df.predict$sex=="F",]
    df.predict.M <- df.predict[df.predict$sex=="M",]
    plot(fit.real~tidal_stage_rad, df.predict, xlim=c(-0.05,2*pi), ylim=c(0,1), xaxt='n', yaxt='n', type='n', xaxs='i', yaxs='i')
    polygon(c(df.predict.M$tidal_stage_rad, rev(df.predict.M$tidal_stage_rad)), c(df.predict.M$ui.real, rev(df.predict.M$li.real)), col=adjustcolor('lightskyblue',alpha=0.7), border=NA) #lightcoral
    polygon(c(df.predict.F$tidal_stage_rad, rev(df.predict.F$tidal_stage_rad)), c(df.predict.F$ui.real, rev(df.predict.F$li.real)), col=adjustcolor('lightcoral',0.7), border=NA) #lightcoral
    lines(fit.real~tidal_stage_rad, df.predict.M, col=adjustcolor('lightskyblue4',0.7), lwd=2)
    lines(fit.real~tidal_stage_rad, df.predict.F, col=adjustcolor('coral4',0.7), lwd=2)
    if (plotnr>8*4) axis(1, at=c(0,pi,2*pi), labels=c('high','low','high'))
    if (plotnr%in%seq(1,40,8)) axis(2, at=c(0,1), las=1, cex.axis=1.3)
    if (plotnr%in%seq(8,40,8)) mtext(yaxis.text, 4, line=0.5, cex=0.7)
    if (plotnr%in%1:8) mtext(top.panel.text, 3, line=0.5)
    box()
  }  
}
mtext("Tidal phase",1,outer=T,line=2, cex.lab=1.3)
mtext("Probability of foraging",2,outer=T,line=1.5, cex.lab=1.3)
legend(3*pi,0.6,c("female","male"), pch=15, col=c("lightcoral","lightskyblue"), xpd=NA, cex=1.5)
dev.off()
### END FIGURE S3 ###

# PREPARATION FOR FIGURE 7
bills.males <- na.omit(birds_prop_for_marine$bill[birds_prop_for_marine$sex=='M'])
pred.males <- expand.grid(bill=seq(min(bills.males), max(bills.males),1))
pred.males$pred.mar.for.bill = predict(glmer.marforprob.males.bill, newdata=pred.males, type = "response", re.form = NA)

### make the same graph, but now also the females plotted in:
birds_prop_for_marine$sexn <- 1
birds_prop_for_marine$sexn[birds_prop_for_marine$sex=='F'] <- 2

########### FIGURE 7 #############
postscript("output/Fig7.eps", width=8, height=5)
#windows(8,5)
plot(prop_for_marine~bill, birds_prop_for_marine, pch=21, bg=c('lightskyblue','lightcoral')[sexn], col=c('lightskyblue4','coral4')[sexn],  cex=2, xlab='Bill length (mm)', ylab='Marine foraging probability', cex.axis=1.2, cex.lab=1.3)
legend('bottomleft',legend=c('female','male'), pch=21, pt.bg=c('lightcoral','lightskyblue'), col=c('coral4','lightskyblue4'), cex=1.5, pt.cex=2)
lines(pred.mar.for.bill~bill, pred.males, col='lightskyblue', lwd=2)
dev.off()
####### END OF FIGURE 7 ##########