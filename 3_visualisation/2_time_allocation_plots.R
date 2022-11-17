cols.behaviour2 = c('chartreuse4','darkolivegreen3','khaki','gold','orange','red','lightblue','cadetblue','blue')
cols.behaviour2.pooled = c('chartreuse4','gold','gold','gold','orange','red','blue','blue','blue')
breeding.phase.cols = c('grey','white','darkorange','dodgerblue','darkolivegreen3','red') # colours for the different breeding.phases for plotting

# explore the time allocation graphs per bird, to see if there is suspicious data:
gps.breeding.data.behav$birdyear <- paste(gps.breeding.data.behav$birdID, gps.breeding.data.behav$year, sep="_")
dur.behav.hr.birdyear.xtabs = xtabs(duration~behaviour2+hour_CEST+breeding.phase2+birdyear, data=gps.breeding.data.behav)
for (birdyear in dimnames(dur.behav.hr.birdyear.xtabs)$birdyear) {
  jpeg(paste("output/time allocation per bird per year/",birdyear,".jpeg",sep=''))
  #windows(5,8)
  dur.behav.hr.eggs = as.matrix(prop.table(dur.behav.hr.birdyear.xtabs[,,'3.eggs',birdyear], 2))
  dur.behav.hr.chicks = as.matrix(prop.table(dur.behav.hr.birdyear.xtabs[,,'4.chicks',birdyear], 2))
  dur.behav.hr.post = as.matrix(prop.table(dur.behav.hr.birdyear.xtabs[,,'5.post-breeding',birdyear], 2))
  layout(matrix(1:3, byrow=T, ncol=1))
  par(mar=c(1,1,1.2,0), oma=c(5,5,4,1))
  barplot(dur.behav.hr.eggs[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
  mtext('EGGS', 4, -0.5, cex=0.9)
  barplot(dur.behav.hr.chicks[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n', cex.axis=1.3, border=NA, space=c(0.1,0.1))
  mtext('CHICKS', 4, -0.5, cex=.9)
  barplot(dur.behav.hr.post[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1 ,yaxt='n' , cex.axis=1.3, cex=1, border=NA, space=c(0.1,0.1))
  mtext('POST-BREEDING', 4, -0.5, cex=.9)
  mtext('Time of day in hours (local time)', 1, 3, cex=1.2, outer=T)
  mtext('Time allocation', 2, 3, cex=1.2, outer=T)
  mtext(birdyear,3,1,cex=1.2,outer=T)
  dev.off()
}

dur.behav.hr.xtabs = xtabs(duration_behav~behaviour2+hour_CEST+sex+breeding.phase2, data=gps.breeding.data.behav)
rownames(dur.behav.hr.xtabs)
unique(paste(gps.breeding.data.behav$birdID, gps.breeding.data.behav$sex))
dur.behav.hr.m.eggs = as.matrix(prop.table(dur.behav.hr.xtabs[,,'M','3.eggs'], 2))
dur.behav.hr.f.eggs = as.matrix(prop.table(dur.behav.hr.xtabs[,,'F','3.eggs'], 2))
dur.behav.hr.m.chicks = as.matrix(prop.table(dur.behav.hr.xtabs[,,'M','4.chicks'], 2))
dur.behav.hr.f.chicks = as.matrix(prop.table(dur.behav.hr.xtabs[,,'F','4.chicks'], 2))
dur.behav.hr.m.post = as.matrix(prop.table(dur.behav.hr.xtabs[,,'M','5.post-breeding'], 2))
dur.behav.hr.f.post = as.matrix(prop.table(dur.behav.hr.xtabs[,,'F','5.post-breeding'], 2))

# and now plot all 4 breeding phases
pdf("output/Fig2A.pdf",10,8)
# windows(10,8)
layout(matrix(1:6, byrow=T, ncol=2))
par(mar=c(1,1,1.2,0), oma=c(5,5,4,15))
barplot(dur.behav.hr.f.eggs[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('FEMALE', 3, 0.6, cex=1)
barplot(dur.behav.hr.m.eggs[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n', yaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('EGGS', 4, -0.5, cex=0.9)
mtext('MALE', 3, 0.6, cex=1)
barplot(dur.behav.hr.f.chicks[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n', cex.axis=1.3, border=NA, space=c(0.1,0.1))
barplot(dur.behav.hr.m.chicks[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n', yaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('CHICKS', 4, -0.5, cex=.9)
barplot(dur.behav.hr.f.post[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1 , cex.axis=1.3, cex=1, border=NA, space=c(0.1,0.1))
barplot(dur.behav.hr.m.post[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1 ,yaxt='n' , cex.axis=1.3, cex=1, border=NA, space=c(0.1,0.1))
mtext('POST-BREEDING', 4, -0.5, cex=.9)
mtext('Time of day in hours (local time)', 1, 3, cex=1.2, outer=T)
mtext('Time allocation', 2, 3, cex=1.2, outer=T)
legend(29, 1, c('foraging marine', 'foraging brackish', 'foraging fresh', 'flying', 'other', 'resting tidal flats', 'resting mainland', 'resting island', 'nest attendance'), pch=15, col=rev(cols.behaviour2), bty='n', xpd=NA, cex=1.5)
dev.off()

### Now make the plot only for eggs and chicks phase, without habitat specification
pdf("output/Fig2B.pdf",10,8)
# windows(10,8)
layout(matrix(1:6, byrow=T, ncol=2))
par(mar=c(1,1,1.2,0), oma=c(5,5,4,15))
barplot(dur.behav.hr.f.eggs[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2.pooled, las=1, xaxt='n', cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('FEMALE', 3, 0.6, cex=1)
barplot(dur.behav.hr.m.eggs[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2.pooled, las=1, xaxt='n', yaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('EGGS', 4, -0.5, cex=0.9)
mtext('MALE', 3, 0.6, cex=1)
barplot(dur.behav.hr.f.chicks[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2.pooled, las=1, xaxt='n', cex.axis=1.3, border=NA, space=c(0.1,0.1))
barplot(dur.behav.hr.m.chicks[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2.pooled, las=1, xaxt='n', yaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('CHICKS', 4, -0.5, cex=.9)
barplot(dur.behav.hr.f.post[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2.pooled, las=1 , cex.axis=1.3, cex=1, border=NA, space=c(0.1,0.1))
barplot(dur.behav.hr.m.post[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2.pooled, las=1 ,yaxt='n' , cex.axis=1.3, cex=1, border=NA, space=c(0.1,0.1))
mtext('POST-BREEDING', 4, -0.5, cex=.9)
mtext('Time of day in hours (local time)', 1, 3, cex=1.2, outer=T)
mtext('Time allocation', 2, 3, cex=1.2, outer=T)
legend(29, 0.6, c('foraging', 'flying', 'other', 'resting', 'nest attendance'), pch=15, col=rev(unique(cols.behaviour2.pooled)), bty='n', xpd=NA, cex=1.5)
dev.off()

############## FIGURE 3 ###############
pdf("output/Fig3.pdf",6,7.5)
#windows(8,10)
layout(1:2)
par(mar=c(1,1,0,0), oma=c(4,4,3,1))
# PANEL A: proportion of time spent foraging ~ sex + breeding phase
boxplot(prop_for~breeding.phase, data=duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$sex=="F",], xaxt='n', col="lightcoral", medcol="coral4", whiskcol="coral4", outcol="coral4", staplecol="coral4", boxcol="coral4", xlab='', ylab='', boxwex=0.25, ylim=c(0.1,0.5), xlim=c(0.8,4.5), las=1, cex.axis=1)
boxplot(prop_for~breeding.phase, data=duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", medcol="lightskyblue4", whiskcol="lightskyblue4", outcol="lightskyblue4", staplecol="lightskyblue4", boxcol="lightskyblue4", ylab='', at=1:4+0.3, boxwex=0.25, add=T)
mtext("Prop. time spent foraging",2,3,cex=1.2)
legend("topright", legend=c("females","males"), pch=22, pt.bg = c("lightcoral","lightskyblue"), col=c("coral4","lightskyblue4"), cex=1)
text(.85,0.11,"(a)")
# show sample sizes! 
N.prop.for.phase.sex <- table(duration_behaviour_bird_phase_year$breeding.phase, duration_behaviour_bird_phase_year$sex)
axis(3, at=1:4, N.prop.for.phase.sex[,1], tick=F, line=-0.5, cex.axis=1.1)
axis(3, at=1:4+0.3, N.prop.for.phase.sex[,2], tick=F, line=-0.5, cex.axis=1.1)
axis(3, at=0.7, "N =", tick=F, line=-0.5, cex.axis=1.1)
# PANEL B: proportion of foraging time in marine waters ~ sex + breeding phase
boxplot(prop_for_marine~breeding.phase, data=duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$sex=="F",], xaxt='n', col="lightcoral", medcol="coral4", whiskcol="coral4", outcol="coral4", staplecol="coral4", boxcol="coral4", ylab='', boxwex=0.25, ylim=c(0,1), xlim=c(0.8,4.5), las=1, cex.axis=1, xlab="")
boxplot(prop_for_marine~breeding.phase, data=duration_behaviour_bird_phase_year[duration_behaviour_bird_phase_year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", medcol="lightskyblue4", whiskcol="lightskyblue4", outcol="lightskyblue4", staplecol="lightskyblue4", boxcol="lightskyblue4", ylab='', at=1:4+0.3, boxwex=0.25, add=T)
axis(1, at=1:4+0.15, labels=c('eggs', 'chicks','post +', 'post -'), cex.axis=1.1)
text(.85,0.03,"(b)")
mtext("Prop. marine foraging",2,3,cex=1.2)
mtext("Breeding phase",1,3,cex=1.2)
dev.off()

### FIGURE S3 ###
# nest attendance in relation to hour of the day and tidal phase for females and males, during egg incubation phase. 
# calculate means and 95% CI on the basis of the raw (averaged) data (assuming a normal distribution, see: https://www.statisticshowto.com/probability-and-statistics/confidence-interval/#CISample ) :
mean.prop.at.nest.sex.hour.tidal.phase <- aggregate(logit_at_nest~sex+breeding.phase+tidal_phase+hour_CEST, duration.behaviour.per.bird.hour.tidal.breeding.phase, mean)
sd.prop.at.nest.sex.hour.tidal.phase <- aggregate(logit_at_nest~sex+breeding.phase+tidal_phase+hour_CEST, duration.behaviour.per.bird.hour.tidal.breeding.phase, sd)
duration.behaviour.per.bird.hour.tidal.breeding.phase$freq=1
N.prop.at.nest.sex.hour.tidal.phase <- aggregate(freq~sex+breeding.phase+tidal_phase+hour_CEST, duration.behaviour.per.bird.hour.tidal.breeding.phase, sum)
sd.prop.at.nest.sex.hour.tidal.phase$se <- sd.prop.at.nest.sex.hour.tidal.phase$logit_at_nest/sqrt(N.prop.at.nest.sex.hour.tidal.phase$freq)
mean.prop.at.nest.sex.hour.tidal.phase$lower.CL <- mean.prop.at.nest.sex.hour.tidal.phase$logit_at_nest - 1.96 * sd.prop.at.nest.sex.hour.tidal.phase$se
mean.prop.at.nest.sex.hour.tidal.phase$upper.CL <- mean.prop.at.nest.sex.hour.tidal.phase$logit_at_nest + 1.96 * sd.prop.at.nest.sex.hour.tidal.phase$se
# translate to proportion instead of logit
mean.prop.at.nest.sex.hour.tidal.phase$prop.at.nest <- plogis(mean.prop.at.nest.sex.hour.tidal.phase$logit_at_nest)
mean.prop.at.nest.sex.hour.tidal.phase$lower.CL <- plogis(mean.prop.at.nest.sex.hour.tidal.phase$lower.CL)
mean.prop.at.nest.sex.hour.tidal.phase$upper.CL <- plogis(mean.prop.at.nest.sex.hour.tidal.phase$upper.CL)
mean.prop.at.nest.sex.hour.tidal.phase.eggs.females <- mean.prop.at.nest.sex.hour.tidal.phase[mean.prop.at.nest.sex.hour.tidal.phase$sex=='F'&mean.prop.at.nest.sex.hour.tidal.phase$breeding.phase=='3.eggs',]
mean.prop.at.nest.sex.hour.tidal.phase.eggs.males <- mean.prop.at.nest.sex.hour.tidal.phase[mean.prop.at.nest.sex.hour.tidal.phase$sex=='M'&mean.prop.at.nest.sex.hour.tidal.phase$breeding.phase=='3.eggs',]
mean.prop.at.nest.sex.hour.tidal.phase.chicks.females <- mean.prop.at.nest.sex.hour.tidal.phase[mean.prop.at.nest.sex.hour.tidal.phase$sex=='F'&mean.prop.at.nest.sex.hour.tidal.phase$breeding.phase=='4.chicks',]
mean.prop.at.nest.sex.hour.tidal.phase.chicks.males <- mean.prop.at.nest.sex.hour.tidal.phase[mean.prop.at.nest.sex.hour.tidal.phase$sex=='M'&mean.prop.at.nest.sex.hour.tidal.phase$breeding.phase=='4.chicks',]

pdf("output/FigS3.pdf",10,7)
#windows(10,7)
layout(matrix(1:4, ncol=2, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4,3,10))
### (A) females
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$hour_CEST[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='low'], 
       mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='low'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='low'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='low'], 
       sfrac=0, gap=0, xlab="hour", ylab="", col="red", pch=19, xlim=c(0,24), ylim=c(0,1), xaxt='n')
mtext("females",3,1,cex=1.2)
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$hour[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='incoming']+0.2, 
       mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='incoming'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='incoming'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='incoming'], 
       sfrac=0, gap=0, col="green", pch=19, add=T)
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$hour[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='outgoing']+0.4, 
       mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='outgoing'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='outgoing'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='outgoing'], 
       sfrac=0, gap=0, col="orange", pch=19, add=T)
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$hour[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='high']+0.6, 
       mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='high'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='high'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.females$tidal_phase=='high'], 
       sfrac=0, gap=0, col="blue", pch=19, add=T)
### (B) males
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$hour_CEST[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='low'], 
       mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='low'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='low'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='low'], 
       sfrac=0, gap=0, xlab="hour", ylab="", yaxt="n", col="red", pch=19, xlim=c(0,24), ylim=c(0,1), xaxt='n')
mtext("males",3,1,cex=1.2)
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$hour[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='incoming']+0.2, 
       mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='incoming'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='incoming'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='incoming'], 
       sfrac=0, gap=0, col="green", pch=19, add=T)
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$hour[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='outgoing']+0.4, 
       mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='outgoing'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='outgoing'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='outgoing'], 
       sfrac=0, gap=0, col="orange", pch=19, add=T)
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$hour[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='high']+0.6, 
       mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='high'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='high'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.eggs.males$tidal_phase=='high'], 
       sfrac=0, gap=0, col="blue", pch=19, add=T)
mtext('EGGS', 4, 0.5, cex=0.9)
### (C) females - chicks
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$hour_CEST[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='low'], 
       mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='low'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='low'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='low'], 
       sfrac=0, gap=0, xlab="hour", ylab="", col="red", pch=19, xlim=c(0,24), ylim=c(0,1))
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$hour[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='incoming']+0.2, 
       mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='incoming'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='incoming'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='incoming'], 
       sfrac=0, gap=0, col="green", pch=19, add=T)
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$hour[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='outgoing']+0.4, 
       mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='outgoing'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='outgoing'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='outgoing'], 
       sfrac=0, gap=0, col="orange", pch=19, add=T)
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$hour[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='high']+0.6, 
       mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='high'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='high'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.females$tidal_phase=='high'], 
       sfrac=0, gap=0, col="blue", pch=19, add=T)
### (D) males - chicks
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$hour_CEST[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='low'], 
       mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='low'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='low'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='low'], 
       sfrac=0, gap=0, xlab="hour", ylab="", yaxt="n", col="red", pch=19, xlim=c(0,24), ylim=c(0,1))
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$hour[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='incoming']+0.2, 
       mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='incoming'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='incoming'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='incoming'], 
       sfrac=0, gap=0, col="green", pch=19, add=T)
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$hour[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='outgoing']+0.4, 
       mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='outgoing'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='outgoing'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='outgoing'], 
       sfrac=0, gap=0, col="orange", pch=19, add=T)
plotCI(mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$hour[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='high']+0.6, 
       mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$prop.at.nest[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='high'], 
       li=mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$lower.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='high'], 
       ui=mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$upper.CL[mean.prop.at.nest.sex.hour.tidal.phase.chicks.males$tidal_phase=='high'], 
       sfrac=0, gap=0, col="blue", pch=19, add=T)
mtext('CHICKS', 4, 0.5, cex=0.9)
mtext("probability of nest attendance",2,2, outer=T, cex=1.2)
mtext("hour",1,2, outer=T, cex=1.2)
legend(28.5,0.40,c("low","incoming","high","outgoing"),pch=19, col=c("red","green","blue","orange"), xpd=NA, cex=1.2)
dev.off()

### END OF FIGURE S3 ###

### FIGURE S4 - TIMING OF FORAGING ###
mean.prop.foraging.sex.hour.tidal.phase <- aggregate(logit_foraging~sex+breeding.phase+tidal_phase+hour_CEST, duration.behaviour.per.bird.hour.tidal.breeding.phase, mean)
sd.prop.foraging.sex.hour.tidal.phase <- aggregate(logit_foraging~sex+breeding.phase+tidal_phase+hour_CEST, duration.behaviour.per.bird.hour.tidal.breeding.phase, sd)
N.prop.foraging.sex.hour.tidal.phase <- aggregate(freq~sex+breeding.phase+tidal_phase+hour_CEST, duration.behaviour.per.bird.hour.tidal.breeding.phase, sum)
sd.prop.foraging.sex.hour.tidal.phase$se <- sd.prop.foraging.sex.hour.tidal.phase$logit_foraging/sqrt(N.prop.foraging.sex.hour.tidal.phase$freq)
mean.prop.foraging.sex.hour.tidal.phase$lower.CL <- mean.prop.foraging.sex.hour.tidal.phase$logit_foraging - 1.96 * sd.prop.foraging.sex.hour.tidal.phase$se
mean.prop.foraging.sex.hour.tidal.phase$upper.CL <- mean.prop.foraging.sex.hour.tidal.phase$logit_foraging + 1.96 * sd.prop.foraging.sex.hour.tidal.phase$se
# translate to proportion instead of logit
mean.prop.foraging.sex.hour.tidal.phase$prop.foraging <- plogis(mean.prop.foraging.sex.hour.tidal.phase$logit_foraging)
mean.prop.foraging.sex.hour.tidal.phase$lower.CL <- plogis(mean.prop.foraging.sex.hour.tidal.phase$lower.CL)
mean.prop.foraging.sex.hour.tidal.phase$upper.CL <- plogis(mean.prop.foraging.sex.hour.tidal.phase$upper.CL)
mean.prop.foraging.sex.hour.tidal.phase.eggs.females <- mean.prop.foraging.sex.hour.tidal.phase[mean.prop.foraging.sex.hour.tidal.phase$sex=='F'&mean.prop.foraging.sex.hour.tidal.phase$breeding.phase=='3.eggs',]
mean.prop.foraging.sex.hour.tidal.phase.eggs.males <- mean.prop.foraging.sex.hour.tidal.phase[mean.prop.foraging.sex.hour.tidal.phase$sex=='M'&mean.prop.foraging.sex.hour.tidal.phase$breeding.phase=='3.eggs',]
mean.prop.foraging.sex.hour.tidal.phase.chicks.females <- mean.prop.foraging.sex.hour.tidal.phase[mean.prop.foraging.sex.hour.tidal.phase$sex=='F'&mean.prop.foraging.sex.hour.tidal.phase$breeding.phase=='4.chicks',]
mean.prop.foraging.sex.hour.tidal.phase.chicks.males <- mean.prop.foraging.sex.hour.tidal.phase[mean.prop.foraging.sex.hour.tidal.phase$sex=='M'&mean.prop.foraging.sex.hour.tidal.phase$breeding.phase=='4.chicks',]
mean.prop.foraging.sex.hour.tidal.phase.postsuc.females <- mean.prop.foraging.sex.hour.tidal.phase[mean.prop.foraging.sex.hour.tidal.phase$sex=='F'&mean.prop.foraging.sex.hour.tidal.phase$breeding.phase=='5.post.breeding.successful',]
mean.prop.foraging.sex.hour.tidal.phase.postsuc.males <- mean.prop.foraging.sex.hour.tidal.phase[mean.prop.foraging.sex.hour.tidal.phase$sex=='M'&mean.prop.foraging.sex.hour.tidal.phase$breeding.phase=='5.post.breeding.successful',]
mean.prop.foraging.sex.hour.tidal.phase.postunsuc.females <- mean.prop.foraging.sex.hour.tidal.phase[mean.prop.foraging.sex.hour.tidal.phase$sex=='F'&mean.prop.foraging.sex.hour.tidal.phase$breeding.phase=='6.post.breeding.unsuccessful',]
mean.prop.foraging.sex.hour.tidal.phase.postunsuc.males <- mean.prop.foraging.sex.hour.tidal.phase[mean.prop.foraging.sex.hour.tidal.phase$sex=='M'&mean.prop.foraging.sex.hour.tidal.phase$breeding.phase=='6.post.breeding.unsuccessful',]

pdf("output/FigS4.pdf")
#windows()
layout(matrix(1:6, ncol=2, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4,3,10))
### (A) females - eggs
plotCI(mean.prop.foraging.sex.hour.tidal.phase.eggs.females$hour_CEST[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='low'], 
       mean.prop.foraging.sex.hour.tidal.phase.eggs.females$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='low'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.eggs.females$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='low'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.eggs.females$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='low'], 
       sfrac=0, gap=0, xlab="", ylab="", col="red", pch=19, xlim=c(0,24), ylim=c(0,1), xaxt='n')
mtext("females",3,1,cex=1.2)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.eggs.females$hour[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='incoming']+0.2, 
       mean.prop.foraging.sex.hour.tidal.phase.eggs.females$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='incoming'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.eggs.females$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='incoming'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.eggs.females$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='incoming'], 
       sfrac=0, gap=0, col="green", pch=19, add=T)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.eggs.females$hour[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='outgoing']+0.4, 
       mean.prop.foraging.sex.hour.tidal.phase.eggs.females$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='outgoing'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.eggs.females$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='outgoing'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.eggs.females$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='outgoing'], 
       sfrac=0, gap=0, col="orange", pch=19, add=T)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.eggs.females$hour[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='high']+0.6, 
       mean.prop.foraging.sex.hour.tidal.phase.eggs.females$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='high'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.eggs.females$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='high'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.eggs.females$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.females$tidal_phase=='high'], 
       sfrac=0, gap=0, col="blue", pch=19, add=T)
### (B) males - eggs
plotCI(mean.prop.foraging.sex.hour.tidal.phase.eggs.males$hour_CEST[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='low'], 
       mean.prop.foraging.sex.hour.tidal.phase.eggs.males$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='low'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.eggs.males$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='low'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.eggs.males$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='low'], 
       sfrac=0, gap=0, xlab="", ylab="", yaxt="n", col="red", pch=19, xlim=c(0,24), ylim=c(0,1), xaxt='n')
mtext("males",3,1,cex=1.2)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.eggs.males$hour[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='incoming']+0.2, 
       mean.prop.foraging.sex.hour.tidal.phase.eggs.males$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='incoming'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.eggs.males$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='incoming'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.eggs.males$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='incoming'], 
       sfrac=0, gap=0, col="green", pch=19, add=T)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.eggs.males$hour[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='outgoing']+0.4, 
       mean.prop.foraging.sex.hour.tidal.phase.eggs.males$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='outgoing'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.eggs.males$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='outgoing'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.eggs.males$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='outgoing'], 
       sfrac=0, gap=0, col="orange", pch=19, add=T)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.eggs.males$hour[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='high']+0.6, 
       mean.prop.foraging.sex.hour.tidal.phase.eggs.males$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='high'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.eggs.males$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='high'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.eggs.males$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.eggs.males$tidal_phase=='high'], 
       sfrac=0, gap=0, col="blue", pch=19, add=T)
mtext('EGGS', 4, 0.5, cex=0.9)
### (C) females - chicks
plotCI(mean.prop.foraging.sex.hour.tidal.phase.chicks.females$hour_CEST[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='low'], 
       mean.prop.foraging.sex.hour.tidal.phase.chicks.females$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='low'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.chicks.females$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='low'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.chicks.females$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='low'], 
       sfrac=0, gap=0, xlab="", ylab="", col="red", pch=19, xlim=c(0,24), ylim=c(0,1), xaxt='n')
plotCI(mean.prop.foraging.sex.hour.tidal.phase.chicks.females$hour[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='incoming']+0.2, 
       mean.prop.foraging.sex.hour.tidal.phase.chicks.females$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='incoming'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.chicks.females$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='incoming'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.chicks.females$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='incoming'], 
       sfrac=0, gap=0, col="green", pch=19, add=T)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.chicks.females$hour[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='outgoing']+0.4, 
       mean.prop.foraging.sex.hour.tidal.phase.chicks.females$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='outgoing'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.chicks.females$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='outgoing'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.chicks.females$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='outgoing'], 
       sfrac=0, gap=0, col="orange", pch=19, add=T)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.chicks.females$hour[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='high']+0.6, 
       mean.prop.foraging.sex.hour.tidal.phase.chicks.females$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='high'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.chicks.females$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='high'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.chicks.females$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.females$tidal_phase=='high'], 
       sfrac=0, gap=0, col="blue", pch=19, add=T)
### (D) males - chicks
plotCI(mean.prop.foraging.sex.hour.tidal.phase.chicks.males$hour_CEST[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='low'], 
       mean.prop.foraging.sex.hour.tidal.phase.chicks.males$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='low'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.chicks.males$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='low'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.chicks.males$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='low'], 
       sfrac=0, gap=0, xlab="", ylab="", yaxt="n", col="red", pch=19, xlim=c(0,24), ylim=c(0,1), xaxt='n')
plotCI(mean.prop.foraging.sex.hour.tidal.phase.chicks.males$hour[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='incoming']+0.2, 
       mean.prop.foraging.sex.hour.tidal.phase.chicks.males$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='incoming'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.chicks.males$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='incoming'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.chicks.males$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='incoming'], 
       sfrac=0, gap=0, col="green", pch=19, add=T)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.chicks.males$hour[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='outgoing']+0.4, 
       mean.prop.foraging.sex.hour.tidal.phase.chicks.males$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='outgoing'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.chicks.males$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='outgoing'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.chicks.males$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='outgoing'], 
       sfrac=0, gap=0, col="orange", pch=19, add=T)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.chicks.males$hour[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='high']+0.6, 
       mean.prop.foraging.sex.hour.tidal.phase.chicks.males$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='high'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.chicks.males$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='high'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.chicks.males$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.chicks.males$tidal_phase=='high'], 
       sfrac=0, gap=0, col="blue", pch=19, add=T)
mtext('CHICKS', 4, 0.5, cex=0.9)
### (E) females - postsuc
plotCI(mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$hour_CEST[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='low'], 
       mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='low'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='low'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='low'], 
       sfrac=0, gap=0, xlab="", ylab="", col="red", pch=19, xlim=c(0,24), ylim=c(0,1))
plotCI(mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$hour[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='incoming']+0.2, 
       mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='incoming'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='incoming'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='incoming'], 
       sfrac=0, gap=0, col="green", pch=19, add=T)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$hour[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='outgoing']+0.4, 
       mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='outgoing'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='outgoing'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='outgoing'], 
       sfrac=0, gap=0, col="orange", pch=19, add=T)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$hour[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='high']+0.6, 
       mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='high'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='high'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.females$tidal_phase=='high'], 
       sfrac=0, gap=0, col="blue", pch=19, add=T)
### (F) males - postsuc
plotCI(mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$hour_CEST[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='low'], 
       mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='low'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='low'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='low'], 
       sfrac=0, gap=0, xlab="", ylab="", yaxt="n", col="red", pch=19, xlim=c(0,24), ylim=c(0,1))
plotCI(mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$hour[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='incoming']+0.2, 
       mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='incoming'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='incoming'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='incoming'], 
       sfrac=0, gap=0, col="green", pch=19, add=T)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$hour[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='outgoing']+0.4, 
       mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='outgoing'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='outgoing'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='outgoing'], 
       sfrac=0, gap=0, col="orange", pch=19, add=T)
plotCI(mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$hour[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='high']+0.6, 
       mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$prop.foraging[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='high'], 
       li=mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$lower.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='high'], 
       ui=mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$upper.CL[mean.prop.foraging.sex.hour.tidal.phase.postsuc.males$tidal_phase=='high'], 
       sfrac=0, gap=0, col="blue", pch=19, add=T)
mtext('POST-BREEDING', 4, 0.5, cex=0.9)
mtext("probability of foraging",2,2, outer=T, cex=1.2)
mtext("hour",1,2, outer=T, cex=1.2)
legend(28.5,0.42,c("low","incoming","high","outgoing"),pch=19, col=c("red","green","blue","orange"), xpd=NA, cex=1.2)
dev.off()
### END FIGURE S4 ###



# Exploration graphs:

## Foraging duration (time spent foraging) as a function of breeding phase (pooling the sexes):
windows()
boxplot(dur_foraging_yday~breeding.phase, data=foraging_duration_yday_bird_phase, names=c('eggs', 'chicks','post +', 'post -'), col = breeding.phase.cols[3:6], ylab = 'Foraging duration per 24 h (hours)', cex.lab=1.3)
## Same plot but now with proportion of time spent foraging
windows()
boxplot(prop_for~breeding.phase, data=foraging_duration_yday_bird_phase, names=c('eggs', 'chicks','post +', 'post -'), col=breeding.phase.cols[3:6], ylab='Proportion of time spent foraging', cex.lab=1.3)

## Foraging duration as a function of weeknr and sex
birdyears = unique(duration_yday_birdID_year[,c('birdID','year','sex')])
birdyears = birdyears[order(birdyears$year, birdyears$birdID),]
birdyears.sex = birdyears[rev(order(birdyears$sex)),]
windows()
plot(for_dur_week_bird_year$week, 1:dim(for_dur_week_bird_year)[1], ylim=c(0,max(for_dur_week_bird_year$dur_foraging_yday)), type='n', xlab='Week of the year', ylab='Time spent foraging per day (h)')
for (i in 1:dim(birdyears.sex)[1]) {
  bird = birdyears.sex$birdID[i]
  year = birdyears.sex$year[i]
  df = for_dur_week_bird_year[for_dur_week_bird_year$birdID==bird&for_dur_week_bird_year$year==year,]
  #lines()
  points(dur_foraging_yday~week, for_dur_week_bird_year[for_dur_week_bird_year$birdID==bird&for_dur_week_bird_year$year==year,], pch=19, col=c('blue','red')[sex.nr], cex=0.5)
  lines(dur_foraging_yday~week, for_dur_week_bird_year[for_dur_week_bird_year$birdID==bird&for_dur_week_bird_year$year==year,], col=c('blue','red')[sex.nr], lwd=2)
}

# Foraging duration as a function of week relative to hatch date (i.e. age of the eggs/chicks)
windows()
plot(for_dur_week_rel_hatching_bird_year$week_rel_hatching, 1:dim(for_dur_week_rel_hatching_bird_year)[1], ylim=c(0,max(for_dur_week_rel_hatching_bird_year$dur_foraging_yday)), type='n', xlab='Week relative to hatching', ylab='Time spent foraging per day (h)')
for (i in 1:dim(birdyears.sex)[1]) {
  bird = birdyears.sex$birdID[i]
  year = birdyears.sex$year[i]
  df = for_dur_week_rel_hatching_bird_year[for_dur_week_rel_hatching_bird_year$birdID==bird&for_dur_week_rel_hatching_bird_year$year==year,]
  #lines()
  points(dur_foraging_yday~week_rel_hatching, for_dur_week_rel_hatching_bird_year[for_dur_week_rel_hatching_bird_year$birdID==bird&for_dur_week_rel_hatching_bird_year$year==year,], pch=19, col=c('blue','red')[sex.nr], cex=0.5)
  lines(dur_foraging_yday~week_rel_hatching, for_dur_week_rel_hatching_bird_year[for_dur_week_rel_hatching_bird_year$birdID==bird&for_dur_week_rel_hatching_bird_year$year==year,], col=c('blue','red')[sex.nr], lwd=2)
}
