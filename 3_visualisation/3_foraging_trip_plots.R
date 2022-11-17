# calculate mean distance of foraging trip per bird per breeding phase for plotting
for.trip.dist.dur.bird.year <- aggregate(cbind(distance.to.nest, trip.duration) ~ breeding.phase + year + birdID + sex, df.for.trips, mean)
# calculate mean number of foraging trips per bird per breeding phase per year for plotting
n_trips_day_bird_year_mean = aggregate(n_trips~year+birdID+sex+breeding.phase, data=n_trips_day_bird_year_zeros, mean)

############### FIGURE 4 ###############
# Now combine the three graphs into one
pdf("output/Fig4.pdf",12,8)
#windows(12,8)
par(mar=c(1,6,0,0), oma=c(5,0,3,1))
layout(matrix(1:3, ncol=3))
# number of foraging trips per 24h
boxplot(n_trips~breeding.phase, data=n_trips_day_bird_year_mean[n_trips_day_bird_year_mean$sex=="F",], xaxt='n', col="lightcoral", medcol="coral4", whiskcol="coral4", outcol="coral4", staplecol="coral4", boxcol="coral4", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,5), las=1, cex.axis=2)
boxplot(n_trips~breeding.phase, data=n_trips_day_bird_year_mean[n_trips_day_bird_year_mean$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", medcol="lightskyblue4", whiskcol="lightskyblue4", outcol="lightskyblue4", staplecol="lightskyblue4", boxcol="lightskyblue4", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=2)
mtext("Number of foraging trips per 24 hours", 2, 3.5, cex=1.5)
legend("topleft", legend=c("females","males"), pch=22, pt.bg = c("lightcoral","lightskyblue"), col=c("coral4","lightskyblue4"), cex=2)
text(0.9,0.1,"(a)",cex=2)
# show sample sizes! 
axis(3, at=1:2, N.prop.for.phase.sex[1:2,1], tick=F, cex.axis=1.8)
axis(3, at=1:2+0.3, N.prop.for.phase.sex[1:2,2], tick=F, cex.axis=1.8)
axis(3, at=0.8, "N =", tick=F, cex.axis=1.8)
# duration of foraging trips
boxplot(trip.duration~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="F",], xaxt='n', col="lightcoral", medcol="coral4", whiskcol="coral4", outcol="coral4", staplecol="coral4", boxcol="coral4", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,12), las=1, cex.axis=2)
boxplot(trip.duration~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", medcol="lightskyblue4", whiskcol="lightskyblue4", outcol="lightskyblue4", staplecol="lightskyblue4", boxcol="lightskyblue4", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=2)
mtext("Foraging trip duration (h)", 2, 3.5, cex=1.5)
text(0.9,0.1/5*12,"(b)",cex=2)
# distance of foraging trips
boxplot(distance.to.nest~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="F",], xaxt='n', col="lightcoral", medcol="coral4", whiskcol="coral4", outcol="coral4", staplecol="coral4", boxcol="coral4", xlab='', ylab='', boxwex=0.25, ylim=c(0,15), xlim=c(0.8,2.5), las=1, cex.axis=2)
boxplot(distance.to.nest~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", medcol="lightskyblue4", whiskcol="lightskyblue4", outcol="lightskyblue4", staplecol="lightskyblue4", boxcol="lightskyblue4", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=2)
mtext("Foraging trip distance (km)", 2, 3.5, cex=1.5)
mtext("Breeding phase", 1, 3, outer=T, cex=1.5)
text(0.9,0.1/5*15,"(c)",cex=2)
dev.off()
#### END OF FIGURE 4 ####


### FIGURE S5: discussion figure as to whether the sex-specific distance of foraging trips is related to habitat preferences or variation in body size (males being larger and thus able to carry more food back to the chicks per foraging trip):
pdf("output/FigS5.pdf", 12, 8)
#windows(12,8)
par(mar=c(1,6,0,0), oma=c(5,0,3,1))
layout(matrix(1:3, ncol=3))
# number of foraging marine_trips per 24h
boxplot(n_marine_trips~breeding.phase, data=n_marine_trips_day_bird_year_mean[n_marine_trips_day_bird_year_mean$sex=="F",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,5), las=1, cex.axis=2)
boxplot(n_marine_trips~breeding.phase, data=n_marine_trips_day_bird_year_mean[n_marine_trips_day_bird_year_mean$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=2)
mtext("Number of marine foraging trips per 24 hours", 2, 3.5, cex=1.5)
legend("topleft", legend=c("females","males"), pch=22, pt.bg = c("lightcoral","lightskyblue"), cex=2, bty="n")
# show sample sizes! 
axis(3, at=1:2, N.prop.for.phase.sex[,1], tick=F, cex.axis=1.8)
axis(3, at=1:2+0.3, N.prop.for.phase.sex[,2], tick=F, cex.axis=1.8)
axis(3, at=0.8, "N =", tick=F, cex.axis=1.8)
# duration of foraging marine_trips
boxplot(trip.duration~breeding.phase, data=marine.for.trip.dist.dur.bird.year[marine.for.trip.dist.dur.bird.year$sex=="F",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,12), las=1, cex.axis=2)
boxplot(trip.duration~breeding.phase, data=marine.for.trip.dist.dur.bird.year[marine.for.trip.dist.dur.bird.year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=2)
mtext("Marine foraging trip duration (h)", 2, 3.5, cex=1.5)
# distance of foraging marine_trips
boxplot(distance.to.nest~breeding.phase, data=marine.for.trip.dist.dur.bird.year[marine.for.trip.dist.dur.bird.year$sex=="F",], xaxt='n', col="lightcoral", xlab='', ylab='', boxwex=0.25, ylim=c(0,15), xlim=c(0.8,2.5), las=1, cex.axis=2)
boxplot(distance.to.nest~breeding.phase, data=marine.for.trip.dist.dur.bird.year[marine.for.trip.dist.dur.bird.year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=c('eggs', 'chicks'), cex.axis=2)
mtext("Marine foraging trip distance (km)", 2, 3.5, cex=1.5)
mtext("Breeding phase", 1, 3, outer=T, cex=1.5)
dev.off()
### END OF FIGURE S5 ###
###


# exploration plots:

windows(10,6)
layout(matrix(1:6, ncol=3, byrow=T))
boxplot(duration_foraging_marine~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="M"&df.for.trips$year==2013,],main="males in 2013", ylim=c(0,12), xlab="weeks to hatching", ylab="total duration foraging (h) in Waddensea", las=1)
boxplot(duration_foraging_marine~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="M"&df.for.trips$year==2014,],main="males in 2014", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(duration_foraging_marine~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="M"&df.for.trips$year==2015,],main="males in 2015", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(duration_foraging_marine~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="M"&df.for.trips$year==2016,],main="males in 2016", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(duration_foraging_marine~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="M"&df.for.trips$year==2017,],main="males in 2017", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(duration_foraging_marine~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="M"&df.for.trips$year==2018,],main="males in 2018", ylim=c(0,12), xlab="weeks to hatching", las=1)

windows(10,6)
layout(matrix(1:6, ncol=3, byrow=T))
boxplot(duration_foraging_marine~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="F"&df.for.trips$year==2013,],main="females in 2013", ylim=c(0,12), xlab="weeks to hatching", ylab="total daily duration foraging (h) in Waddensea", las=1)
boxplot(duration_foraging_marine~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="F"&df.for.trips$year==2014,],main="females in 2014", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(duration_foraging_marine~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="F"&df.for.trips$year==2016,],main="females in 2016", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(duration_foraging_marine~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="F"&df.for.trips$year==2017,],main="females in 2017", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(duration_foraging_marine~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="F"&df.for.trips$year==2018,],main="females in 2018", ylim=c(0,12), xlab="weeks to hatching", las=1)
boxplot(duration_foraging_marine~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="F"&df.for.trips$year==2019,],main="females in 2019", ylim=c(0,12), xlab="weeks to hatching", las=1)

# sex-specific distribution of foraging trip duration, specified per breeding phase:
windows(10,6)
layout(matrix(1:4,nrow=2,byrow=T))
multhist(list(df.for.trips$trip.duration[df.for.trips$sex=="M"&df.for.trips$breeding.phase.nr==3],
              df.for.trips$trip.duration[df.for.trips$sex=="F"&df.for.trips$breeding.phase.nr==3]), 
         breaks=seq(0,40,4), xlab='Duration foraging trip (h)', main='Egg incubation', col=c("lightskyblue","lightcoral"))
multhist(list(df.for.trips$trip.duration[df.for.trips$sex=="M"&df.for.trips$breeding.phase.nr==4],
              df.for.trips$trip.duration[df.for.trips$sex=="F"&df.for.trips$breeding.phase.nr==4]),
         breaks=seq(0,40,4), xlab='Duration foraging trip (h)', main='Chick rearing', col=c("lightskyblue","lightcoral"))
# sex-specific distribution of foraging trip distance, specified per breeding phase
multhist(list(df.for.trips$distance.to.nest[df.for.trips$sex=="M"&df.for.trips$breeding.phase.nr==3],
              df.for.trips$distance.to.nest[df.for.trips$sex=="F"&df.for.trips$breeding.phase.nr==3]),
         breaks=seq(0,40,4), xlab='Foraging trip distance (km)', main='Egg incubation', col=c("lightskyblue","lightcoral"))
multhist(list(df.for.trips$distance.to.nest[df.for.trips$sex=="M"&df.for.trips$breeding.phase.nr==4],
              df.for.trips$distance.to.nest[df.for.trips$sex=="F"&df.for.trips$breeding.phase.nr==4]),
         breaks=seq(0,40,4), xlab='Foraging trip distance (km)', main='Chick rearing', col=c("lightskyblue","lightcoral"))

# same graph but now only for males, and specified per freshwater or marine foraging trip
windows(10,6)
layout(matrix(1:4,nrow=2,byrow=T))
multhist(list(df.for.trips$trip.duration[df.for.trips$sex=="M"&df.for.trips$breeding.phase.nr==3&df.for.trips$marine.trip==1],df.for.trips$trip.duration[df.for.trips$sex=="M"&df.for.trips$breeding.phase.nr==3&df.for.trips$marine.trip==0]),breaks=seq(0,40,4), xlab='Duration foraging trip (h)', main='Egg incubation', col=c("blue","lightskyblue"), beside=F)
multhist(list(df.for.trips$trip.duration[df.for.trips$sex=="M"&df.for.trips$breeding.phase.nr==4&df.for.trips$marine.trip==1],df.for.trips$trip.duration[df.for.trips$sex=="M"&df.for.trips$breeding.phase.nr==4&df.for.trips$marine.trip==0]),breaks=seq(0,40,4), xlab='Duration foraging trip (h)', main='Chick rearing', col=c("blue","lightskyblue"), beside=F)
# sex-specific distribution of foraging trip distance, specified per breeding phase
multhist(list(df.for.trips$distance.to.nest[df.for.trips$sex=="M"&df.for.trips$breeding.phase.nr==3&df.for.trips$marine.trip==1],df.for.trips$distance.to.nest[df.for.trips$sex=="M"&df.for.trips$breeding.phase.nr==3&df.for.trips$marine.trip==0]),breaks=seq(0,40,4), xlab='Foraging trip distance (km)', main='Egg incubation', col=c("blue","lightskyblue"), beside=F)
multhist(list(df.for.trips$distance.to.nest[df.for.trips$sex=="M"&df.for.trips$breeding.phase.nr==4&df.for.trips$marine.trip==1],df.for.trips$distance.to.nest[df.for.trips$sex=="M"&df.for.trips$breeding.phase.nr==4&df.for.trips$marine.trip==0]),breaks=seq(0,40,4), xlab='Foraging trip distance (km)', main='Chick rearing', col=c("blue","lightskyblue"), beside=F)


# foraging trip duration in relation to age of the chicks (in weeks)
windows()
layout(matrix(1:2, ncol=1))
par(mar=c(2,4,1,1), oma=c(2,0,0,0))
boxplot(trip.duration~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="F",], col="lightcoral", ylim=c(0,20), ylab="")
boxplot(trip.duration~week_rel_hatching, data=df.for.trips[df.for.trips$sex=="M",], col="lightskyblue", ylim=c(0,20), ylab="")
mtext("Week relative to hatching",1,3)
mtext("Duration of foraging trip",2,-1, outer=T)
