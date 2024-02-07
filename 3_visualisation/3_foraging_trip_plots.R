# calculate mean distance of foraging trip per bird per breeding phase for plotting
for.trip.dist.dur.bird.year <- aggregate(cbind(distance.to.nest, trip.duration) ~ breeding.phase + year + birdID + sex, df.for.trips, mean)
# calculate mean number of foraging trips per bird per breeding phase per year for plotting
n_trips_day_bird_year_mean = aggregate(n_trips~year+birdID+sex+breeding.phase, data=n_trips_day_bird_year_zeros, mean)
n_trips_day_bird_year_mean$breeding.phase = factor(n_trips_day_bird_year_mean$breeding.phase)
table(paste(n_trips_day_bird_year_mean$sex, n_trips_day_bird_year_mean$birdID))
birds.for.trips = unique(df.for.trips[,c('birdID','year','sex','breeding.phase')])
sample.sizes.for.trips <- table(birds.for.trips$breeding.phase, birds.for.trips$sex)

############### FIGURE 6 ###############
# Now combine the three graphs into one
pdf("output/Fig6.pdf",13,7)
#windows(14,8)
par(mar=c(1,6,0,0), oma=c(4,0,3,10))
layout(matrix(1:3, ncol=3))
ymin=0
# number of foraging trips per 24h
ymax=4.5
boxplot(n_trips~breeding.phase, data=n_trips_day_bird_year_mean[n_trips_day_bird_year_mean$sex=="F",], xaxt='n', yaxs='i', col="lightcoral", medcol="coral4", whiskcol="coral4", outcol="coral4", staplecol="coral4", boxcol="coral4", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,ymax), las=1, cex.axis=2)
boxplot(n_trips~breeding.phase, data=n_trips_day_bird_year_mean[n_trips_day_bird_year_mean$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", medcol="lightskyblue4", whiskcol="lightskyblue4", outcol="lightskyblue4", staplecol="lightskyblue4", boxcol="lightskyblue4", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=F)
axis(1, at=1:2+0.15, labels=c('incubation', 'chick-'), cex.axis=2, line=0.5, tick=F)
axis(1, at=1:2+0.15, labels=c('', 'rearing'), cex.axis=2, line=2, tick=F)
plot.sign.for.trips(ymax=ymax, signs=n.trips.pairs$sign)
mtext("Number of foraging trips per 24 hours", 2, 3.5, cex=1.5)
text(0.9,0.97*ymax,"(a)",cex=2)
# show sample sizes! 
axis(3, at=1:2, sample.sizes.for.trips[1:2,1], tick=F, cex.axis=1.8)
axis(3, at=1:2+0.3, sample.sizes.for.trips[1:2,2], tick=F, cex.axis=1.8)
axis(3, at=0.8, "N =", tick=F, cex.axis=1.8)
# duration of foraging trips
ymax=14
boxplot(trip.duration~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="F",], xaxt='n', yaxs='i', col="lightcoral", medcol="coral4", whiskcol="coral4", outcol="coral4", staplecol="coral4", boxcol="coral4", xlab='', ylab='', boxwex=0.25, xlim=c(0.8,2.5), ylim=c(0,ymax), las=1, cex.axis=2)
boxplot(trip.duration~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", medcol="lightskyblue4", whiskcol="lightskyblue4", outcol="lightskyblue4", staplecol="lightskyblue4", boxcol="lightskyblue4", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=F)
axis(1, at=1:2+0.15, labels=c('incubation', 'chick-'), cex.axis=2, line=0.5, tick=F)
axis(1, at=1:2+0.15, labels=c('', 'rearing'), cex.axis=2, line=2, tick=F)
mtext("Foraging trip duration (h)", 2, 3.5, cex=1.5)
text(0.9,0.97*ymax,"(b)",cex=2)
plot.sign.for.trips(ymax=ymax, signs=dur.trips.pairs$sign)
# distance of foraging trips
ymax=18
boxplot(distance.to.nest~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="F",], xaxt='n', yaxs='i', col="lightcoral", medcol="coral4", whiskcol="coral4", outcol="coral4", staplecol="coral4", boxcol="coral4", xlab='', ylab='', boxwex=0.25, ylim=c(0,ymax), xlim=c(0.8,2.5), las=1, cex.axis=2)
boxplot(distance.to.nest~breeding.phase, data=for.trip.dist.dur.bird.year[for.trip.dist.dur.bird.year$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", medcol="lightskyblue4", whiskcol="lightskyblue4", outcol="lightskyblue4", staplecol="lightskyblue4", boxcol="lightskyblue4", ylab='', at=1:2+0.3, boxwex=0.25, add=T)
axis(1, at=1:2+0.15, labels=F)
axis(1, at=1:2+0.15, labels=c('incubation', 'chick-'), cex.axis=2, line=0.5, tick=F)
axis(1, at=1:2+0.15, labels=c('', 'rearing'), cex.axis=2, line=2, tick=F)
mtext("Foraging trip distance (km)", 2, 3.5, cex=1.5)
text(0.9,0.97*ymax,"(c)",cex=2)
plot.sign.for.trips(ymax=ymax, signs=dist.trips.pairs$sign)
legend(2.6,0.15*ymax, legend=c("female","male"), pch=22, pt.bg = c("lightcoral","lightskyblue"), col=c("coral4","lightskyblue4"), cex=2, bty='n', xpd=NA)
dev.off()
### END OF FIGURE 6 ###