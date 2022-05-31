source("functions.R") # to have the most updated version of the functions
gps.breeding.data.behav <- gps.breeding.data.behav.sel

table(gps.breeding.data.behav$habitat_salinity, gps.breeding.data.behav$habitat, gps.breeding.data.behav$behaviour)
table(gps.breeding.data.behav$nest1, gps.breeding.data.behav$year, gps.breeding.data.behav$birdID)

# check where nest location was not assigned:  
gps.breeding.data.behav$lat.nest[is.na(gps.breeding.data.behav$lat.nest)]=0
gps.breeding.data.behav$lon.nest[is.na(gps.breeding.data.behav$lon.nest)]=0
gps.breeding.data.behav$lat.nest2[is.na(gps.breeding.data.behav$lat.nest2)]=0
gps.breeding.data.behav$lon.nest2[is.na(gps.breeding.data.behav$lon.nest2)]=0
nest.locations.bird.year <- aggregate(cbind(nest1,nest2)~year+birdID+attempt+lat.nest+lon.nest+lat.nest1+lon.nest1+lat.nest2+lon.nest2,gps.breeding.data.behav,sum)
nest.locations.bird.year = nest.locations.bird.year[order(nest.locations.bird.year$year, nest.locations.bird.year$birdID, nest.locations.bird.year$attempt),]
nest.locations.bird.year[nest.locations.bird.year$lat.nest==0,]
# these were cases where the number of days in the egg phase was very small; presumably without 5 hours nest attendance, causing the hatch date to be outside the range of days with 5h nest attendance. This could be solved by making the criteria that the range of dates between the first and last day of 5 hours nest attendance should overlap with the period of 25 days before and 30 days after hatch date.  
# in version of 0530 lat.nest2/lon.nest2 didn't come across correctly, but that doesn't matter as the only cases where nest2 was a real attempt, this was correctly assigned to lat.nest en lon.nest. Code is now corrected though.
# resting on nest (within 5 m of the nest position, dit wordt door de kolommen nest 1 en nest 2 aangegeven):
# replace the latitude and longitude of the nest where the bird was present during most of the time during a given attempt
# (1) when nest1>nest2
nest.locations.bird.year[nest.locations.bird.year$lat.nest==0 & nest.locations.bird.year$nest1>nest.locations.bird.year$nest2,c('lat.nest','lon.nest')] <- nest.locations.bird.year[nest.locations.bird.year$lat.nest==0 & nest.locations.bird.year$nest1>nest.locations.bird.year$nest2,c('lat.nest1','lon.nest1')]
# (2) when nest2>nest1 and lat.nest2!=0 (meaning that nest2 was a real nesting attempt):
nest.locations.bird.year[nest.locations.bird.year$lat.nest==0 & nest.locations.bird.year$nest2>nest.locations.bird.year$nest1 & nest.locations.bird.year$lat.nest2!=0,c('lat.nest','lon.nest')] <- nest.locations.bird.year[nest.locations.bird.year$lat.nest==0 & nest.locations.bird.year$nest2>nest.locations.bird.year$nest1 & nest.locations.bird.year$lat.nest2!=0,c('lat.nest2','lon.nest2')]

# do this for all nest attempts, to see if this would change the existing values in lat.nest and lon.nest:
nest.locations.bird.year[,c('lat.nest.check','lon.nest.check')]<-c(NA,NA)
# when nest1>nest2
nest.locations.bird.year[nest.locations.bird.year$nest1>nest.locations.bird.year$nest2,c('lat.nest.check','lon.nest.check')] <- nest.locations.bird.year[nest.locations.bird.year$nest1>nest.locations.bird.year$nest2,c('lat.nest1','lon.nest1')]
# when nest2>nest1 and lat.nest2!=0 (meaning that nest2 was a real nesting attempt):
nest.locations.bird.year[nest.locations.bird.year$nest2>nest.locations.bird.year$nest1 & nest.locations.bird.year$lat.nest2!=0,c('lat.nest.check','lon.nest.check')] <- nest.locations.bird.year[nest.locations.bird.year$nest2>nest.locations.bird.year$nest1 & nest.locations.bird.year$lat.nest2!=0,c('lat.nest2','lon.nest2')]
nest.locations.bird.year$check <- nest.locations.bird.year$lat.nest==nest.locations.bird.year$lat.nest.check
# alles ziet er goed uit, behalve voor 6118 in 2014, die zou nooit op nest1 of nest2 gezeten hebben, terwijl lat.nest en lon.nest wel zijn ingevuld... vreemd... Dit blijkt te komen omdat er geen ACC-data is verzameld voor 6118 tijdens de ei- en kuikenfase, pas vanaf half augustus. 

# now fill in the correct coordinates for lat.nest and lon.nest
gps.breeding.data.behav <- merge(gps.breeding.data.behav[,names(gps.breeding.data.behav)%in%c('lat.nest','lon.nest')==F], nest.locations.bird.year[,c('birdID','year','attempt','lat.nest','lon.nest')])
# recalculate distance to nest:
gps.breeding.data.behav$distance.to.nest2 <- NA
gps.breeding.data.behav$distance.to.nest2[gps.breeding.data.behav$lat.nest!=0] <- round(distCosine(gps.breeding.data.behav[gps.breeding.data.behav$lat.nest!=0,c('longitude','latitude')], gps.breeding.data.behav[gps.breeding.data.behav$lat.nest!=0,c('lon.nest','lat.nest')], r=6378137),3) # gives the distance in meters between nest location and actual position of the bird; only calculate this when there is at least one row where lat.nest is known.
gps.breeding.data.behav$distance.to.nest2[is.na(gps.breeding.data.behav$distance.to.nest)]
plot(distance.to.nest2~distance.to.nest, gps.breeding.data.behav)

# remove the data where the habitat is unknown:
gps.breeding.data.behav$behaviour2 = gps.breeding.data.behav$behaviour
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour=='foraging'] = paste(gps.breeding.data.behav$behaviour[gps.breeding.data.behav$behaviour=='foraging'], gps.breeding.data.behav$habitat_salinity[gps.breeding.data.behav$behaviour=='foraging'], sep='_')
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$behaviour=='resting'] = paste(gps.breeding.data.behav$behaviour[gps.breeding.data.behav$behaviour=='resting'], gps.breeding.data.behav$habitat_type[gps.breeding.data.behav$behaviour=='resting'], sep='_')
gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$distance.to.nest2<5] = 'at_nest'
# TL 2022.05.26: eerder stond hier als nest1==1|nest2==1, die waren in determine.breeding.phases berekend als <5 meter van nest1 of nest2 af. Dit zou hetzelfde moeten geven als distance.to.nest<5, maar dat is blijkbaar niet zo, want ineens is het percentage nest attendance veel lager. zijn de nest coordinaten dan toch verkeerd toegekend? Ik ben nu de determine.breeding.phases functie opnieuw aan het runnen waarbij ik de kolommen nest1 en nest2 behoud, om te kijken wat hier fout gaat. 
#gps.breeding.data.behav$behaviour2[gps.breeding.data.behav$nest1==1|gps.breeding.data.behav$nest2==1] = 'at_nest'

table(gps.breeding.data.behav$behaviour2)

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

time.alloc.hr.sex.bird.phase = aggregate(duration~behaviour2+hour_CEST+breeding.phase2+sex+birdID, data=gps.breeding.data.behav, sum)
cols.behaviour2 = c('chartreuse4','darkolivegreen3','khaki','gold','plum2','red','lightblue','cadetblue','blue')

dur.behav.hr.xtabs = xtabs(duration~behaviour2+hour_CEST+sex+breeding.phase2, data=gps.breeding.data.behav)
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
barplot(dur.behav.hr.m.eggs[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('MALE', 3, 0.6, cex=1)
barplot(dur.behav.hr.f.eggs[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n', yaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('EGGS', 4, -0.5, cex=0.9)
mtext('FEMALE', 3, 0.6, cex=1)
barplot(dur.behav.hr.m.chicks[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n', cex.axis=1.3, border=NA, space=c(0.1,0.1))
barplot(dur.behav.hr.f.chicks[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1, xaxt='n', yaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('CHICKS', 4, -0.5, cex=.9)
barplot(dur.behav.hr.m.post[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1 , cex.axis=1.3, cex=1, border=NA, space=c(0.1,0.1))
barplot(dur.behav.hr.f.post[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2, las=1 ,yaxt='n' , cex.axis=1.3, cex=1, border=NA, space=c(0.1,0.1))
mtext('POST-BREEDING', 4, -0.5, cex=.9)
mtext('Time of day in hours (local time)', 1, 3, cex=1.2, outer=T)
mtext('Time allocation', 2, 3, cex=1.2, outer=T)
legend(29, 1, c('foraging marine', 'foraging brackish', 'foraging fresh', 'flying', 'other', 'resting tidal flats', 'resting mainland', 'resting island', 'nest attendance'), pch=15, col=rev(cols.behaviour2), bty='n', xpd=NA, cex=1.5)
dev.off()

### Now make the plot only for eggs and chicks phase, without habitat specification
cols.behaviour2.pooled = c('chartreuse4','gold','gold','gold','plum2','red','blue','blue','blue')
pdf("output/Fig2B.pdf",10,8)
# windows(10,8)
layout(matrix(1:6, byrow=T, ncol=2))
par(mar=c(1,1,1.2,0), oma=c(5,5,4,15))
barplot(dur.behav.hr.m.eggs[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2.pooled, las=1, xaxt='n', cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('MALE', 3, 0.6, cex=1)
barplot(dur.behav.hr.f.eggs[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2.pooled, las=1, xaxt='n', yaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('EGGS', 4, -0.5, cex=0.9)
mtext('FEMALE', 3, 0.6, cex=1)
barplot(dur.behav.hr.m.chicks[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2.pooled, las=1, xaxt='n', cex.axis=1.3, border=NA, space=c(0.1,0.1))
barplot(dur.behav.hr.f.chicks[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2.pooled, las=1, xaxt='n', yaxt='n' , cex.axis=1.3, border=NA, space=c(0.1,0.1))
mtext('CHICKS', 4, -0.5, cex=.9)
barplot(dur.behav.hr.m.post[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2.pooled, las=1 , cex.axis=1.3, cex=1, border=NA, space=c(0.1,0.1))
barplot(dur.behav.hr.f.post[c(1,8,7,9,6,2,4,3,5),], main='', col=cols.behaviour2.pooled, las=1 ,yaxt='n' , cex.axis=1.3, cex=1, border=NA, space=c(0.1,0.1))
mtext('POST-BREEDING', 4, -0.5, cex=.9)
mtext('Time of day in hours (local time)', 1, 3, cex=1.2, outer=T)
mtext('Time allocation', 2, 3, cex=1.2, outer=T)
legend(29, 0.6, c('foraging', 'flying', 'other', 'resting', 'nest attendance'), pch=15, col=rev(unique(cols.behaviour2.pooled)), bty='n', xpd=NA, cex=1.5)
dev.off()

### Investigate patterns of total time spent foraging per day: 
behaviour.bird.day.year <- table(gps.breeding.data.behav$yday_CEST, gps.breeding.data.behav$behaviour, paste(gps.breeding.data.behav$birdID, gps.breeding.data.behav$year))
behaviour.bird.day.year[,,"584 2013"]
gps.breeding.data.behav[gps.breeding.data.behav$birdID==584&gps.breeding.data.behav$year==2013&gps.breeding.data.behav$yday_CEST==209,]

foraging_duration_yday_bird_phase = aggregate(duration/60~year+birdID+yday_CEST+day_rel_hatching+week_rel_hatching, gps.breeding.data.behav[gps.breeding.data.behav$behaviour=='foraging',], sum)
foraging_duration_yday_bird_phase[foraging_duration_yday_bird_phase$birdID==584&foraging_duration_yday_bird_phase$year==2013&foraging_duration_yday_bird_phase$yday_CEST==209,]

names(foraging_duration_yday_bird_phase)[6] = 'dur_foraging_yday' # change column duration in to "dur_foraging_yday"
duration_yday_birdID_year <- duration_yday_birdID_year[duration_yday_birdID_year$breeding.phase!='1.exclude',]
duration_yday_birdID_year$breeding.phase <- factor(duration_yday_birdID_year$breeding.phase)
foraging_duration_yday_bird_phase = merge(duration_yday_birdID_year, foraging_duration_yday_bird_phase, all.x=T) # to make sure that all the days for each bird are in the data, also the days without foraging (the famous zero's)
foraging_duration_yday_bird_phase$dur_foraging_yday[is.na(foraging_duration_yday_bird_phase$dur_foraging_yday)] = 0 # change the NA's into zeros.

foraging_duration_yday_bird_phase = foraging_duration_yday_bird_phase[order(foraging_duration_yday_bird_phase$year, foraging_duration_yday_bird_phase$birdID, foraging_duration_yday_bird_phase$yday_CEST),]

### Remove data for which hatching dates were unknown:
foraging_duration_yday_bird_phase <- foraging_duration_yday_bird_phase[is.na(foraging_duration_yday_bird_phase$day_rel_hatching)==F,]
## Summary plot of foraging duration per breeding phase (pooling the sexes):
windows()
boxplot(dur_foraging_yday~breeding.phase, data=foraging_duration_yday_bird_phase, names=c('eggs', 'chicks','post +', 'post -'), col = breeding.phase.cols[3:6], ylab = 'Foraging duration per 24 h (hours)', cex.lab=1.3)

## same plot grouped per week:
for_dur_week_bird_year = aggregate(dur_foraging_yday~year+birdID+sex+week, foraging_duration_yday_bird_phase, mean)
for_dur_week_bird_year$sex.nr = ifelse(for_dur_week_bird_year$sex=='F',2,1)
## order birdyears so that females are plotted last:
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

## same plot grouped per week relative to hatching:
for_dur_week_rel_hatching_bird_year = aggregate(dur_foraging_yday~year+birdID+sex+week_rel_hatching, foraging_duration_yday_bird_phase, mean)
for_dur_week_rel_hatching_bird_year$sex.nr = ifelse(for_dur_week_rel_hatching_bird_year$sex=='F',2,1)

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

foraging_duration_yday_bird_phase$prop_for = foraging_duration_yday_bird_phase$dur_foraging_yday/foraging_duration_yday_bird_phase$dur_per_yday
foraging_duration_yday_bird_phase$logit_prop_for = qlogis(foraging_duration_yday_bird_phase$prop_for+min(foraging_duration_yday_bird_phase$prop_for[foraging_duration_yday_bird_phase$prop_for>0]))
# calculate average time spent foraging per bird per breeding phase (this should probably be the unit of the statistical analysis)
foraging_duration_phase_year_bird <- aggregate(prop_for~breeding.phase+year+birdID+sex, foraging_duration_yday_bird_phase, mean)

### plot the same graph as above, but now with proportion time spent foraging:
windows()
boxplot(prop_for~breeding.phase, data=foraging_duration_yday_bird_phase, names=c('eggs', 'chicks','post +', 'post -'), col=breeding.phase.cols[3:6], ylab='Proportion of time spent foraging', cex.lab=1.3)

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

############## FIGURE 3 ###############
pdf("output/Fig3.pdf",6,7.5)
#windows(8,10)
layout(1:2)
par(mar=c(1,1,0,0), oma=c(4,4,3,1))
# PANEL A: proportion of time spent foraging ~ sex + breeding phase
boxplot(prop_for~breeding.phase, data=foraging_duration_phase_year_bird[foraging_duration_phase_year_bird$sex=="F",], xaxt='n', col="lightcoral", medcol="coral4", whiskcol="coral4", outcol="coral4", staplecol="coral4", boxcol="coral4", xlab='', ylab='', boxwex=0.25, ylim=c(0.1,0.6), xlim=c(0.8,4.5), las=1, cex.axis=1)
boxplot(prop_for~breeding.phase, data=foraging_duration_phase_year_bird[foraging_duration_phase_year_bird$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", medcol="lightskyblue4", whiskcol="lightskyblue4", outcol="lightskyblue4", staplecol="lightskyblue4", boxcol="lightskyblue4", ylab='', at=1:4+0.3, boxwex=0.25, add=T)
mtext("Prop. time spent foraging",2,3,cex=1.2)
legend("topright", legend=c("females","males"), pch=22, pt.bg = c("lightcoral","lightskyblue"), cex=1)
# show sample sizes! 
N.prop.for.phase.sex <- table(foraging_duration_phase_year_bird$breeding.phase, foraging_duration_phase_year_bird$sex)
axis(3, at=1:4, N.prop.for.phase.sex[,1], tick=F, line=-0.5, cex.axis=1.1)
axis(3, at=1:4+0.3, N.prop.for.phase.sex[,2], tick=F, line=-0.5, cex.axis=1.1)
axis(3, at=0.7, "N =", tick=F, line=-0.5, cex.axis=1.1)
# PANEL B: proportion of foraging time in marine waters ~ sex + breeding phase
boxplot(prop_for_marine~breeding.phase, data=for_prop_marine_bird_year_phase[for_prop_marine_bird_year_phase$sex=="F",], xaxt='n', col="lightcoral", medcol="coral4", whiskcol="coral4", outcol="coral4", staplecol="coral4", boxcol="coral4", ylab='', boxwex=0.25, ylim=c(0,1), xlim=c(0.8,4.5), las=1, cex.axis=1, xlab="")
boxplot(prop_for_marine~breeding.phase, data=for_prop_marine_bird_year_phase[for_prop_marine_bird_year_phase$sex=="M",], xaxt='n', yaxt='n', col="lightskyblue", medcol="lightskyblue4", whiskcol="lightskyblue4", outcol="lightskyblue4", staplecol="lightskyblue4", boxcol="lightskyblue4", ylab='', at=1:4+0.3, boxwex=0.25, add=T)
axis(1, at=1:4+0.15, labels=c('eggs', 'chicks','post +', 'post -'), cex.axis=1.1)
mtext("Prop. marine foraging",2,3,cex=1.2)
mtext("Breeding phase",1,3,cex=1.2)
dev.off()

# Statistical analysis of Proportion of time spent foraging
foraging_duration_phase_year_bird$logit_prop_for <- qlogis(foraging_duration_phase_year_bird$prop_for)
m_for.dur.phasexsex = lme(logit_prop_for~breeding.phase*sex, data=foraging_duration_phase_year_bird, random=~1|birdID/year, method="ML") 
dredge(m_for.dur.phasexsex)
m_for.dur.phase_REML = lme(logit_prop_for~breeding.phase+sex, data=foraging_duration_phase_year_bird, random=~1|birdID/year, method="REML")
anova(m_for.dur.phase_REML)
summary(m_for.dur.phase_REML) 
lsmeans_for_dur_phase_sex = lsmeans(m_for.dur.phase_REML, pairwise~breeding.phase+sex) # within breeding phases, proportion of time spent foraging is higher in females than in males (p=0.0098). Proportion of time spent foraging decreases over the season, though the only pairwise comparison that was significant was between chick and post-breeding unsuccessful phase. # the most parsimonious model does not include the interaction between breeding phase and sex. However, perhaps it is true that in one of the breeding phases, the sexes significantly differ in time spent foraging (i.e. when caring for the eggs/chicks) but not during post-breeding?
m_for.dur.phasexsex_REML = lme(logit_prop_for~breeding.phase*sex, data=foraging_duration_phase_year_bird, random=~1|birdID/year, method="REML")
summary(m_for.dur.phasexsex_REML)
lsmeans_for_dur_phasexsex = lsmeans(m_for.dur.phasexsex_REML, pairwise~breeding.phase*sex) 
# when looking at the interaction model, only during the egg (p=0.0020) and chick phase (p=0.0378), females spend significantly more time foraging than males. 
# within females, there were no significant differences in time spent foraging between breeding phases
# within males, time spent foraging during chick phase was significant higher than during post-breeding unsuccessful phase. 


# Statistical analysis of proportion of foraging time spent in marine habitat
for_prop_marine_bird_year_phase$prop_for_marine[for_prop_marine_bird_year_phase$prop_for_marine==1] <- 1-min(for_prop_marine_bird_year_phase$prop_for_marine)
for_prop_marine_bird_year_phase$logit_prop_for_marine <- qlogis(for_prop_marine_bird_year_phase$prop_for_marine)
m_for.marine.phasexsex = lme(logit_prop_for_marine~breeding.phase*sex, data=for_prop_marine_bird_year_phase, random=~1|birdID/year, method="ML") 
dredge(m_for.marine.phasexsex)
m_for.marine.phase.sex_REML = lme(logit_prop_for_marine~breeding.phase+sex, data=for_prop_marine_bird_year_phase, random=~1|birdID/year, method="REML")
summary(m_for.marine.phase.sex_REML)
lsmeans_for_marine_phase_sex = lsmeans(m_for.marine.phase.sex_REML, pairwise~breeding.phase+sex) # females forage significantly more in marine habitats than males. The proportion of time spent foraging in marine habitat was significantly higher during the egg phase than during the post-breeding phase (either success- or unsuccessful).

# the most parsimonious model does not include the interaction between breeding phase and sex. however, perhaps it is true that in one of the breeding phases, the sexes significantly differ in time spent foraging (i.e. when caring for the eggs/chicks) but not during post-breeding?
m_for.marine.phasexsex_REML = lme(logit_prop_for_marine~breeding.phase*sex, data=for_prop_marine_bird_year_phase, random=~1|birdID/year, method="REML")
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
# redo analysis to see if results change... however, then what should we do for the walking points in water? assume they were foraging? this is rather arbitrary, as birds may actually be walking in water as well....

