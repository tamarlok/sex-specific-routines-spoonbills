source("functions.R") # to have the most updated version of the functions

# plot the shape file
load("data/raw/ShapeFileSchier.RData")
ColHabitats = c('grey60','forestgreen','darkolivegreen','lightblue','lightblue','darkblue',
                'cyan','darkolivegreen2','cyan','forestgreen','lightblue','cyan','blue',
                'blue','lightgoldenrod3','darkolivegreen2','forestgreen','darkolivegreen','lightblue','lightblue')
mapped_habitats <- unique(as.data.frame(schier_new84_sel)[,c('HabitatNr','Habitat','habitat')])
mapped_habitats <- mapped_habitats[order(mapped_habitats$HabitatNr),]
mapped_habitats$ColHabitats <- ColHabitats
mapped_habitats$HabitatsMapped <- c("island","land","marsh","water (fresh)","water (fresh)","North Sea","water (brackish)","saltmarsh","water (brackish)","land","water (fresh)","water (brackish)","water (marine)","water (marine)","mudflats","saltmarsh","land","marsh","water (fresh)","water (fresh)") 
mapped_habitats <- unique(mapped_habitats[,c("HabitatsMapped","ColHabitats")])
mapped_habitats <- mapped_habitats[c(2,3,1,7,9,5,8,6,4),] # arranged in the order of plotting the legend

# manually create legend for Figure 1:
habitat_types <- data.frame(HabitatsMapped=c("North Sea","tidal gullies (marine)","tidal flats (marine)", "brackish water", "freshwater", "land", "saltmarsh", "marsh", "other islands"),
                            ColHabitats=c("darkblue","blue","lightgoldenrod3","cyan","lightblue","forestgreen","darkolivegreen2","darkolivegreen","grey60"))

############# FIGURE 1 ######################
pdf("output/Fig1.pdf",11,6)
#windows(11,6)
par(oma=c(0,0,1,11), mar=c(3,3,0,0))
plot(schier_new84_sel, axes=TRUE, border=NA, col = ColHabitats[schier_new84_sel$HabitatNr], xlim=c(5.9,6.6), ylim=c(53.30,53.57))
legend(6.66,53.56, habitat_types$HabitatsMapped, pch=22, pt.bg=habitat_types$ColHabitats, xpd=NA)
dev.off()
############ END FIGURE 1 ##################

rm(schier_new84_sel)

load("data/processed/gps.breeding.data.0525.RData")
load("data/processed/gps.breeding.data.behav.0525.RData")

## plot the breeding periods (excl. (pre-)breeding as breeding.phase) and days with sufficient amount of data (>23.5h of duration data with positions on the habitatmap) over the season per bird and per year:
table(gps.breeding.data$birdID)
duration_yday_birdID_year_all <- aggregate(duration/60~year+birdID+sex+yday_CEST+week+breeding.phase+breeding.phase.nr, gps.breeding.data[gps.breeding.data$breeding.phase.nr>2,], sum) 
birdyears = unique(duration_yday_birdID_year_all[,c('birdID','year','sex')])
birdyears$birdID <- as.numeric(as.character(birdyears$birdID))
birdyears = birdyears[order(birdyears$year, birdyears$birdID),]

table(birdyears$year, birdyears$sex) # chick bird data per year
unique(birdyears[order(birdyears$sex),c(1,3)])
table(ydays.year.bird.phase.sel$birdID, ydays.year.bird.phase.sel$breeding.phase.nr, ydays.year.bird.phase.sel$year) # check breeding stage data per bird per year

# colours for the different breeding.phases for plotting
breeding.phase.cols = c('grey','white','darkorange','dodgerblue','darkolivegreen3','red')

################# FIGURE S1 ##########################
pdf("output/FigS1.pdf", height=6, width=7)
#windows(6,7)
#par(mar=c(4,6,0.5,10))
par(mar=c(1,1,0.5,0), oma=c(2.5,4.5,0.5,10))
plot(duration_yday_birdID_year_all$yday_CEST, 1:dim(duration_yday_birdID_year_all)[1], ylim=c(1,dim(birdyears)[1]), type='n', xlab='', ylab='', xaxt='n', yaxt='n')
axis(1, at=c(91,121,152,182,213,244,274), labels=F)
axis(1, at=c(91,121,152,182,213,244,274)+15, line=-1, labels=c('Apr','May','Jun','Jul','Aug','Sep','Oct'), cex.axis=0.8, tick=F)
axis(2, dim(birdyears)[1]:1, paste(birdyears$birdID, birdyears$year), las=1, cex.axis=0.5)
legend(300,8,c('eggs', 'chicks','post-breeding +', 'post-breeding -'), pch=19, col=breeding.phase.cols[c(3:6)],bty='n', xpd=NA, cex=1)
mtext('Day of the year', 1, 1, cex=1, outer=T)
mtext('Bird-ID per year', 2, 3, cex=1, outer=T)
for (i in 1:dim(birdyears)[1]) {
  birdline=dim(birdyears)[1]+1-i
  bird = birdyears$birdID[i]
  year = birdyears$year[i]
  df = duration_yday_birdID_year_all[duration_yday_birdID_year_all$birdID==bird&duration_yday_birdID_year_all$year==year,]
  for (j in unique(df$breeding.phase.nr)) points(df$yday_CEST[df$breeding.phase.nr==j], rep(birdline, dim(df[df$breeding.phase.nr==j,])[1]), col=breeding.phase.cols[j], pch=15, cex=0.5)
}
dev.off()
############# END FIGURE S1 ########################

### check 656-2014 data: no chick phase, while hatchdate was provided... 

### we could instead make this figure with duration_yday_birdID_year, showing the ydays for which there is >23.5 hours of gps/acc/habitat-data with intervals of <=30 minutes.

# Make some example graphs of changes in daily rhythm of nest attendance over the season (also plotting the rhythm of the tide)
# plot nest attendance during egg incubation and chick rearing for one female and male
# import tide data
tide_data <- read.table("data/raw/getij_schiermonnikoog_2010-2019.txt", skip=4)
names(tide_data) <- c("date","time","lowhigh", "waterheight")
tide_data$date_time <- dmy_hm(paste(tide_data$date, tide_data$time, sep=" "))
tide_data$date_time_wintertime <- force_tz(tide_data$date_time, tzone="Etc/GMT-1")
tide_data$date_time_UTC <- with_tz(tide_data$date_time_wintertime, tzone="UTC")
tide_data$date_time_summertime <- with_tz(tide_data$date_time_wintertime, tzone="Etc/GMT-2")
tide_data$yday_CEST <- yday(tide_data$date_time_summertime)
tide_data$hour_CEST <- hour(tide_data$date_time_summertime)
tide_data$min_CEST <- minute(tide_data$date_time_summertime)
tide_data$year <- year(tide_data$date_time_summertime)
tide_data$day_min <- tide_data$hour_CEST*60+tide_data$min_CEST
high_tides <- data.frame(tide_data[tide_data$lowhigh==1,])
high_tides <- high_tides[,c("year","yday_CEST", "waterheight","day_min")]
low_tides <- data.frame(tide_data[tide_data$lowhigh==2,])
low_tides <- low_tides[,c("year","yday_CEST", "waterheight","day_min")]
#
df.6066.2017 <- gps.breeding.data.sel[gps.breeding.data.sel$birdID==6066&gps.breeding.data.sel$year==2017,]
df.6066.2017.eggs <- df.6066.2017[df.6066.2017$breeding.phase=="3.eggs",]
df.6066.2017.chicks <- df.6066.2017[df.6066.2017$breeding.phase=="4.chicks",]
plot.nest.attendance(df.6066.2017.eggs, breeding.phase="egg incubation")
plot.nest.attendance(df.6066.2017.chicks, breeding.phase="chick rearing")
df.6284.2017 <- gps.breeding.data.sel[gps.breeding.data.sel$birdID==6284.2&gps.breeding.data.sel$year==2017,]
df.6284.2017.eggs <- df.6284.2017[df.6284.2017$breeding.phase=="3.eggs",]
df.6284.2017.chicks <- df.6284.2017[df.6284.2017$breeding.phase=="4.chicks",]
plot.nest.attendance(df.6284.2017.eggs, breeding.phase="egg incubation")
plot.nest.attendance(df.6284.2017.chicks, breeding.phase="chick rearing")
