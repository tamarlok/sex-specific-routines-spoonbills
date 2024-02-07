source("functions.R") # to have the most updated version of the functions

# plot the shape file
load("data/raw/ShapeFileSchier.RData")
ColHabitats = c('grey60','forestgreen','darkolivegreen','lightblue','lightblue','darkblue',
                'cyan','darkolivegreen2','cyan','forestgreen','lightblue','cyan','blue',
                'blue','lightgoldenrod3','darkolivegreen2','forestgreen','darkolivegreen','lightblue','lightblue')
mapped_habitats <- unique(as.data.frame(schier_new84_sel)[,c('HabitatNr','Habitat')])
mapped_habitats <- mapped_habitats[order(mapped_habitats$HabitatNr),]
mapped_habitats$ColHabitats <- ColHabitats
mapped_habitats$HabitatsMapped <- c("island","land","marsh","water (fresh)","water (fresh)","North Sea","water (brackish)","saltmarsh","water (brackish)","land","water (fresh)","water (brackish)","water (marine)","water (marine)","mudflats","saltmarsh","land","marsh","water (fresh)","water (fresh)") 
mapped_habitats <- unique(mapped_habitats[,c("HabitatsMapped","ColHabitats")])
mapped_habitats <- mapped_habitats[c(2,3,1,7,9,5,8,6,4),] # arranged in the order of plotting the legend

# manually create legend for Figure 1:
habitat_types <- data.frame(HabitatsMapped=c("North Sea","tidal gullies (marine)","tidal flats (marine)", "brackish water", "freshwater", "land", "saltmarsh", "marsh", "other islands"),
                            ColHabitats=c("darkblue","blue","lightgoldenrod3","cyan","lightblue","forestgreen","darkolivegreen2","darkolivegreen","grey60"))
map_NL <- readPNG("data/raw/map_NL_with_study_area.png")

# nest locations of birds used in the analysis are in gps.breeding.data.behav:
nest.locations.used <- na.omit(unique(gps.breeding.data.behav[,c('birdID','year','lat.nest','lon.nest')]))

############# FIGURE 1 ######################
pdf("output/Fig1.pdf",11,6)
#windows(11,6)
par(oma=c(0,0,1,11), mar=c(3,3,0,0))
plot(schier_new84_sel, axes=TRUE, border=NA, col = ColHabitats[schier_new84_sel$HabitatNr], xlim=c(5.9,6.6), ylim=c(53.30,53.57))
legend(6.66,53.56, habitat_types$HabitatsMapped, pch=22, pt.bg=habitat_types$ColHabitats, xpd=NA)
#points(lat.nest~lon.nest, nest.locations.used, col='red')
DrawEllipse(x=mean(nest.locations.used$lon.nest), y=mean(nest.locations.used$lat.nest), radius.x=0.03, radius.y=0.007, rot=0.19, col=NA, border='red', lwd=2)
rasterImage(map_NL, 6.65, 53.3, 6.8, 53.4, xpd=NA)
dev.off()
############ END FIGURE 1 ##################

rm(schier_new84_sel)

## plot the breeding phases (excluding the unspecified 'breeding') and days with complete data (48 samples per bird) over the season per bird and per year:
table(gps.breeding.data.behav$birdID)
table(gps.breeding.data.behav$breeding.phase)

# number of birddays per bird with nearly complete data:
table(N_yday_birdID_year_sel$birdID)
table(N_yday_birdID_year_sel$birdID, N_yday_birdID_year_sel$sex) # 9 females; 13 males
n_years_bird = table(N_yday_birdID_year_sel$birdID, N_yday_birdID_year_sel$year) 
n_years_bird[n_years_bird>1]<-1
table(rowSums(n_years_bird)) # 11 individuals with 1 year of data, 5 with 2 years, 4 with 3 years and 2 with 4 years
dim(N_yday_birdID_year_sel) # 3115 bird days

birdyears = unique(N_yday_birdID_year_sel[,c('birdID','year','sex')]) 
dim(birdyears) # 41 bird years
birdyears$birdID <- as.numeric(as.character(birdyears$birdID))
birdyears = birdyears[order(birdyears$sex, birdyears$birdID, birdyears$year),]

# colours for the different breeding.phases for plotting
breeding.phase.cols = c('grey','white','darkorange','dodgerblue','darkolivegreen3','red')

################# FIGURE S1 ##########################
pdf("output/FigS1.pdf", height=6, width=8)
#windows(8,6)
par(mar=c(1,1,0.5,0), oma=c(2.5,4.5,0.5,10))
plot(N_yday_birdID_year_sel$yday_CEST, 1:dim(N_yday_birdID_year_sel)[1], ylim=c(1,dim(birdyears)[1]), type='n', xlab='', ylab='', xaxt='n', yaxt='n')
axis(1, at=c(91,121,152,182,213,244,274), labels=F)
axis(1, at=c(91,121,152,182,213,244,274)+15, line=-1, labels=c('Apr','May','Jun','Jul','Aug','Sep','Oct'), cex.axis=0.8, tick=F)
axis(2, dim(birdyears)[1]:1, paste(birdyears$sex, birdyears$birdID, birdyears$year), las=1, cex.axis=0.5)
legend(250,10,c('pre-breeding','incubation', 'chick-rearing','post-breeding +', 'post-breeding -'), pch=19, col=breeding.phase.cols[c(1,3:6)],bty='n', xpd=NA, cex=1)
mtext('Day of the year', 1, 1, cex=1, outer=T)
mtext('Birdyear', 2, 3, cex=1, outer=T)
for (i in 1:dim(birdyears)[1]) {
  birdline=dim(birdyears)[1]+1-i
  bird = birdyears$birdID[i]
  year = birdyears$year[i]
  df = N_yday_birdID_year_sel[N_yday_birdID_year_sel$birdID==bird&N_yday_birdID_year_sel$year==year,]
  for (j in unique(df$breeding.phase.nr)) points(df$yday_CEST[df$breeding.phase.nr==j], rep(birdline, dim(df[df$breeding.phase.nr==j,])[1]), col=breeding.phase.cols[j], pch=15, cex=0.5)
}
dev.off()
############# END FIGURE S1 ########################

# check if gaps are correctly visualised in the figure. Example 6298 in 2018. It seems that 1 day is missing during incubation and chick-rearing, and two adjacent days during post-breeding. 
df.6298.2018 = N_yday_birdID_year_sel[N_yday_birdID_year_sel$birdID==6298 & N_yday_birdID_year_sel$year==2018,]
# missing days: eggs (150,151,156), chicks (170,171,176) and postbreeding (187,188,189,191,197,200)

### FIGURE S2 parts ###
# Make some example graphs of changes in daily rhythm of nest attendance over the season (also plotting the rhythm of the tide)
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
# plot nest attendance during egg incubation and chick rearing for one female and male
gps.breeding.data <- gps.breeding.data[order(gps.breeding.data$birdID, gps.breeding.data$year, gps.breeding.data$yday_CEST),]
df.6066.2017 <- gps.breeding.data[gps.breeding.data$birdID==6066&gps.breeding.data$year==2017,]
df.6066.2017.eggs <- df.6066.2017[df.6066.2017$breeding.phase=="3.eggs",]
df.6066.2017.chicks <- df.6066.2017[df.6066.2017$breeding.phase=="4.chicks",]
plot.nest.attendance(df.6066.2017.eggs, breeding.phase="incubation") # manually save pdf
plot.nest.attendance(df.6066.2017.chicks, breeding.phase="chick-rearing") # manually save pdf
df.6284.2017 <- gps.breeding.data[gps.breeding.data$birdID==6284.2&gps.breeding.data$year==2017,]
df.6284.2017.eggs <- df.6284.2017[df.6284.2017$breeding.phase=="3.eggs",]
df.6284.2017.chicks <- df.6284.2017[df.6284.2017$breeding.phase=="4.chicks",]
plot.nest.attendance(df.6284.2017.eggs, breeding.phase="incubation") # manually save pdf
plot.nest.attendance(df.6284.2017.chicks, breeding.phase="chick-rearing") # manually save pdf
### END OF FIGURE S2 ###

### analyse PERIODICITY of nest attendance (a la Bulla 2016); DO I USE THIS?
df.6284.2017.eggs$date_time_10min <- round_date(df.6284.2017.eggs$date_time, unit="10 min")
# select maximum nest attendance at each 10 minute measurement:
df.6284.at.nest <- aggregate(nest1~date_time_10min, df.6284.2017.eggs,max) # using column nest1 does not always work; in gps.breeding.data however, the current nest coordinates are not yet determined. But in the time allocation script, days with <23.5 hour of duration data are removed, which is not desirable for the periodicity analysis. 
# link to complete 10 minute dataframe with corresponding dates:
dates_uni <- unique(round_date(df.6284.2017.eggs$date_time, unit="day"))
date_time_uni <- expand.grid(date=as.character(dates_uni), hour=0:23, minute=c(0,10,20,30,40,50),second=0)
date_time_uni$hms <- paste(date_time_uni$hour, date_time_uni$minute, date_time_uni$second, sep=":")
date_time_uni$date_time_10min <- ymd_hms(paste(date_time_uni$date, date_time_uni$hms, sep=" "), tz="Europe/Amsterdam")
date_time_uni <- date_time_uni[order(date_time_uni$date_time_10min),]
datetimes <- data.frame(date_time_10min = date_time_uni$date_time_10min)
df.6284.at.nest.NA <- merge(datetimes, df.6284.at.nest, all.x=T)
# fill the NA's if the preceding and subsequent 10 minutes have the same value:
for (i in 2:(dim(df.6284.at.nest.NA)[1]-1)) {
  if (is.na(df.6284.at.nest.NA$nest1[i])==T & is.na(df.6284.at.nest.NA$nest1[i-1])==F & is.na(df.6284.at.nest.NA$nest1[i+1])==F) 
    if (df.6284.at.nest.NA$nest1[i-1] == df.6284.at.nest.NA$nest1[i+1]) df.6284.at.nest.NA$nest1[i] <- df.6284.at.nest.NA$nest1[i-1] 
}

head(df.6284.2017.eggs)
hist(df.6284.2017.eggs$duration)
df.6284.2017.chicks <- df.6284.2017[df.6284.2017$breeding.phase=="4.chicks",]

nest.attendance <- ts(df.6284.at.nest.NA$nest1, start=c(yday(df.6284.at.nest.NA$date_time_10min[1]),1), frequency=24*6)
acf(nest.attendance, type="correlation", na.action='na.pass')

acf(x=c(1,2,7,8), type="correlation", na.action='na.pass')
# REMOVE LINES 114-139


### Make density map of foraging locations of males and females:
# round longitude to nearest 0.005
gps.breeding.data.behav$lon.rnd <- round(gps.breeding.data.behav$longitude/0.005)*0.005
# round latitude to nearest 0.002 (which produces more or less squares on the map)
gps.breeding.data.behav$lat.rnd <- round(gps.breeding.data.behav$latitude/0.003)*0.003
gps.breeding.data.behav$breeding.phase3 <- substr(as.character(gps.breeding.data.behav$breeding.phase),3,15)
foraging.data.per.coordrnd <- aggregate(foraging~lat.rnd+lon.rnd+sex+breeding.phase+breeding.phase3, gps.breeding.data.behav[gps.breeding.data.behav$foraging==1,], sum)
foraging.data.per.coordrnd$breeding.phase2 = foraging.data.per.coordrnd$breeding.phase
foraging.data.per.coordrnd$breeding.phase2[foraging.data.per.coordrnd$breeding.phase2%in%c("5.post.breeding.successful","6.post.breeding.unsuccessful")] = '5.post.breeding'
lonmin = min(foraging.data.per.coordrnd$lon.rnd)
lonmax = max(foraging.data.per.coordrnd$lon.rnd)
latmin = min(foraging.data.per.coordrnd$lat.rnd)
latmax = max(foraging.data.per.coordrnd$lat.rnd)
schiermap <- openmap(c(latmin,lonmin-0.02),c(latmax,lonmax+0.02),type="bing")

# transpose the foraging points to mercator projection
df.foraging.per.coord.merc <- as.data.frame( 
  OpenStreetMap::projectMercator( lat = foraging.data.per.coordrnd$lat.rnd, 
                                  long = foraging.data.per.coordrnd$lon.rnd ) 
)
foraging.data.per.coordrnd.merc <- cbind( foraging.data.per.coordrnd, df.foraging.per.coord.merc)
hist(foraging.data.per.coordrnd.merc$foraging[foraging.data.per.coordrnd.merc$foraging<100], breaks=100)
# for better visualization, remove the locations with only a single foraging point (maybe don't do this for the 30 min data?)
#foraging.data.per.coordrnd.merc <- foraging.data.per.coordrnd.merc[foraging.data.per.coordrnd.merc$foraging>1,]
plot(y~x, foraging.data.per.coordrnd.merc)

# get mercator locations for axes:
min(foraging.data.per.coordrnd.merc$x)
grid.locations <- expand.grid(longitude=seq(floor(lonmin*10)/10,ceiling(lonmax*10)/10,0.1), latitude=seq(53.30,53.50,0.05))
grid.locations.merc <- as.data.frame( 
  OpenStreetMap::projectMercator( lat = grid.locations$latitude, 
                                  long = grid.locations$longitude ) 
)
grid.locations.merc <- cbind( grid.locations, grid.locations.merc)
grid.x <- unique(grid.locations.merc[,c("x","longitude")])
grid.y <- unique(grid.locations.merc[,c("y","latitude")])

phase.labs = c('pre-breeding','incubation','chick-rearing','post-breeding')
names(phase.labs) = c("1.pre-breeding","3.eggs","4.chicks","5.post.breeding")
sex.labs = c("female","male")
names(sex.labs) = c("F","M")

windows(9,8)
p <- OpenStreetMap::autoplot.OpenStreetMap(schiermap) + 
  geom_tile(data = foraging.data.per.coordrnd.merc, aes( x = x, y = y, fill=foraging)) +
  theme(legend.position = c(1.15,0.15),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) + 
  scale_fill_gradient(low = "yellow", high = "red", trans="log", labels=c(1,7,55,403), name="density") + 
  facet_grid(breeding.phase2~sex, labeller = labeller(breeding.phase2 = phase.labs, sex = sex.labs))
p
ggsave(file=paste("output/FigS4_foraging.maps.pdf",sep=""), plot = p, width=15, height=14, units="cm")
ggsave(file=paste("output/FigS4_foraging.maps.png",sep=""), plot = p, width=15, height=14, units="cm")
