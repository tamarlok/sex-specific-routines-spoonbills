# Plot shapefile of study area
schier_new84_sel <- readOGR(dsn = "data/study_area_shapefile/study_area_shapefile.shp")

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
map_NL <- readPNG("data/map_NL_greyscale.png")

# nest locations of birds used in the analysis are in gps.breeding.data.behav:
nest.locations.used <- na.omit(unique(gps.breeding.data.behav[,c('birdID','year','lat.nest','lon.nest')]))

############# FIGURE 1 ######################
postscript("output/Fig1.eps",width=11,height=6)
# windows(11,6)
par(oma=c(0,0,1,14), mar=c(3,3,0,0))
plot(schier_new84_sel, axes=TRUE, border=NA, col = ColHabitats[schier_new84_sel$HabitatNr], xlim=c(5.9,6.6), ylim=c(53.30,53.57))
legend(6.66,53.56, habitat_types$HabitatsMapped, pch=22, pt.bg=habitat_types$ColHabitats, xpd=NA)
DrawEllipse(x=mean(nest.locations.used$lon.nest), y=mean(nest.locations.used$lat.nest), radius.x=0.023, radius.y=0.007, rot=0.19, col=NA, border='red', lwd=2)
rasterImage(map_NL, 6.65, 53.3, 6.8, 53.4, xpd=NA)
polygon(c(6.745,6.767,6.767,6.745),c(53.385,53.385,53.395,53.395), xpd=NA, col=NA, border='blue')
# add scale:
lines(c(6.48, 6.56), c(53.32, 53.32), lwd=2)
lines(c(6.48, 6.48), c(53.318, 53.322), lwd=2)
lines(c(6.56, 6.56), c(53.318, 53.322), lwd=2)
text(6.52,53.327,"5 km", cex=1.3)
# end of adding scale
dev.off()
############ END FIGURE 1 ##################

############ FIGURE 1 in TIFF ###############
tiff("output/Fig1.tiff",width=3520,height=1920)
par(oma=c(0,0,1,11), mar=c(3,3,0,0))
plot(schier_new84_sel, axes=TRUE, border=NA, col = ColHabitats[schier_new84_sel$HabitatNr], xlim=c(5.9,6.6), ylim=c(53.30,53.57))
legend(6.66,53.56, habitat_types$HabitatsMapped, pch=22, pt.bg=habitat_types$ColHabitats, xpd=NA)
DrawEllipse(x=mean(nest.locations.used$lon.nest), y=mean(nest.locations.used$lat.nest), radius.x=0.03, radius.y=0.007, rot=0.19, col=NA, border='red', lwd=2)
rasterImage(map_NL, 6.65, 53.3, 6.8, 53.4, xpd=NA)
dev.off()
########### END FIGURE 1 in TIFF ############

rm(schier_new84_sel) # to save memory

## plot the breeding phases and days with nearly complete data (47-48 samples per bird) over the season per bird and per year:

# number of birddays per bird with nearly complete data:
gps.breeding.data.behav$freq=1
N_yday_birdID_year_sel <- aggregate(freq~year+birdID+sex+yday_CEST+breeding.phase+breeding.phase.nr, gps.breeding.data.behav, sum) 
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
pdf("output/FigS1.pdf", height=8, width=8)
#windows(8,8)
par(mar=c(1,1,0.5,0), oma=c(2.5,6,0.5,8))
plot(N_yday_birdID_year_sel$yday_CEST, 1:dim(N_yday_birdID_year_sel)[1], ylim=c(1,dim(birdyears)[1]), type='n', xlab='', ylab='', xaxt='n', yaxt='n')
axis(1, at=c(91,121,152,182,213,244,274), labels=F)
axis(1, at=c(91,121,152,182,213,244,274)+15, line=-1, labels=c('Apr','May','Jun','Jul','Aug','Sep','Oct'), cex.axis=0.8, tick=F)
axis(2, dim(birdyears)[1]:1, paste(birdyears$sex, birdyears$birdID, birdyears$year), las=1, cex.axis=0.7)
legend(245,10,c('pre-breeding','incubation', 'chick-rearing','post-breeding +', 'post-breeding -'), pch=19, col=breeding.phase.cols[c(1,3:6)],bty='n', xpd=NA, cex=1)
mtext('Day of the year', 1, 1, cex=1, outer=T)
mtext('Birdyear', 2, 4, cex=1, outer=T)
for (i in 1:dim(birdyears)[1]) {
  birdline=dim(birdyears)[1]+1-i
  bird = birdyears$birdID[i]
  year = birdyears$year[i]
  df = N_yday_birdID_year_sel[N_yday_birdID_year_sel$birdID==bird&N_yday_birdID_year_sel$year==year,]
  for (j in unique(df$breeding.phase.nr)) points(df$yday_CEST[df$breeding.phase.nr==j], rep(birdline, dim(df[df$breeding.phase.nr==j,])[1]), col=breeding.phase.cols[j], pch=15, cex=0.8)
}
dev.off()
############# END FIGURE S1 ########################

### Make density map of foraging locations of males and females:
# round longitude to nearest 0.005
gps.breeding.data.behav$lon.rnd <- round(gps.breeding.data.behav$longitude/0.005)*0.005
# round latitude to nearest 0.002
gps.breeding.data.behav$lat.rnd <- round(gps.breeding.data.behav$latitude/0.003)*0.003
gps.breeding.data.behav$breeding.phase3 <- substr(as.character(gps.breeding.data.behav$breeding.phase),3,15)
foraging.data.per.coordrnd <- aggregate(foraging~lat.rnd+lon.rnd+sex+breeding.phase+breeding.phase3, gps.breeding.data.behav[gps.breeding.data.behav$foraging==1,], sum)
foraging.data.per.coordrnd$breeding.phase2 = as.character(foraging.data.per.coordrnd$breeding.phase)
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

p <- OpenStreetMap::autoplot.OpenStreetMap(schiermap) + 
  geom_tile(data = foraging.data.per.coordrnd.merc, aes( x = x, y = y, fill=foraging)) +
  scale_fill_gradient(low = "yellow", high = "red", trans="log", labels=c(1,7,55,403), name="density") + 
  facet_grid(breeding.phase2~sex, labeller = labeller(breeding.phase2 = phase.labs, sex = sex.labs)) +
  theme(legend.position = c(1.15,0.15),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        strip.text.y.right = element_text(angle=90, vjust=1)) 

ggsave(file=paste("output/FigS4_foraging.maps.pdf",sep=""), plot = p, width=15, height=14, units="cm")
