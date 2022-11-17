# load best-supported models (and data on which these models were trained, using the function RF.model.start) from ACC paper: 
load("data/raw/RF.models.RData")
rm(dfs.fixed.0.4, dfs.flex.100)

# link gps and acc data, while keeping it a list to reduces the required RAM. 
# add column with BirdID to each item in the list, adjust Index to start at 0 and sort on date_time and Index (for the segmentation code to work properly)
gps.acc.data.list <- list()
for (i in 1:length(gps.data.list)) {
  device <- gps.data.list[[i]]$device_info_serial[1]
  print(device)
  if (is.null(acc.data.list[[i]])==F) { # only possible to combine gps and acc data for those birds where acc data is available
    gps.acc.data.list[[i]] <- link.gps.acc.data(gps.data=na.omit(gps.data.list[[i]]), acc.data=na.omit(acc.data.list[[i]]), device.info=device.infos[device.infos$device_info_serial==device,])
    gps.acc.data.list[[i]]$birdID <- bird.data$birdID[i]
    gps.acc.data.list[[i]]$Index <- gps.acc.data.list[[i]]$index-1 # for the segmentation code to work properly, Index should start at 0
    gps.acc.data.list[[i]] <- gps.acc.data.list[[i]][order(gps.acc.data.list[[i]]$date_time, gps.acc.data.list[[i]]$Index),]
    # empty the just linked gps and acc data from the separate lists to save memory:
    gps.data.list[[i]] <- NA
    acc.data.list[[i]] <- NA
  }
}
rm(gps.data.list, acc.data.list)

# do the segmentation and prediction of behaviour for each bird separately before making it a df, to save memory space
# running on the NIOZ cluster on 06.04.2022
seg.df.list.fixed.0.4 <- list()
for (i in 1:length(gps.acc.data.list)) {
  if (is.null(gps.acc.data.list[[i]])==F) { # only run if list element is not empty
    data = gps.acc.data.list[[i]]
    print(data$birdID[1])
    data$obs.id <- paste(data$birdID, as.numeric(data$date_time), sep = ".")
    seg.df <- create.fixed.segments(segment.length=0.4, data, annotated.data=F, naomit=F)
    seg.df[[1]]$pred.behav <- predict(RF.fixed.0.4, seg.df[[1]])
    seg.df.list.fixed.0.4[[i]] <- merge(unique(seg.df[[2]][,c("birdID","date_time","altitude","latitude","longitude","speed_2d","segment.id.cut")]), seg.df[[1]])
  }
}

# change from list to dataframe: 
df.all.fixed.0.4 <- from.list.to.df(seg.df.list.fixed.0.4)
keep(df.all.fixed.0.4, sure=T)
save.image("data/processed/gps.acc.data.predicted.behaviour.1904.RData") 

# determine the most expressed behaviour per GPS location:
df.all.fixed.0.4 <- df.all.fixed.0.4[order(df.all.fixed.0.4$birdID, df.all.fixed.0.4$date_time),]
# remove NA's from predicted behaviour file 
df.behav.NA <- df.all.fixed.0.4[is.na(df.all.fixed.0.4$pred.behav),]
table(is.na(df.behav.NA$kurt.z)) # the cases where behaviour could not predicted (N=293962) had NA for kurtosis and skewness of the z-axis. These all concerned old transmitters.
# remove these NA's

df.pred.behav.sel <- na.omit(df.all.fixed.0.4[,c('birdID','date_time','odba','pred.behav')])
rm(df.all.fixed.0.4)
# select the most occurring behaviour per birdID & date_time combination
# first pool behavioural categories
df.pred.behav.sel$behaviour <- as.character(df.pred.behav.sel$pred.behav)
df.pred.behav.sel$behaviour[df.pred.behav.sel$pred.behav%in%c("for-handle","for-intake","for-search")] = "foraging"
df.pred.behav.sel$behaviour[df.pred.behav.sel$pred.behav%in%c("walk","drink")] = "other"
df.pred.behav.sel$behaviour[df.pred.behav.sel$pred.behav%in%c("sit","stand")] = "resting"
df.pred.behav.sel$behaviour[df.pred.behav.sel$pred.behav%in%c("fly-flap","fly-soar")] = "flying"
# select the most often classified behaviour per GPS fix (as there are often multiple segments (2-4) of 0.4 s per GPS fix)
df.pred.behav.sel$freq <- 1
birdID.datetime.behaviours <- aggregate(freq~birdID+date_time+behaviour, df.pred.behav.sel, sum)
birdID.datetime.behav.max <- aggregate(freq~birdID+date_time, birdID.datetime.behaviours, max) # the number of segments that the most often classified behaviour is classified
birdID.datetime.behavdom <- merge(birdID.datetime.behaviours, birdID.datetime.behav.max, by=c('birdID','date_time','freq')) # behaviour that is expressed most often. This could still be more than one, if so, then randomly chose on of these behaviours: 
birdID.datetime.behavdom$rnd <- runif(dim(birdID.datetime.behavdom)[1])
birdID.datetime.rndmax <- aggregate(rnd~birdID+date_time, birdID.datetime.behavdom, max)
birdID.datetime.behavsel <- merge(birdID.datetime.behavdom, birdID.datetime.rndmax, by=c("birdID","date_time","rnd"))[,c("birdID","date_time","behaviour")]
table(birdID.datetime.behavsel$birdID, year(birdID.datetime.behavsel$date_time))
# calculate sum ODBA per birdID & date_time combination, and add this to the dataframe with selected behaviour per bird & date_time:
ODBA.bird.datetime <- aggregate(odba~birdID+date_time, df.pred.behav.sel, sum)
birdID.datetime.behavsel <- merge(birdID.datetime.behavsel, ODBA.bird.datetime)
birdID.datetime.behavsel$odba <- round(birdID.datetime.behavsel$odba,3)

keep(df.pred.behav.sel, birdID.datetime.behavsel, sure=T)
