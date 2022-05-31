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