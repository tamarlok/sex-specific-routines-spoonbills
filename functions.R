### Functions to import tracking data from UvA-BiTS website ###
# make R data file
ImportGPSDataBird <- function(birdID, date_start, date_end) {
  df <- sqlQuery(db, query = paste("SELECT device_info_serial,  
                                     date_time, longitude,
                                     latitude,altitude,speed_2d, gps_fixtime FROM gps.ee_tracking_speed_limited 
                                     WHERE device_info_serial = ",birdID,"AND  date_time >= '" , as.POSIXct(date_start, origin='1970-01-01', tz="UTC"),
                                   "' AND date_time <= '", as.POSIXct(date_end, origin='1970-01-01', tz="UTC"), "'", sep = ''))
  df[order(df$date_time),]
}

ImportSMSDataBird <- function(birdID, date_start, date_end) {
  df <- sqlQuery(db, query = paste("SELECT device_info_serial,  
                                     date_time, longitude,
                                     latitude FROM gps.ee_sms_position_limited
                                     WHERE device_info_serial = ",birdID,"AND  date_time >= '" , as.POSIXct(date_start, origin='1970-01-01', tz="UTC"),
                                   "' AND date_time <= '", as.POSIXct(date_end, origin='1970-01-01', tz="UTC"), "'", sep = ''))
  df[order(df$date_time),]
}

ImportAccDataBird <- function(birdID, date_start, date_end) {
  df <- sqlQuery(db, query = paste("SELECT device_info_serial,  
                                   date_time, index, x_acceleration, y_acceleration, z_acceleration FROM gps.ee_acceleration_limited 
                                   WHERE device_info_serial = ",birdID,"AND  date_time >= '" , as.POSIXct(date_start, origin='1970-01-01', tz="UTC"),
                                   "' AND date_time <= '", as.POSIXct(date_end, origin='1970-01-01', tz="UTC"), "'", sep = ''))
  df[order(df$date_time),]
}

ImportCOMDataBird <- function(birdID, date_start, date_end) {
  df <- sqlQuery(db, query = paste("SELECT device_info_serial,  
                                   date_time, vbat, longitude,
                                   latitude FROM gps.ee_comm_limited 
                                   WHERE device_info_serial = ",birdID,"AND  date_time >= '" , as.POSIXct(date_start, origin='1970-01-01', tz="UTC"),
                                   "' AND date_time <= '", as.POSIXct(date_end, origin='1970-01-01', tz="UTC"), "'", sep = ''))
  na.omit(df[order(df$date_time),])
}

ImportTrackerInfo <- function(birdID) {
  df <- sqlQuery(db, query = paste("SELECT device_info_serial, firmware_version, mass, start_date, end_date, x_o, x_s, y_o, y_s, z_o, z_s, tracker_id FROM gps.ee_tracker_limited 
                                   WHERE device_info_serial = ",birdID, sep = ''))
  df
}

### End of tracking data loading functions

### Functions needed to calculate summary statistics over acceleration segments ###

# Dominant Power Spectrum
dps <- function(x){
  d.x <- x - mean(x, na.rm = T)   
  return(max(((Mod(fft(d.x))^2)/length(x))[1:(length(x)/2)]))
}

# Frequency at the Dominant Power Spectrum
fdps <- function(x){
  fq <- (1:(length(x)/2)) / (length(x)*0.05)
  d.x <- x - mean(x, na.rm = T)   
  return(fq[which.max(((Mod(fft(d.x))^2)/length(x))[1:(length(x)/2)])])
}

odba <- function(x){
  d.x <- x - mean(x, na.rm = T)   
  return(sum(abs(d.x), na.rm = T)/length(x))
}

trend <- function(x){
  dx <- lm(x ~ c(1:length(x)))
  return(dx$coeff[2])
}

noise <- function(x){
  noise.tmp <- NA
  noise.mean <- NA
  if (length(x)>2) {
    for (i in 2:(length(x)-1)) noise.tmp[i] <- x[i]-(x[i-1]+x[i+1])/2
    noise.mean <- mean(na.omit(noise.tmp))
  }
  return(noise.mean)
}

### End of summary statistics function ###

### create fixed segments of acc samples, providing segment length and other options 
create.fixed.segments <- function(segment.length, acc = acc.annotated, remove.shorter.segments = TRUE, sampling.freq=20, annotated.data=T, naomit=T) { # segment length is expressed in seconds
  
  samples.per.segment <- ceiling(segment.length * sampling.freq) # to make it an integer (and select only segments with highest possible number of samples, given the sampling frequency; e.g. for a segment of 0.8 seconds at 2 Hz, this is 2 samples)
  
  # 20210617: these two lines are added to include the adjustment of the original sampling frequency (which was 20 Hz) within the creation of fixed segments code:
  indices.to.use <- seq(min(acc$Index),max(acc$Index),by=20/sampling.freq)
  acc.sel = acc[acc$Index %in% indices.to.use,] 

  acc.sel$segment.id.cut <- paste(acc.sel$obs.id, formatC(format="d", ceiling((acc.sel$Index+1)/(segment.length*20)),flag="0",width=ceiling(log10(max(ceiling((acc.sel$Index+1)/(segment.length*20)))))), sep = ".") # 20 is the sampling frequency at which the data was originally collected
  
  ## calculate summary statistics for each segment: 
  seg.df <- ddply(acc.sel, .(segment.id.cut, birdID), summarize, 
                  nobs.segments  = length (x), speed_2d = mean(speed_2d),
                  mean.x = mean(x), mean.z = mean(z), mean.y = mean(y), 
                  min.x = min (x), min.y = min (y), min.z = min (z),
                  max.x = max (x), max.y = max (y), max.z = max (z), 
                  trend.x = trend (x), trend.y = trend (y), trend.z = trend (z),
                  odba.x = odba(x), odba.y = odba(y), odba.z = odba(z), 
                  dps.x = dps(x), dps.y = dps(y), dps.z = dps(z),
                  fdps.x = fdps(x),  fdps.y = fdps(y), fdps.z = fdps(z), 
                  kurt.x = kurtosis(x), kurt.y = kurtosis(y), kurt.z = kurtosis(z), 
                  skew.x = skewness(x), skew.y = skewness(y), skew.z = skewness(z),
                  noise.x = noise(x), noise.y = noise(y), noise.z = noise(z)
  ) 
  
  seg.df$odba <- seg.df$odba.x + seg.df$odba.y + seg.df$odba.z 

  # If remove.shorter.segments is set at TRUE, then only use the segments of specified segment length for the machine learning and testing: 
  if (remove.shorter.segments == TRUE) seg.df.sel <- seg.df[seg.df$nobs.segments==samples.per.segment,] 
  set.seed(3) # to reproduce the same results from the random choice of equally occurring behaviours below (not sure if this is passed onto the global environment; this should not happen!)
  if (annotated.data==T) seg.df.sel <- assign.behaviour.to.segments(seg.df.sel, acc.sel) # here is some randomness included, when there are two behaviours expressed an equal amount of time within a single segment. One of these behaviours is then randomly chosen.
  table(seg.df.sel$behaviour.pooled)
  if (naomit==T) list(na.omit(seg.df.sel), acc.sel) else list(seg.df.sel, acc.sel) # remove cases where certain predictor variables (e.g., kurtosis or skewness) could not be calculated, if naomit=T
}

#########################################################
## Function to create dataframe with flexible segments ##
#########################################################

## check whether this works with downsampling the sampling frequency (see above for fixed segment lengths)
create.flexible.segments <- function(ARL0=5000, acc = acc.annotated, sampling.freq=20, max.segment.length=1.6, segmentation.script = "new", startup=1, annotated.data=T, naomit=T) {
  
  ### START creation flexible segments ###
  
  ## Renumber column Index so that each obs.id starts at 0
  index.min.obs.id <- aggregate(Index~obs.id, acc, min)
  names(index.min.obs.id)[2]<-"Index.start"
  acc <- merge(acc, index.min.obs.id)
  acc$Index <- acc$Index-acc$Index.start

  ### cut the segments to the length set by max.segment.length to allow the analysed data to better reflect how the data is collected in the long term on the majority of transmitters (in case of the spoonbills, mostly during bouts of 1.6 sec); by default it is set to 10 sec, which is the longest sampling duration in the data (10 sec), used for birds that were video-recorded: 
  acc$obs.id.cut <- paste(acc$obs.id, formatC(format="d", ceiling((acc$Index+1)/(max.segment.length*20)),flag="0",width=ceiling(log10(max(ceiling((acc$Index+1)/(max.segment.length*20)))))), sep = ".") 
  
  acc$segment.id.cut <- acc$obs.id.cut
  acc$duration <- 1/20
  
  un.obs.cut <- aggregate(duration~obs.id.cut, acc, sum)
  
  un.obs.cut <- un.obs.cut[round(un.obs.cut$duration,1)==max.segment.length,] # only select segments equal to the max segment length (as this will be the segment length at which the data is collected)
  un.obs.cut <- un.obs.cut$obs.id.cut
  acc <- acc[acc$obs.id.cut %in% un.obs.cut, ]
  
  # new script:
  if (segmentation.script == "new") { 
    for(k in 1 : length(un.obs.cut)) {
    temp.acc <- acc[which(acc$obs.id.cut == un.obs.cut[k]),]
    
    dcpb <- processStream(temp.acc$x, "GLR", ARL0=ARL0, startup=startup)   
    
    incl <- dcpb$changePoints[c(1,which(diff(dcpb$changePoints)>2)+1)] # after selecting the first changepoint, only select subsequent changepoints that are more than 2 acc-samples further from the previous changepoint (i.e. causing the minimum segment length to become 3 samples); Bom et al. 2014 used a min sample size of 4 here. If breakpoints are estimated at position 3, 5 and 7 for example, only 3 is used.  
    
    if (is.na(max(incl))==F) # only run the below line when there is at least one change point, otherwise use the original (and entire) segment.id.cut. 
        acc$segment.id.cut[which(acc$obs.id.cut == un.obs.cut[k])] <- paste(temp.acc$obs.id.cut, letters[rep(c(1:(length(incl)+1)), c(diff(c(0, incl, nrow(temp.acc)))))], sep = ".")
    }
  }
    
   # old script used in Bom et al. 2014 (performs particularly worse with segment lengths of 1-2 s; the new script is insensitive to max seg length)
   if (segmentation.script == "old") {
     for(k in 1 : length(un.obs.cut)) {
      temp.acc <- acc[which(acc$obs.id.cut == un.obs.cut[k]),]
      
      dcpb <- processStream(temp.acc$x,"GLR",ARL0=ARL0, startup=startup)
      
      incl <- dcpb$changePoints[c(1,which(diff(dcpb$changePoint)>3)+1)]
      if(length(incl) > 1) # this code causes the segments to be cut into smaller segments only when there is more than one breakpoint, whereas it should also be cut when there is only one breakpoint. This causes the dip in performance at max segment lengths of 1-2 s.
        acc$segment.id.cut[which(acc$obs.id.cut == un.obs.cut[k])] <- paste(temp.acc$obs.id.cut, letters[rep(c(1:(length(incl)+1)), c(diff(c(0, incl, nrow(temp.acc)))))], sep = ".")
    }
  }

  # segments can be shorter but never larger than the sampling duration * sampling frequency:
  max.segment.length * sampling.freq
  min(table(acc$segment.id.cut))
  max(table(acc$segment.id.cut))
  
  ### END creation flexible segments ###
  
  # remove NA's from acc:
  if (annotated.data==T) acc <- na.omit(acc[,c("birdID","segment.id.cut","speed_2d","x","y","z","behaviour.pooled")]) else acc <- na.omit(acc[,c("birdID","segment.id.cut","device_info_serial","date_time","latitude","longitude","altitude","speed_2d","x","y","z")])
  
  ## calculate summary statistics per segment
  seg.df <- ddply(acc, .(segment.id.cut, birdID), summarize, 
                  nobs.segments  = length (x), speed_2d = mean(speed_2d),
                  mean.x = mean(x), mean.z = mean(z), mean.y = mean(y), 
                  min.x = min (x), min.y = min (y), min.z = min (z),
                  max.x = max (x), max.y = max (y), max.z = max (z), 
                  trend.x = trend (x), trend.y = trend (y), trend.z = trend (z),
                  odba.x = odba(x), odba.y = odba(y), odba.z = odba(z), 
                  dps.x = dps(x), dps.y = dps(y), dps.z = dps(z),
                  fdps.x = fdps(x),  fdps.y = fdps(y), fdps.z = fdps(z), 
                  kurt.x = kurtosis(x), kurt.y = kurtosis(y), kurt.z = kurtosis(z), 
                  skew.x = skewness(x), skew.y = skewness(y), skew.z = skewness(z),
                  noise.x = noise(x), noise.y = noise(y), noise.z = noise(z)
  ) 
  
  seg.df$odba <- seg.df$odba.x + seg.df$odba.y + seg.df$odba.z 
  if (annotated.data==T) seg.df <- assign.behaviour.to.segments(seg.df, acc)
  if (naomit==T) list(na.omit(seg.df), acc) # remove cases where certain predictor variables (e.g., kurtosis or skewness) could not be calculated
  else list(seg.df, acc)
}  

assign.behaviour.to.segments <- function(seg.df, acc) {
    # match most occurring behaviour during a segment with seg.df data frame (this is different from Roeland's code)
    acc$freq <- 1
    segment.id.cut.behaviours <- aggregate(freq~segment.id.cut+behaviour.pooled, acc, sum)
    segment.id.cut.nobsmax <- aggregate(freq~segment.id.cut, segment.id.cut.behaviours, max) # the number of points that the longest expressed behaviour is expressed
    segment.id.cut.behavdom <- merge(segment.id.cut.behaviours, segment.id.cut.nobsmax, by=c('segment.id.cut','freq')) # behaviour that is expressed the longest
    # randomly select one of the equally often expressed behaviours:
    segment.id.cut.behavdom$rnd <- runif(dim(segment.id.cut.behavdom)[1])
    segment.id.cut.rndmax <- aggregate(rnd~segment.id.cut, segment.id.cut.behavdom, max)
    segment.id.cut.behavsel <- merge(segment.id.cut.behavdom, segment.id.cut.rndmax, by=c("segment.id.cut","rnd"))
    
    # determine whether a segment consists of a single behaviour ("clean" segments)
    num.obs <- cast(melt(acc), segment.id.cut ~ behaviour.pooled, length, subset = variable == 'birdID')
    nobs.segment <- aggregate(freq~segment.id.cut, acc, sum)
    names(nobs.segment)[2]<-"nobs"
    segment.id.cut.behavsel <- merge(segment.id.cut.behavsel, nobs.segment, all.x=T)
    segment.id.cut.behavsel$single.behaviour <- 0
    segment.id.cut.behavsel$single.behaviour[segment.id.cut.behavsel$freq==segment.id.cut.behavsel$nobs] <- 1
    
    seg.df$behaviour.pooled <- segment.id.cut.behavsel$behaviour.pooled[match(seg.df$segment.id.cut,  segment.id.cut.behavsel$segment.id.cut)] 
    seg.df$single.behaviour <- segment.id.cut.behavsel$single.behaviour[match(seg.df$segment.id.cut,  segment.id.cut.behavsel$segment.id.cut)] 
    seg.df <- seg.df[is.na(seg.df$behaviour.pooled)==F,] # remove cases where behaviour was not classified
    seg.df
  }
     
######################################
## Function for Random Forest model ##
######################################

RF.model <- function(seg.df, acc, selected.variables = predictors.all, clean.segments.train = FALSE, clean.segments.test = FALSE, stand=1, search=1, drink=1, handle=1, ingest=1, walk=1, soar=1) {
  # in the segmentation function, there is an option to remove all rows which contain at least 1 predictor with NA. If this has NOT been done, we here remove the predictors that contain all NA's (because they can't be calculated over 1 or 2 points), and then remove the remaining rows that still have some NA's for other predictors. 
  seg.df <- seg.df[,apply(!is.na(seg.df), 2, any)]
  seg.df <- na.omit(seg.df)
    
  ind <- sample(1:2, nrow(seg.df), replace = TRUE, prob=c(0.7, 0.3)) # divide data into 70% training (ind=1) and 30% testing (ind=2) data 
  data.train <- seg.df[ind == 1,] # the training dataset
  data.test <- seg.df[ind ==2,] # the testing dataset
  
  # perform up- and downsampling on the train dataset only, to be able to properly interpret the effects on sensitivity and precision for the test dataset
  data.train <- downsampling.behaviours(data.train, stand=stand, search=search)
  data.train <- upsampling.behaviours(data.train, drink=drink, handle=handle, ingest=ingest, walk=walk, soar=soar)
  data.train$behaviour.pooled <-  factor(data.train$behaviour.pooled) 
  if (clean.segments.train == T) data.train <- data.train[data.train$single.behaviour==1,]
  if (clean.segments.test == T) data.test <- data.test[data.test$single.behaviour==1,]
  
  # remove the predictor variables from selected.variables that contained all NA in seg.df (e.g. noise when there are only 2 samples per segment, at 2 Hz)
  selected.variables <- selected.variables[selected.variables%in%names(seg.df)]
  data.train <- data.train[,c("behaviour.pooled", selected.variables)]
  
  # fit the model on the train dataset
  fit.RF <- randomForest(behaviour.pooled ~ ., data = data.train, importance=T)
  behav.pred <- predict(fit.RF, data.test) # do the prediction on a random selection of the dataset (the testing/validation dataset)
  df.pred <- cbind(data.test, behav.pred)
  acc.pred  <- merge(acc, df.pred[,c("segment.id.cut","behav.pred")])
  mytable <- table(predicted = acc.pred$behav.pred, observed = acc.pred$behaviour.pooled)
  mytable <- mytable[, match(rownames(mytable), colnames(mytable))]
  list(fit.RF, mytable, df.pred)
}

link.gps.acc.data <- function(gps.data, acc.data, device.info) {
  data <- merge(gps.data, acc.data)
  data$x <- (data$x_acceleration-device.info$x_o)/device.info$x_s
  data$y <- (data$y_acceleration-device.info$y_o)/device.info$y_s
  data$z <- (data$z_acceleration-device.info$z_o)/device.info$z_s
  data <- na.omit(data[,c('device_info_serial','index','date_time',"longitude","latitude","altitude",'speed_2d','x','y','z')])
  data
}

from.list.to.df <- function(list) {   # change from list to dataframe
  df <- list[[1]]
  if (length(list)>1) for (i in 2:length(list)) df <- rbind(df, list[[i]])
  df
}

# function to determine nest coordinates and calculate nest attendance 
# make the plotting optional, as this is not possible on the NIOZ cluster
determine.breeding.phases <- function(df, day_hatched=NA, day_caught=NA, day_hatched2=NA, successful=0) {
  df=df[df$duration<61,] # only use data with a duration of one hour or less. 
  df <- na.omit(df) # this removes the points for which no habitat info is available, and the first and last point of a bird in each year, as duration could not be calculated (as time_until_previous or time_until_next was NA)
  df$lat_rnd=round(df$latitude, digit=5) 
  df$lon_rnd=round(df$longitude, digit=6) 
  
  # rough determination of breeding phases, irrespective of when eggs are being laid
  # assuming that breeding starts upon arrival on the Oosterkwelder of Schiermonnikoog and ends at the day after the last visit of the Oosterkwelder
  df$schier.kwelder <- ifelse(df$habitat=="Schier_Kwelder",1,0)
  schier.kwelder.points = aggregate(schier.kwelder ~ year + yday_CEST, df, sum)
  breeding.period = schier.kwelder.points[schier.kwelder.points$schier.kwelder>1,] 
  start.breeding = min(breeding.period$yday_CEST)
  end.breeding = max(breeding.period$yday_CEST)
  df$breeding <- "pre-breeding"
  df$breeding[df$yday_CEST>=start.breeding & df$yday_CEST<=end.breeding] <- "breeding"
  df$breeding[df$yday_CEST>end.breeding] <- "post-breeding"

  # determine the total time spent per rounded coordinate, selecting the position with the longest total time spent as (the primary) nest:
  duration.per.coord = aggregate(duration~lat_rnd+lon_rnd, df, sum)
  duration.per.coord = duration.per.coord[order(duration.per.coord$duration, decreasing=TRUE),][1:20,]
  coord.nest1 = duration.per.coord[1,1:2] # determine coordinates nest as the coordinates visited for the longest time 
  # calculate distance to nest:
  df$dist.nest1 = distCosine(as.matrix(df[,c('longitude','latitude')]), coord.nest1[,c('lon_rnd','lat_rnd')], r=6378137) # gives the distance in meters
  # to get around 50% nest attendance during egg incubation, a diameter of about 5 meter is required around the nest to define that the bird is on the nest
  df$nest1 = ifelse(df$dist.nest1<5,1,0)
  df$nest1.1m = ifelse(df$dist.nest1<1,1,0)
  nest1.attendance = aggregate(round(duration/60,2)~yday_CEST, df[df$nest1==1,], sum)
  names(nest1.attendance)[2]='hours.on.nest'
  sum(nest1.attendance$hours.on.nest)
  nest1.first.day.5h = min(nest1.attendance$yday_CEST[nest1.attendance$hours.on.nest>5])
  nest1.last.day.5h = max(nest1.attendance$yday_CEST[nest1.attendance$hours.on.nest>5])
  hours.on.kwelder = aggregate(round(duration/60,2)~yday_CEST, df[df$habitat=='Schier_Kwelder',], sum)
  names(hours.on.kwelder)[2]='hours.on.kwelder'
  kwelder.first.day.5h = min(hours.on.kwelder$yday_CEST[hours.on.kwelder$hours.on.kwelder>5])
  kwelder.last.day.5h = max(hours.on.kwelder$yday_CEST[hours.on.kwelder$hours.on.kwelder>5])
  hours.on.mainland = aggregate(round(duration/60,2)~yday_CEST, df[df$habitat=='wal_rest_zoet'|df$habitat=='wal_rest_land'|df$habitat=='LM_land'|df$habitat=='LM_zoet',], sum)
  names(hours.on.mainland)[2]='hours.on.mainland'
  # last GPS fix of this bird in a certain year (used for plotting) 
  date.last.fix = max(df$yday_CEST)
  nest.coords <- data.frame(lat.nest=coord.nest1$lat_rnd, lon.nest=coord.nest1$lon_rnd, day.first.5h=nest1.first.day.5h, day.last.5h=nest1.last.day.5h) # if there was only a single nesting attempt, this will be the nest.coords. If there is a second nesting attempt (determined below), nest.coords will be overwritten, consisting of two rows associated with the two nesting attempts in correct order. 
  # determine whether the 1st nesting attempt was real (i.e. more than 5 days of >5 hours nest attendance, on Schier):
  nest1.attendance$more.than.5h = ifelse(nest1.attendance$hours.on.nest>=5,1,0)
  nest1.ndays.5h = sum(nest1.attendance$more.than.5h) # calculcates the number of days that this place is visited for more than 5 hours.
  # determine if breeding attempt 1 was a real breeding attempt (more than 5 days with more than 5 hours nest attendance):
  if (is.na(unique(df$habitat[df$nest1==1])[1])) nest1.real=0 else nest1.real = ifelse(nest1.ndays.5h>=5 & unique(df$habitat[df$nest1==1])[1]=='Schier_Kwelder', 1, 0)
  
  # save the nest coordinates if nest1.real=1
  if (nest1.real==1) {
    df$lat.nest1=coord.nest1$lat_rnd
    df$lon.nest1=coord.nest1$lon_rnd
  } else {
    df$lat.nest1=NA
    df$lon.nest1=NA
  }
  
  # determine if there was another breeding attempt:
  df.rest = df[df$dist.nest1>50,] # should be outside the GPS error range of the primary breeding attempt
  df.rest<-df.rest[order(df.rest$date_time),]
  duration.per.coord2 = aggregate(duration~lat_rnd+lon_rnd, df.rest, sum)
  duration.per.coord2 = duration.per.coord2[order(duration.per.coord2$duration, decreasing=TRUE),][1:20,]
  coord.nest2 = duration.per.coord2[1,1:2] # determine coordinates nest as the coordinates most visited
  df$dist.nest2 = distCosine(as.matrix(df[,c('longitude','latitude')]), coord.nest2[,c('lon_rnd','lat_rnd')], r=6378137) # gives the distance in meters
  df$nest2 = ifelse(df$dist.nest2<5,1,0)
  df$nest2.1m = ifelse(df$dist.nest2<1,1,0)
  nest2.attendance = aggregate(round(duration/60,2)~yday_CEST, df[df$nest2==1,], sum)
  names(nest2.attendance)[2]='hours.on.nest'
  sum(nest2.attendance$hours.on.nest)
  nest2.first.day.5h = min(nest2.attendance$yday_CEST[nest2.attendance$hours.on.nest>5])
  nest2.last.day.5h = max(nest2.attendance$yday_CEST[nest2.attendance$hours.on.nest>5])
  nest2.attendance$more.than.5h = ifelse(nest2.attendance$hours.on.nest>=5,1,0)
  nest2.ndays.5h = sum(nest2.attendance$more.than.5h) # calculcates the number of days that this place is visited for more than 5 hours.
  
  # determine whether the 2nd nesting attempt was real (i.e. more than 5 days of >5 hours nest attendance, on Schier):
  if (is.na(unique(df$habitat[df$nest2==1])[1])) nest2.real=0 else nest2.real = ifelse(nest2.ndays.5h>=5 & unique(df$habitat[df$nest2==1])[1]=='Schier_Kwelder', 1, 0)

  if ( nest2.real==1 ) { 
    # recreate dataframe with nest.coords, now consisting of two rows. The order of the attempts is determined later.
    nest.coords <- data.frame(lat.nest=c(coord.nest1$lat_rnd, coord.nest2$lat_rnd), lon.nest=c(coord.nest1$lon_rnd, coord.nest2$lon_rnd), day.first.5h=c(nest1.first.day.5h,nest2.first.day.5h), day.last.5h=c(nest1.last.day.5h,nest2.last.day.5h))
    df$lat.nest2=coord.nest2$lat_rnd
    df$lon.nest2=coord.nest2$lon_rnd
  }
  
  if ( nest2.real==0 ) {
    coord.nest2=NA
    df$nest2=0
    df$nest2.1m=0
    df$lat.nest2=NA
    df$lon.nest2=NA
  }
  
  # determining the breeding phase of the bird, only when hatchday (and sometimes hatchday2) is known:
  phase.doy = data.frame(yday_CEST=1:365, breeding.phase=NA)
  phase.doy$breeding.phase = 'pre-breeding'
  if (is.na(day_hatched)==F|is.na(day_hatched2)==F) {  
    if (is.na(day_hatched)==F)  phase.doy$breeding.phase[phase.doy$yday_CEST>=(day_hatched-25)&phase.doy$yday_CEST<day_hatched] = 'eggs' # this gives 25 days of egg incubation
    if (is.na(day_hatched2)==F) phase.doy$breeding.phase[phase.doy$yday_CEST>=day_hatched2-25&phase.doy$yday_CEST<day_hatched2] = 'eggs'
    if (successful==1) {
      if (is.na(day_hatched2)==T) { # then nest1 was the first breeding attempt
        phase.doy$breeding.phase[phase.doy$yday_CEST>=day_hatched&phase.doy$yday_CEST<(day_hatched+30)]='chicks'
        phase.doy$breeding.phase[phase.doy$yday_CEST>=(day_hatched+30)]='post.breeding.successful' # if this was also the only nesting attempt
      }
      else { # when there is a day_hatched2, the first breeding attempt was by definition unsuccessful.  
        phase.doy$breeding.phase[phase.doy$yday_CEST>=day_hatched2&phase.doy$yday_CEST<(day_hatched2+30)]='chicks'
        phase.doy$breeding.phase[phase.doy$yday_CEST>=(day_hatched2+30)]='post.breeding.successful'
        phase.doy$breeding.phase[phase.doy$yday_CEST>=day_hatched&phase.doy$yday_CEST<=nest1.last.day.5h]='chicks' # an earlier unsuccessful breeding attempt may have reached the chick phase; we assume that the chicks died on the last day the parent was at the nest for 5 hours (this is the last day with breeding phase 'chicks'; the next day it is 'pre-breeding' again.
      }
    } else { # if successful==0
      if (is.na(day_hatched)==F) phase.doy$breeding.phase[phase.doy$yday_CEST>=day_hatched&phase.doy$yday_CEST<=nest1.last.day.5h]='chicks'
      if (is.na(day_hatched2)==F) phase.doy$breeding.phase[phase.doy$yday_CEST>=day_hatched2&phase.doy$yday_CEST<=nest2.last.day.5h]='chicks' # if nest2.last.day.5h=NULL, this code does nothing
      phase.doy$breeding.phase[phase.doy$yday_CEST>max(nest1.last.day.5h,nest2.last.day.5h)]='post.breeding.unsuccessful'
    }
  
    ## Determine order of attempts (including not monitored attempts prior to the first real attempt)
    phase.doy$attempt = 0
    if (phase.doy$breeding.phase[1]=='eggs'|phase.doy$breeding.phase[1]=='chicks'|phase.doy$breeding.phase[1]=='breeding') phase.doy$attempt = 1
    for (i in 2:dim(phase.doy)[1]) {
      if ((phase.doy$breeding.phase[i]=='eggs'|phase.doy$breeding.phase[i]=='breeding')&
          phase.doy$breeding.phase[i-1]!='eggs'&phase.doy$breeding.phase[i-1]!='breeding'&phase.doy$breeding.phase[i-1]!='chicks') phase.doy$attempt[i]=phase.doy$attempt[i-1]+1
      else phase.doy$attempt[i]=phase.doy$attempt[i-1]
    }
    phase.doy$attempt[phase.doy$breeding.phase=='pre-breeding']<-0
    
    # assign nest coordinates to correct attempts (with known hatchdates - so only data are corrected of nests where eggs hatched):
    if (is.na(day_hatched)==F & is.na(day_hatched2)) attempt.hatchday <- data.frame(attempt=1, hatchday=day_hatched)
    if (is.na(day_hatched2)==F) attempt.hatchday <- data.frame(attempt=1:2, hatchday=c(day_hatched, day_hatched2))
    nest.coords <- merge(nest.coords, attempt.hatchday)
    nest.coords <- nest.coords[nest.coords$hatchday>nest.coords$day.first.5h & nest.coords$hatchday<nest.coords$day.last.5h,]
    phase.doy <- merge(phase.doy, nest.coords[,c('attempt','lat.nest','lon.nest')], all.x=T) # all.x=T is needed for cases where the nest attendance is outside the range of the hatch day. This was the case for 656 in 2015, when the logger stopped working.
    phase.doy
    table(phase.doy$breeding.phase) # to check that egg phase is indeed 25 days, and chick phase 30 days. 
    
    # merge breeding phase and nest coordinate info with df 
    df = merge(df, phase.doy, by='yday_CEST', all.x=T)
    df = df[order(df$date_time),]
  }
  
  # if hatchday is unknown, add the columns that were not created as the above code was not run, and fill them with NA (so that in the structure of the df will be exactly the same, for rbinding them later on)
  if (is.na(day_hatched)&is.na(day_hatched2)) {
    df$attempt <- NA
    df$breeding.phase <- NA
    df[,c('lat.nest','lon.nest')] <- c(NA,NA)
    df[df$yday_CEST>=nest1.first.day.5h&df$yday_CEST<=nest1.last.day.5h,c('lat.nest','lon.nest')] <- coord.nest1 # only give the nest coordinates from the first until the last day that the bird is more than 5 hours per day on this coordinate.
    if (nest2.real==1) df[df$yday_CEST>=nest2.first.day.5h&df$yday_CEST<=nest2.last.day.5h,c("lat.nest","lon.nest")] <- coord.nest2
  }
  
  # determine distance to nest
  df$distance.to.nest <- NA
  if ((nest1.real==1|nest2.real==1) & min(is.na(df$lat.nest))==0) df$distance.to.nest[is.na(df$lat.nest)==F] <- round(distCosine(df[is.na(df$lat.nest)==F,c('longitude','latitude')], df[is.na(df$lat.nest)==F,c('lon.nest','lat.nest')], r=6378137),3) # gives the distance in meters between nest location and actual position of the bird; only calculate this when there is at least one row where lat.nest is known.
  
  # calculate distance between subsequent points:
  df$distance.to.prev[2:dim(df)[1]] = round(distCosine(df[1:(dim(df)[1]-1),c('longitude','latitude')], df[2:dim(df)[1],c('longitude','latitude')], r=6378.137),3)
  df$distance.to.next = c(df$distance.to.prev[2:dim(df)[1]],NA)
  
  df[,-which(names(df) %in% c("catch.date","lat_rnd","lon_rnd","dist.nest1","nest1.1m","dist.nest2","nest2.1m"))] # code could be rewritten so that these columns are not added to df in the first place. 
}

# visualize the nest and kwelder attendance and breeding phases in a plot:
plot.breeding.phases.and.nest.schier.attendance <- function(x) {
  windows(8,6)
  par(mar=c(4.5,4.5,3,12))
  plot(c(90,250), c(0,24), xlab='Date', ylab='Hours present per date', main=paste('Logger',unique(df$birdID),'in',unique(df$year)), type='n', cex.lab=1.2, xaxt='n')
  axis(1, c(91, 121, 152, 182, 213, 244), labels=c('1 Apr','1 May', '1 Jun','1 Jul','1 Aug','1 Sep'))
  ## Plot attempt 1                                                                                                             
  if( max(phase.doy$attempt==1&phase.doy$breeding.phase=='breeding')==1 ) {
    breeding1.min = min(phase.doy$yday_CEST[phase.doy$attempt==1&phase.doy$breeding.phase=='breeding'])
    breeding1.max = max(phase.doy$yday_CEST[phase.doy$attempt==1&phase.doy$breeding.phase=='breeding'])+1 # de '+1' is om de periodes aan 
    polygon(c(breeding1.min, breeding1.min, breeding1.max, breeding1.max), c(0,24,24,0), col='lightyellow', border=NA)
  }
  if( max(phase.doy$attempt==1&phase.doy$breeding.phase=='eggs')==1 ) {
    eggs1.min = min(phase.doy$yday_CEST[phase.doy$attempt==1&phase.doy$breeding.phase=='eggs'])
    eggs1.max = max(phase.doy$yday_CEST[phase.doy$attempt==1&phase.doy$breeding.phase=='eggs'])+1
    polygon(c(eggs1.min, eggs1.min, eggs1.max, eggs1.max), c(0,24,24,0), col='lightblue', border=NA)
  }
  if( max(phase.doy$attempt==1&phase.doy$breeding.phase=='chicks')==1 ) {
    chicks1.min = min(phase.doy$yday_CEST[phase.doy$attempt==1&phase.doy$breeding.phase=='chicks'])
    chicks1.max = max(phase.doy$yday_CEST[phase.doy$attempt==1&phase.doy$breeding.phase=='chicks'])+1
    polygon(c(chicks1.min, chicks1.min, chicks1.max, chicks1.max), c(0,24,24,0), col='lightpink', border=NA)
  }
  if( max(phase.doy$attempt==1&phase.doy$breeding.phase=='fledglings')==1 ) {
    fledglings1.min = min(phase.doy$yday_CEST[phase.doy$attempt==1&phase.doy$breeding.phase=='fledglings'])
    fledglings1.max = max(phase.doy$yday_CEST[phase.doy$attempt==1&phase.doy$breeding.phase=='fledglings'])+1
    polygon(c(fledglings1.min, fledglings1.min, fledglings1.max, fledglings1.max), c(0,24,24,0), col='plum2', border=NA)
  }
  ## Plot attempt 2
  if( max(phase.doy$attempt==2&phase.doy$breeding.phase=='breeding')==1 ) {
    breeding2.min = min(phase.doy$yday_CEST[phase.doy$attempt==2&phase.doy$breeding.phase=='breeding'])
    breeding2.max = max(phase.doy$yday_CEST[phase.doy$attempt==2&phase.doy$breeding.phase=='breeding'])+1
    polygon(c(breeding2.min, breeding2.min, breeding2.max, breeding2.max), c(0,24,24,0), col='lightyellow', border=NA)
  }
  if( max(phase.doy$attempt==2&phase.doy$breeding.phase=='eggs')==1 ) {
    eggs2.min = min(phase.doy$yday_CEST[phase.doy$attempt==2&phase.doy$breeding.phase=='eggs'])
    eggs2.max = max(phase.doy$yday_CEST[phase.doy$attempt==2&phase.doy$breeding.phase=='eggs'])+1
    polygon(c(eggs2.min, eggs2.min, eggs2.max, eggs2.max), c(0,24,24,0), col='lightblue', border=NA)
  }
  if( max(phase.doy$attempt==2&phase.doy$breeding.phase=='chicks')==1 ) {
    chicks2.min = min(phase.doy$yday_CEST[phase.doy$attempt==2&phase.doy$breeding.phase=='chicks'])
    chicks2.max = max(phase.doy$yday_CEST[phase.doy$attempt==2&phase.doy$breeding.phase=='chicks'])+1
    polygon(c(chicks2.min, chicks2.min, chicks2.max, chicks2.max), c(0,24,24,0), col='lightpink', border=NA)
  }
  if( max(phase.doy$attempt==2&phase.doy$breeding.phase=='fledglings')==1 ) {
    fledglings2.min = min(phase.doy$yday_CEST[phase.doy$attempt==2&phase.doy$breeding.phase=='fledglings'])
    fledglings2.max = max(phase.doy$yday_CEST[phase.doy$attempt==2&phase.doy$breeding.phase=='fledglings'])+1
    polygon(c(fledglings2.min, fledglings2.min, fledglings2.max, fledglings2.max), c(0,24,24,0), col='plum2', border=NA)
  }
  lines(hours.on.mainland~yday_CEST, data=hours.on.mainland, pch=19, col='blue', type='l')
  lines(hours.on.kwelder~yday_CEST, data=hours.on.kwelder, pch=19, col='darkolivegreen4', cex=0.8, type='o')
  lines(hours.on.nest~yday_CEST, data=nest1.attendance, pch=19, cex=0.8, type='o')
  #lines(c(day.caught,day.caught),c(0,24))
  lines(c(date.last.fix,date.last.fix),c(0,24), col='red', lwd=2)
  
  if ( nest2.real==1 ) lines(hours.on.nest~yday_CEST, data=nest2.attendance, pch=21, cex=0.8, bg='white', type='o')
}

# plot nest attendance, potentially in relation to the tide 
plot.nest.attendance <- function(df, breeding.phase="egg incubation", add.tide = T) {
  df$minute <- minute(df$date_time)
  df$day_min = df$hour_CEST*60+df$minute # here: it makes calculations with hour_CEST and min_CEST which is summertime. 
  windows()
  layout(1:30)
  par(mar=c(0,0,0,0), oma=c(2,3,3,1))
  for (i in unique(df$yday_CEST)) {
    df.yday = df[df$yday_CEST==i,]
    plot(nest1~day_min, df.yday, type='n', xaxt='n', yaxt='n', xaxs='i', yaxs='i',xlim=c(0,1440),ylim=c(0,1)) # xlim=c(0,1440)
    mtext(unique(paste(day(df.yday$date_time), month(df.yday$date_time),sep="/")), 2, 0.5, las=1, cex=0.8)
    N <- length(df.yday$day_min) 
    for (j in 1:(N-1)) polygon(x=c(df.yday$day_min[j],df.yday$day_min[j+1],df.yday$day_min[j+1],df.yday$day_min[j]), y=c(0,0,1,1), col=c('coral1','green')[df.yday$nest1[j]+1], border=NA)
    if (add.tide==T) {
      segments(low_tides$day_min[low_tides$yday_CEST==i&low_tides$year==unique(df$year)], y0=0, y1=1, col="yellow", lwd=2)
      segments(high_tides$day_min[high_tides$yday_CEST==i&high_tides$year==unique(df$year)], y0=0, y1=1, col="blue", lwd=2)
    }
  }
  sex = ifelse(unique(df$sex)=="F","Female","Male")
  mtext(paste(sex, unique(df$birdID), 'at nest during', breeding.phase, sep=' '), 3,1, outer=T)
  axis(1,at=60*(0:24),labels=0:24)
}