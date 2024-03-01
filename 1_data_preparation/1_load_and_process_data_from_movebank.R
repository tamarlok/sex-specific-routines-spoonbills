## Enter your Movebank username and password:
loginStored <- movebankLogin(username="xxxx", password="xxxx")

# search for study name with keywords:
searchMovebankStudies("poonbill", login=loginStored) # this gives different results than when searching in Movebank itself... Moreover, it is case sensitive. Therefore, remove first letter to see both ways of writing.

# if an error message is thrown with "Timeout was reached" in it, this means that data traffic at www.movebank.org is currently too heavy. Try again later.

## The below line only works if you have persmission to download the study (data). To get permission, contact the data owner. 
spoonbill_metawad_ID <- getMovebankID("SPOONBILL_METAWAD - Eurasian Spoonbills (Platalea leucorodia, Threskiornithidae) breeding on Schiermonnikoog, The Netherlands", login=loginStored)

# import bird data to import Movebank data seperately per individual
bird.data <- read.csv("data/raw/bird.data.csv")
bird.data$start_deployment <- dmy(bird.data$start_deployment, tz='UTC')
bird.data$year.start <- year(bird.data$start_deployment)
bird.data$end_deployment <- dmy(bird.data$end_deployment, tz='UTC')
bird.data$year.end <- year(bird.data$end_deployment)
birds = bird.data$birdID

# Set system time zone 
Sys.setenv(TZ="GMT")

# make a loop to load and process GPS and ACC data per bird and save data in a list:
gps.behav.data.list = list()

for (i in 1:length(birds)) {
  birdID = birds[i]
  print(birdID)
  gps.data = getMovebankLocationData(spoonbill_metawad_ID, sensorID="GPS", animalName=as.character(birdID), login=loginStored,
                                     timestamp_start = ymd_hms(paste(bird.data[i,"year.start"], "-01-01 00:00:01", sep="")), 
                                     timestamp_end = ymd_hms(paste(bird.data[i,"end_deployment"], " 23:59:59", sep="")))
  acc.data = getMovebankNonLocationData(spoonbill_metawad_ID, sensorID="Accessory Measurements", animalName=as.character(birdID), login=loginStored,
                                        timestamp_start = ymd_hms(paste(bird.data[i,"year.start"], "-01-01 00:00:01", sep="")), 
                                        timestamp_end = ymd_hms(paste(bird.data[i,"end_deployment"], " 23:59:59", sep="")))
  # select and relabel relevant columns in gps data:
  gps.data = gps.data[,c('individual.local.identifier','tag.local.identifier','timestamp','location.long','location.lat','height.above.msl','ground.speed')]
  names(gps.data) = c('birdID','tagID','date_time','longitude','latitude','altitude','speed_2d')
  # select and relabel relevant columns in acc data:
  acc.data = acc.data[,c('individual_local_identifier','start_timestamp','timestamp','tilt_x','tilt_y','tilt_z')]
  names(acc.data) = c('birdID','date_time','date_time_acc','x','y','z')
  # only select gps data >53N, as only these are used in the analyses (to speed up behavioural classification procedure)
  gps.data <- gps.data[gps.data$latitude>53,]
  # link gps and acc data for behavioural classification that also requires gps speed:
  data <- merge(gps.data, acc.data, all.x=T, by=c('birdID','date_time'))
  # make sure that data are order according the acc data timestamp:
  data <- data[order(data$date_time_acc),]
  # add column with Index for acc data
  data$Index <- 0
  for(j in 2:dim(data)[1]) if (data$date_time[j]==data$date_time[j-1]) data$Index[j]=data$Index[j-1]+1 else data$Index[j]=0 
  # predict behaviour from acc data and gps speed:
  data$obs.id <- paste(data$birdID, as.numeric(data$date_time), sep = ".")
  seg.df <- create.fixed.segments(segment.length=0.4, data)
  seg.df$pred.behav <- predict(RF.fixed.0.4, seg.df)
  # select the most occurring behaviour per GPS fix (i.e., date_time)
  # first pool behavioural categories
  seg.df$behaviour <- as.character(seg.df$pred.behav)
  seg.df$behaviour[seg.df$pred.behav%in%c("for-handle","for-intake","for-search")] = "foraging"
  seg.df$behaviour[seg.df$pred.behav%in%c("walk","drink")] = "other"
  seg.df$behaviour[seg.df$pred.behav%in%c("sit","stand")] = "resting"
  seg.df$behaviour[seg.df$pred.behav%in%c("fly-flap","fly-soar")] = "flying"
  # select the most often classified behaviour per GPS fix (as there are usually multiple segments (up to 4) of 0.4 s per GPS fix)
  seg.df$freq <- 1
  datetime.behaviours <- aggregate(freq~date_time+behaviour, seg.df, sum)
  datetime.behav.max <- aggregate(freq~date_time, datetime.behaviours, max) # the number of segments that the most often classified behaviour is classified
  datetime.behavdom <- merge(datetime.behaviours, datetime.behav.max, by=c('date_time','freq')) # behaviour that is expressed most often. This could still be more than one behaviour. If this is the case, then randomly choose one of these behaviours: 
  datetime.behavdom$rnd <- runif(dim(datetime.behavdom)[1])
  datetime.rndmax <- aggregate(rnd~date_time, datetime.behavdom, max)
  datetime.behavsel <- merge(datetime.behavdom, datetime.rndmax, by=c("date_time","rnd"))[,c("date_time","behaviour")]
  # calculate sum ODBA per birdID & date_time combination, and add this to the dataframe with selected behaviour per bird & date_time:
  ODBA.datetime <- aggregate(odba~date_time, seg.df, sum)
  datetime.behavsel <- merge(datetime.behavsel, ODBA.datetime)
  datetime.behavsel$odba <- round(datetime.behavsel$odba,3)
  datetime.behavsel <- datetime.behavsel[order(datetime.behavsel$date_time),]
  # link again to gps data to also keep the gps locations without acc data (for determining breeding phases):
  gps.behav.data = merge(gps.data, datetime.behavsel[,c('date_time','behaviour','odba')], all.x=T)
  gps.behav.data <- gps.behav.data[order(gps.behav.data$date_time),]
  gps.behav.data.list[[i]] <- gps.behav.data
}

keep(gps.behav.data.list, bird.data, sure=T)