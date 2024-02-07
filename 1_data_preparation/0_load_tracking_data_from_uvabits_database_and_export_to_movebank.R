## load GPS and ACC data of all adult birds
## Establish connection with UvA-BITS database:
## provide the name of the database file
db.file <-"GPS"
#Establish a connection to the database (if architecture error, change R to 64 bits, go to Tools - Global Options - General - R version)
# Furthermore, I should be logged in as adminlocal...
db <- odbcConnect(db.file) 

# load bird data for all adults
bird.data <- read.csv("data/raw/bird.data.csv")
bird.data$start_deployment <- dmy(bird.data$start_deployment, tz='UTC')
bird.data$year.start <- year(bird.data$start_deployment)
bird.data$end_deployment <- dmy(bird.data$end_deployment, tz='UTC')
bird.data$year.end <- year(bird.data$end_deployment)

# create new gps.data.list and acc.data.list and device.infos and fill as much as possible with the above data:
gps.data.list <- vector(mode="list", length=dim(bird.data)[1])
names(gps.data.list) <- bird.data$birdID
acc.data.list <- vector(mode="list", length=dim(bird.data)[1])
names(acc.data.list) <- bird.data$birdID
device.infos <- ImportTrackerInfo(760) # to get the columns required for device info

# download data per deployment (bird x tracker)
for (i in 1:dim(bird.data)[1]) {
  print(bird.data[i,"logger"])
  gps.data <- ImportAllGPSDataBird(bird.data[i,"logger"], paste(bird.data[i,"year.start"], "-01-01 00:00:00", sep=""), paste(bird.data[i,"end_deployment"], " 23:59:59", sep=""))
  acc.data <- ImportAllAccDataBird(bird.data[i,"logger"], paste(bird.data[i,"year.start"], "-01-01 00:00:00", sep=""), paste(bird.data[i,"end_deployment"], " 23:59:59", sep=""))
  device.info <- ImportTrackerInfo(bird.data[i,"logger"])
  gps.data.list[[i]] <- gps.data
  acc.data.list[[i]] <- acc.data
  device.infos[i,] <- device.info
}

# export data in format suitable for Movebank:

# only select columns from bird.data that can be stored in Movebank:
bird.data.mb <- bird.data[,c('logger','birdID','ringnumber','colourcode','sex','start_deployment','end_deployment','bodymass','tracker_mass')]

# rename columns in bird.data:
names(bird.data.mb) <- c('tag-id', 'animal-id', 'animal-ring-id','animal-nickname','animal-sex','deploy-on-timestamp','deploy-off-timestamp','animal-mass','tag-mass')

# add additional columns:
bird.data.mb$'deployment-id' <- paste(bird.data.mb$'animal-id', bird.data.mb$'tag-id', sep="-")
bird.data.mb$'animal-taxon' <- 'Platalea leucorodia'
bird.data.mb$'animal-taxon-detail' <- 'Platalea leucorodia leucorodia (Linneaus, 1758)'
bird.data.mb$'animal-life-stage' <- 'adult'
bird.data.mb$'attachment-type' <- 'backpack-harness'
bird.data.mb$'capture-latitude' <- 53.49
bird.data.mb$'capture-longitude' <- 6.25
bird.data.mb$'deploy-on-person' <- 'Petra de Goeij'
bird.data.mb$'manipulation-type' <- 'none'
bird.data.mb$'study-site' <- 'Schiermonnikoog, The Netherlands'
bird.data.mb$'tag-readout-method' <- 'other-wireless'
bird.data.mb$'tag-manufacturer-name' <- 'UvA-BiTS'

write.csv(bird.data.mb, "data/raw/movebank-files/movebank_ref_data.csv", na="")

# go to Movebank -> Upload -> Import Data -> Reference data about animals, tracking tags, or deployments -> Use custom reference data format -> Combination (animal, tag, and deployment data in a single file) -> Match each column to a Movebank column. Be careful to correctly format the Timestamp string in Movebank for deploy-on-timestamp and deploy-off-timestamp (yyyy-MM-dd; check csv-file for this when opened with Notepad (as Excel changes the format)), and select correct timezone (Fixed offset from UTC)!

# upload the first csv file via: Upload -> Import data -> GPS data -> custom GPS data
# "Height above mean sea level", "location-error-numerical", "manually marked outlier" and "temperature-external" can be found under "Other event attributes"; location-lat and location-long can be assigned at once under "Location". Then this format can be saved under a given name.
# subsequent imports are then easily performed using the saved format found under Upload -> Import data -> Jump to pre-defined upload channel directly, and then select the custom format at the very bottom of the list.  

# create a csv file with GPS and one with ACC data for each deployment (bird x tag), with a GPS interval of at least 10 minutes and maximally 32 samples of accelerometer data per GPS-fix:
for (i in 1:c(1:dim(bird.data.mb)[1])) { 
  
  gps.data <- gps.data.list[[i]]
  acc.data <- acc.data.list[[i]]

  ### PREPARE GPS DATA FOR EXPORT ###
  
  gps.data$date <- floor_date(gps.data$date_time, 'day')
  # select period between start and end of deployment (including start and end date of deployment)
  gps.data <- gps.data[gps.data$date>=bird.data.mb$`deploy-on-timestamp`[i] & gps.data$date<=bird.data.mb$`deploy-off-timestamp`[i],] 
  
  # where applicable (for deployments < 2016), downsample to 10 minute interval.  
  if (year(bird.data.mb$`deploy-on-timestamp`[i])<2016) {
    gps.data$date_time_10min <- round_date(gps.data$date_time, "10min")
    # this rounding sometimes gives problems, for example with 15:05:01 and 15:14:59, both rounded to 15:10:00
    # calculate time between rounded 10 minute timestamps and actual time:
    gps.data$time_diff <- abs(gps.data$date_time - gps.data$date_time_10min)  
    # calculate the minimum time_diff_rnd and time_diff_flr for each rounded 10 minute timestamp:
    time_diff_min <- aggregate(time_diff~date_time_10min, gps.data, min)
    # merge original data with time_diff_min to select only those timestamps that are closest to the 10 minute   timestamps:
    gps.data.sel <- merge(gps.data, time_diff_min)
  } else gps.data.sel <- gps.data # for deployments from 2016 onward, as 10 minute intervals have always been used since then.
  
  # select and rename columns to fit with Movebank columns (after Peter Desmet code https://github.com/inbo/bird-tracking/blob/master/sql/uvabits_to_movebank_gps.sql):
  gps.data.sel <- gps.data.sel[,c('device_info_serial','pressure','positiondop','satellites_used',
                                  'gps_fixtime','speed_2d','direction','altitude','latitude','longitude',
                                  'h_accuracy','userflag','temperature','date_time','v_accuracy')]
  # put variables on the right scale or in the right format:
  gps.data.sel$direction <- ifelse(gps.data.sel$direction<0, 360 + gps.data.sel$direction, gps.data.sel$direction<0)
  gps.data.sel$userflag <- ifelse(gps.data.sel$userflag!=0, TRUE, FALSE)
  #gps.data.sel$date_time <- as.character(gps.data.sel$date_time)
  
  names(gps.data.sel) <- c('tag-id','barometric-pressure','gps-dop','gps-satellite-count','gps-time-to-fix','ground-speed','heading','height-above-mean-sea-level','location-lat','location-long','location-error-numerical','manually-marked-outlier','temperature-external','timestamp','vertical-error-numerical')
  
  # add the animal-id:
  gps.data.sel$'animal-id'<- bird.data.mb$'animal-id'[i]
  
  data_file = file.path("data/raw/movebank-files", paste0("movebank_gps", "_", bird.data.mb$'deployment-id'[i], ".csv"))
  write.csv(gps.data.sel, data_file, na="")

  ### PREPARE ACC DATA FOR EXPORT ###
  
  if (dim(acc.data)[1]>0) { # only if there is acc data available
    
    # only select first 32 indices per acc-sample: 
    acc.data.sel <- acc.data.sel[acc.data.sel$index<33,]

    # only select acc date_times that correspond to gps date_times:
    acc.data.sel <- acc.data[acc.data$date_time%in%gps.data.sel$timestamp,]

    # rename columns to fit with Movebank columns (after Peter Desmet code https://github.com/inbo/bird-tracking/blob/master/sql/uvabits_to_movebank_acc.sql):
    names(acc.data.sel) <- c('tag-id','start-timestamp','index','acceleration-raw-x','acceleration-raw-y','acceleration-raw-z')
  
    # add additional columns:
    acc.data.sel$'animal-id'<- bird.data.mb$'animal-id'[i]
    acc.data.sel$'sensor-type' <- 'Acceleration'
    acc.data.sel$timestamp <- acc.data.sel$'start-timestamp' + seconds((acc.data.sel$index-1)/20)
    acc.data.sel$'tilt-x' <- (acc.data.sel$'acceleration-raw-x' - device.infos$x_o[i]) / device.infos$x_s[i] 
    acc.data.sel$'tilt-y' <- (acc.data.sel$'acceleration-raw-y' - device.infos$y_o[i]) / device.infos$y_s[i] 
    acc.data.sel$'tilt-z' <- (acc.data.sel$'acceleration-raw-z' - device.infos$z_o[i]) / device.infos$z_s[i] 
  
    options(digits.secs=3) # to show milliseconds
  
    # if datafile is larger than 5 million rows, then split up into multiple csv-files:
    size.max <- 5000000
    if (dim(acc.data.sel)[1]<=size.max) {
      data_file = file.path("data/raw/movebank-files", paste0("movebank_acc", "_", bird.data.mb$'deployment-id'[i], ".csv"))
      write.csv(acc.data.sel, data_file, na="")
    } else {
    # determine number of files to create:
      n_files <- ceiling(dim(acc.data.sel)[1]/size.max)
      for (j in 1:n_files) {
        data_file_n = file.path("data/raw/movebank-files", paste0("movebank_acc", j, "_", bird.data.mb$'deployment-id'[i], ".csv"))
        if (j<n_files) acc.data.sel_n <- acc.data.sel[((j-1)*size.max+1):(j*size.max),] else acc.data.sel_n <- acc.data.sel[((j-1)*size.max+1):dim(acc.data.sel)[1],] 
      write.csv(acc.data.sel_n, data_file_n, na="")
      }
    }
  }
}