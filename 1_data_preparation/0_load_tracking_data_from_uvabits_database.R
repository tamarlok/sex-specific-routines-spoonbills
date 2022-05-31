# load GPS and ACC data of female spoonbills 2016-2019
## Establish connection with UvA-BITS database:
#provide the name of the database file
db.file <-"GPS"
#Establish a connection to the database (if architecture error, change R to 64 bits, go to Tools - Global Options - General - R version)
# Furthermore, I should be logged in as adminlocal...
db <- odbcConnect(db.file) 

bird.data <- read.csv("data/raw/bird.data.csv")

# create new gps.data.list and acc.data.list and device.infos and fill as much as possible with the above data:
gps.data.list <- vector(mode="list", length=dim(bird.data)[1])
names(gps.data.list) <- bird.data$birdID
acc.data.list <- vector(mode="list", length=dim(bird.data)[1])
names(acc.data.list) <- bird.data$birdID
device.infos <- ImportTrackerInfo(760) # to get the columns required for device info

# download data per bird (not per logger)
for (i in 1:dim(bird.data)[2]) {
  print(bird.data[i,"logger"])
  gps.data <- ImportGPSDataBird(bird.data[i,"logger"], paste(bird.data[i,"year.start"], "-01-01 00:00:00", sep=""), paste(bird.data[i,"year.end"], "-01-01 00:00:00", sep=""))
  acc.data <- ImportAccDataBird(bird.data[i,"logger"], paste(bird.data[i,"year.start"], "-01-01 00:00:00", sep=""), paste(bird.data[i,"year.end"], "-01-01 00:00:00", sep=""))
  device.info <- ImportTrackerInfo(bird.data[i,"logger"])
  gps.data.list[[i]] <- gps.data
  acc.data.list[[i]] <- acc.data
  device.infos[[i]] <- device.info
}