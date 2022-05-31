# achieve the same as 0_Load_tracking_data_from_uvabits_database.R but from existing downloaded files (as re-downloading the data takes a loooooooong time):

# most data has already been downloaded but is in seperate files
load("data/raw/Imported.gps.acc.data.2012-2015.RData")
gps.data.list.2012.2015 <- gps.data.list
names(gps.data.list.2012.2015) <- sapply(gps.data.list.2012.2015, function(x) x$device_info_serial[1])
acc.data.list.2012.2015 <- acc.data.list
names(acc.data.list.2012.2015) <- sapply(acc.data.list.2012.2015, function(x) x$device_info_serial[1])
device.infos.2012.2015 <- device.infos
load("data/raw/Imported.gps.acc.data.2016-2019.RData")
gps.data.list.2016.2019 <- gps.data.list
names(gps.data.list.2016.2019) <- sapply(gps.data.list.2016.2019, function(x) x$device_info_serial[1])
acc.data.list.2016.2019 <- acc.data.list
names(acc.data.list.2016.2019) <- sapply(acc.data.list.2016.2019, function(x) x$device_info_serial[1])
device.infos.2016.2019 <- device.infos

# Load the new birddata file (as in the previously loaded RData files, there are different birddata files) 
bird.data <- read.csv("data/raw/bird.data.csv")

# create new gps.data.list and acc.data.list and device.infos and fill as much as possible with the above data:
gps.data.list <- vector(mode="list", length=dim(bird.data)[1])
names(gps.data.list) <- bird.data$birdID
acc.data.list <- vector(mode="list", length=dim(bird.data)[1])
names(acc.data.list) <- bird.data$birdID
device.infos <- ImportTrackerInfo(760) # to get the columns required for device info

# fill the lists with the data of 2012-2015:
for (i in 1:length(gps.data.list.2012.2015)) {
  logger <- names(gps.data.list.2012.2015)[i]
  gps.data.list[[logger]] <- gps.data.list.2012.2015[[i]] 
  acc.data.list[[logger]] <- acc.data.list.2012.2015[[i]] 
}
# then fill the remaining spots in the list with the data of 2016-2019; but by doing so, check if a certain spot is empty or not. If it is not empty, then append the data to this spot. 
for (i in 1:length(gps.data.list.2016.2019)) {
  logger <- names(gps.data.list.2016.2019)[i]
  if (is.null(gps.data.list[[logger]])) gps.data.list[[logger]] <- gps.data.list.2016.2019[[i]] else
    gps.data.list[[logger]] <- rbind(gps.data.list[[logger]],gps.data.list.2016.2019[[i]])
  if (is.null(acc.data.list[[logger]])) acc.data.list[[logger]] <- acc.data.list.2016.2019[[i]] else
    acc.data.list[[logger]] <- rbind(acc.data.list[[logger]],acc.data.list.2016.2019[[i]])
}

# combine the device infos of 2012-2015 and 2016-2019 and only keep unique cases (i.e. one row for each device)
device.infos <- rbind(device.infos.2012.2015, device.infos.2016.2019)
device.infos <- unique(device.infos)
# somehow, 763 exists with and without version info
rm(gps.data.list.2012.2015, gps.data.list.2016.2019, acc.data.list.2012.2015, acc.data.list.2016.2019, device.infos.2012.2015, device.infos.2016.2019)

# check if things went OK:
for (i in 1:length(gps.data.list)) print(c(dim(gps.data.list[[i]])[1],names(gps.data.list)[i]))
for (i in 1:length(acc.data.list)) print(c(dim(acc.data.list[[i]])[1],names(acc.data.list)[i]))

# data of three birds are missing: 761, 758 and 736 (corresponding to rows 2, 4 and 5 in the bird.data file); no or hardly any acc data was collected for these birds. That's why they were previously excluded from downloading 
for (i in c(2,4,5)) {
  print(bird.data[i,"logger"])
  gps.data <- ImportGPSDataBird(bird.data[i,"logger"], paste(bird.data[i,"year.start"], "-01-01 00:00:00", sep=""), paste(bird.data[i,"year.end"], "-01-01 00:00:00", sep=""))
  acc.data <- ImportAccDataBird(bird.data[i,"logger"], paste(bird.data[i,"year.start"], "-01-01 00:00:00", sep=""), paste(bird.data[i,"year.end"], "-01-01 00:00:00", sep=""))
  device.info <- ImportTrackerInfo(bird.data[i,"logger"])
  gps.data.list[[i]] <- gps.data
  acc.data.list[[i]] <- acc.data
  device.infos <- rbind(device.infos,device.info)
}

device.infos <- device.infos[-30,] # remove duplicate data of 763

# all data of 6284 has ended up in the first entry in the list, while its data from 2017 (when the tracker was reused on a new bird) onward should be moved to the second entry
names(gps.data.list)[names(gps.data.list)=="6284"][2]<-"6284.2"
names(acc.data.list)[names(acc.data.list)=="6284"][2]<-"6284.2"
# add the 2017+ data to the 6284.2 list entry
gps.data.list[["6284.2"]]<-gps.data.list[["6284"]][year(gps.data.list[["6284"]]$date_time)>2016,]
acc.data.list[["6284.2"]]<-acc.data.list[["6284"]][year(acc.data.list[["6284"]]$date_time)>2016,]
# remove the 2017+ data from the 6284 list entry
gps.data.list[["6284"]]<-gps.data.list[["6284"]][year(gps.data.list[["6284"]]$date_time)==2016,]
acc.data.list[["6284"]]<-acc.data.list[["6284"]][year(acc.data.list[["6284"]]$date_time)==2016,]

names(gps.data.list)[names(gps.data.list)=="1609"]<-"656"
names(acc.data.list)[names(acc.data.list)=="1609"]<-"656"
names(gps.data.list)[names(gps.data.list)=="6282"]<-"656"
names(acc.data.list)[names(acc.data.list)=="6282"]<-"656"
