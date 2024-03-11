# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

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

### create fixed segments of acc samples and calculate summary statistics over them
create.fixed.segments <- function(segment.length, data, sampling.freq=20) { # segment length is expressed in seconds
  
  samples.per.segment <- ceiling(segment.length * sampling.freq) 
  indices.to.use <- seq(min(data$Index),max(data$Index),by=20/sampling.freq)
  data.sel = data[data$Index %in% indices.to.use,] 

  data.sel$segment.id.cut <- paste(data.sel$obs.id, formatC(format="d", ceiling((data.sel$Index+1)/(segment.length*20)),flag="0",width=ceiling(log10(max(ceiling((data.sel$Index+1)/(segment.length*20)))))), sep = ".") # 20 is the sampling frequency at which the data was originally collected
  
  data.sel <- na.omit(data.sel)
  
  ## calculate summary statistics for each segment: 
  seg.df <- ddply(data.sel, .(segment.id.cut, birdID, date_time), summarize, 
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

  seg.df <- seg.df[seg.df$nobs.segments==samples.per.segment,] # only use segments of the specified segment length
  seg.df
}

from.list.to.df <- function(list) {   # change from list to dataframe
  df <- list[[1]]
  if (length(list)>1) for (i in 2:length(list)) df <- rbind(df, list[[i]])
  df
}

# function to determine nest coordinates and calculate nest attendance 
determine.breeding.phases <- function(df, day_hatched=NA, day_hatched2=NA, successful=0) {
  df <- na.omit(df) # this removes the points for which no habitat info is available, and the first and last point of a bird in each year, as duration could not be calculated (as time_until_previous or time_until_next was NA)
  df$lat_rnd=round(df$latitude, digit=5) 
  df$lon_rnd=round(df$longitude, digit=6) 
  
  # rough determination of breeding phases, irrespective of when eggs have been laid (the latter is only known for nests with known hatch dates)
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
  df$lat.nest1=coord.nest1$lat_rnd
  df$lon.nest1=coord.nest1$lon_rnd
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
  # determine whether the 1st nesting attempt was real (i.e. more than 5 days of >5 hours nest attendance, on Schier):
  nest1.attendance$more.than.5h = ifelse(nest1.attendance$hours.on.nest>=5,1,0)
  nest1.ndays.5h = sum(nest1.attendance$more.than.5h) # calculcates the number of days that this place is visited for more than 5 hours.
  # determine if breeding attempt 1 was a real breeding attempt (more than 5 days with more than 5 hours nest attendance):
  if (is.na(unique(df$habitat[df$nest1==1])[1])) df$nest1.real=0 else df$nest1.real = ifelse(nest1.ndays.5h>=5 & unique(df$habitat[df$nest1==1])[1]=='Schier_Kwelder', 1, 0)
  nest1.real = unique(df$nest1.real)

  # determine if there was another breeding attempt:
  df.rest = df[df$dist.nest1>50,] # should be outside the GPS error range of the primary breeding attempt
  df.rest<-df.rest[order(df.rest$date_time),]
  duration.per.coord2 = aggregate(duration~lat_rnd+lon_rnd, df.rest, sum)
  duration.per.coord2 = duration.per.coord2[order(duration.per.coord2$duration, decreasing=TRUE),][1:20,]
  coord.nest2 = duration.per.coord2[1,1:2] # determine coordinates nest as the coordinates most visited
  df$lat.nest2=coord.nest2$lat_rnd
  df$lon.nest2=coord.nest2$lon_rnd
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
  if (is.na(unique(df$habitat[df$nest2==1])[1])) df$nest2.real=0 else df$nest2.real = ifelse(nest2.ndays.5h>=5 & unique(df$habitat[df$nest2==1])[1]=='Schier_Kwelder', 1, 0)
  nest2.real <- unique(df$nest2.real)

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
      if (is.na(day_hatched)==F) phase.doy$breeding.phase[phase.doy$yday_CEST>=day_hatched & phase.doy$yday_CEST<=nest1.last.day.5h]='chicks'
      if (is.na(day_hatched2)==F) phase.doy$breeding.phase[phase.doy$yday_CEST>=day_hatched2 & phase.doy$yday_CEST<=nest2.last.day.5h]='chicks' # if nest2.last.day.5h=NULL, this code does nothing
      if (nest2.real==1) phase.doy$breeding.phase[phase.doy$yday_CEST > max(nest1.last.day.5h,nest2.last.day.5h)]='post.breeding.unsuccessful' else 
        phase.doy$breeding.phase[phase.doy$yday_CEST > nest1.last.day.5h]='post.breeding.unsuccessful'  
    }

    # Assigning attempts, so that pre-breeding is counted within the breeding attempt: 
    phase.doy$attempt = 1
    for (i in 2:dim(phase.doy)[1]) {
      if ((phase.doy$breeding.phase[i]==phase.doy$breeding.phase[i-1]) |
          (phase.doy$breeding.phase[i]=='eggs'&phase.doy$breeding.phase[i-1]=='pre-breeding') |
          (phase.doy$breeding.phase[i]=='chicks'&phase.doy$breeding.phase[i-1]=='eggs') |
          (phase.doy$breeding.phase[i]=='post.breeding.successful'&phase.doy$breeding.phase[i-1]=='chicks') |
          (phase.doy$breeding.phase[i]=='post.breeding.unsuccessful'&phase.doy$breeding.phase[i-1]=='chicks'))
        phase.doy$attempt[i]=phase.doy$attempt[i-1]
      else phase.doy$attempt[i]=phase.doy$attempt[i-1]+1 
    }

    table(phase.doy$breeding.phase) # to check that egg phase is indeed 25 days, and chick phase 30 days. 
    
    # merge breeding phase and nest coordinate info with df 
    df = merge(df, phase.doy, by='yday_CEST', all.x=T)
    df = df[order(df$date_time),]
  }
  
  # if hatchday is unknown, add the columns that were not created as the above code was not run, and fill them with NA (so that in the structure of the df will be exactly the same, for rbinding them later on)
  if (is.na(day_hatched)&is.na(day_hatched2)) {
    df$attempt <- NA
    df$breeding.phase <- NA
  }
  
  # calculate distance between subsequent points:
  df$distance.to.prev[2:dim(df)[1]] = round(distCosine(df[1:(dim(df)[1]-1),c('longitude','latitude')], df[2:dim(df)[1],c('longitude','latitude')], r=6378.137),3)
  df$distance.to.next = c(df$distance.to.prev[2:dim(df)[1]],NA)
  
  df[,-which(names(df) %in% c("catch.date","lat_rnd","lon_rnd","dist.nest1","nest1.1m","dist.nest2","nest2.1m"))]
}

# make a neat table for manuscript from dredge output
make.table.from.dredge.output <- function(dredge_output) {
  table.output <- as.data.frame(dredge_output)
  for (i in 1:dim(table.output)[1]) {
    expl.vars = table.output[i,2:(which(names(table.output)=='df')-1)]
    expl.vars = names(expl.vars)[!is.na(expl.vars)]
    model.formula = paste(expl.vars,collapse=" + ")
    table.output$model.name[i] <- model.formula
  }
  table.output$'-2logL' <- table.output$logLik * -2
  table.output$'d-2logL' <- table.output$'-2logL' - min(table.output$'-2logL')
  table.output <- table.output[,c('model.name','df','d-2logL','delta','weight')]
  names(table.output)[c(2,4,5)] <- c('K','dAICc','Akaike.weight')
  table.output[,3:5] <- format(round(table.output[,3:5],2), trim=T)
  table.output
}

plot.sign.arrow <- function(x1=1, x2=2, ymin=0, ymax=1, yrel=0.9, sign="***", col="black", fontsize=1.5, dist.text=0.015) {
  arrows(x1,ymin+yrel*(ymax-ymin),x2,ymin+yrel*(ymax-ymin), length=0, code=1, col=col)
  arrows(x1,ymin+yrel*(ymax-ymin),x1,ymin+(yrel-0.015)*(ymax-ymin), length=0, code=1, col=col)
  arrows(x2,ymin+yrel*(ymax-ymin),x2,ymin+(yrel-0.015)*(ymax-ymin), length=0, code=1, col=col)
  text(x1+0.5*(x2-x1),ymin+(yrel+dist.text)*(ymax-ymin), sign, col=col, cex=fontsize)
} 

plot.sign.prop.for <- function(ymin, ymax, yrel=0.9, signs = c("***", "***", "***", "***", "***"), col="black", dist.text=0.015, fontsize=1.5) {
  plot.sign.arrow(1, 1.3, ymin=ymin, ymax=ymax, yrel=yrel, sign=signs[1], dist.text=dist.text, fontsize=fontsize)
  plot.sign.arrow(2, 2.3, ymin=ymin, ymax=ymax, yrel=yrel, sign=signs[2], dist.text=dist.text, fontsize=fontsize)
  plot.sign.arrow(3, 3.3, ymin=ymin, ymax=ymax, yrel=yrel, sign=signs[3], dist.text=dist.text, fontsize=fontsize)
  plot.sign.arrow(4, 4.3, ymin=ymin, ymax=ymax, yrel=yrel, sign=signs[4], dist.text=dist.text, fontsize=fontsize)
  plot.sign.arrow(5, 5.3, ymin=ymin, ymax=ymax, yrel=yrel, sign=signs[5], dist.text=dist.text, fontsize=fontsize)
}

plot.sign.for.trips <- function(ymin=0, ymax, yrel=0.9, signs = c("***", "***", "***", "***"), fontsize=1.5) {
  plot.sign.arrow(1, 1.3, ymin=ymin, ymax=ymax, yrel=0.9, sign=signs[1])
  plot.sign.arrow(2, 2.3, ymin=ymin, ymax=ymax, yrel=0.9, sign=signs[2])
  plot.sign.arrow(1, 2, ymin=ymin, ymax=ymax, yrel=0.93, sign=signs[3], col="lightcoral")
  plot.sign.arrow(1.3, 2.3, ymin=ymin, ymax=ymax, yrel=0.96, sign=signs[4], col="lightskyblue")
}

# retrieve AIC and deviance from a bam model 
AIC_deviance <- function(x) {
  K = sum(x$edf)
  aic_value = x$aic
  aic_value_alt = AIC(x)
  AICc_value = AICc(x)
  round(c(K=K, AIC1=aic_value, AIC2=aic_value_alt, AICc=AICc_value),2)
}