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
save.image("data/processed/gps.acc.data.linked.RData")
load("data/processed/gps.acc.data.linked.RData")

sapply(gps.acc.data.list, function(x) dim(x)[1])

# temporary code for NIOZ cluster (tmux session is named sexfor)
setwd("/export/data01/user/tlok/Tamar/")
load("RF.models.RData")
load("gps.acc.data.linked.RData")
source("functions.R")
library(randomforest)
library(plyr)
library(moments)
# end of temp code

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

save.image("gps.acc.data.predicted.behaviour.RData")
#rm(gps.acc.data.list)

save.image("data/processed/gps.acc.data.predicted.behaviour.RData")

# load the habitat shapefile of Schiermonnikoog and surroundings, made by the RUG Geodienst 
load("data/raw/ShapeFileSchier.RData")
rm(schier_new84_sel)

# link the gps.acc.data to breeding data, and determine nest and breeding phases. 
# 656 and 1609 are one and the same bird. These data should be combined. 

# link the gps.acc.data to habitat and calculate other variables. 


df.760 <- AllData(gps.acc.data.760, 1)
rm(gps.acc.data.760)
df.763 <- AllData(gps.acc.data.763, 2)
rm(gps.acc.data.763)
df.584 <- AllData(gps.acc.data.584, 3)
rm(gps.acc.data.584)
df.647 <- AllData(gps.acc.data.647, 4)
rm(gps.acc.data.647)
save.image("data.tmp.RData")

load("gps.acc.classified.651.656.6117.6118.6066.6068.RData")
load("gps.acc.classified.6067.RData")
load("gps.acc.classified.6066.RData") # deze na 6067 laden

df.651 <- AllData(gps.acc.data.651, 5)
rm(gps.acc.data.651)
df.656 <- AllData(gps.acc.data.656, 6)
rm(gps.acc.data.656)
df.6117 <- AllData(gps.acc.data.6117, 7)
rm(gps.acc.data.6117)
df.6118 <- AllData(gps.acc.data.6118, 8)
rm(gps.acc.data.6118)
df.6066 <- AllData(gps.acc.data.6066, 9) # er lijkt pas accelerometer data te zijn verzameld vanaf 18 mei 2014 (ipv 5 mei, toen de zender op de vogel ging)
rm(gps.acc.data.6066)
df.6067 <- AllData(gps.acc.data.6067, 10)
rm(gps.acc.data.6067)
df.6068 <- AllData(gps.acc.data.6068, 11)
rm(gps.acc.data.6068)
save.image("data.tmp.RData")

load("gps.acc.classified.birds.caught.2016-2018.RData")
df.1606 <- AllData(gps.acc.data.1606, 12)
rm(gps.acc.data.1606)
df.1608 <- AllData(gps.acc.data.1608, 13)
rm(gps.acc.data.1608)
# why not 1609 = 656 = 6282?
df.6282 <- AllData(gps.acc.data.6282, 14)
rm(gps.acc.data.6282)
df.6283 <- AllData(gps.acc.data.6283, 15)
rm(gps.acc.data.6283)
df.6284.2016 <- AllData(gps.acc.data.6284.2016, 16)
rm(gps.acc.data.6284.2016)
df.6285 <- AllData(gps.acc.data.6285, 17)
rm(gps.acc.data.6285)
df.6287 <- AllData(gps.acc.data.6287, 18)
rm(gps.acc.data.6287)
df.6288 <- AllData(gps.acc.data.6288, 19)
rm(gps.acc.data.6288)
df.6289 <- AllData(gps.acc.data.6289, 20)
rm(gps.acc.data.6289)
df.6291 <- AllData(gps.acc.data.6291, 21)
rm(gps.acc.data.6291)
df.6294 <- AllData(gps.acc.data.6294, 22)
rm(gps.acc.data.6294)
save.image("data.tmp.RData")

df.6284.2017 <- AllData(gps.acc.data.6284.2017, 23)
rm(gps.acc.data.6284.2017)
df.6298 <- AllData(gps.acc.data.6298, 24)
rm(gps.acc.data.6298)
df.6358 <- AllData(gps.acc.data.6358, 25)
rm(gps.acc.data.6358)
save.image("data.tmp.RData")

keep(df.760, df.763, df.584, df.647, df.651, df.656, df.6117, df.6118, df.6066, df.6067, df.6068, df.1606, df.1608, df.6282, df.6283, df.6284.2016, df.6285, df.6287, df.6288, df.6289, df.6291, df.6294, df.6284.2017, df.6298, df.6358, schier_new84_sel, sure=T)
save.image("All_data_df_acc_schier_map_14_12_2018.RData")

load("All_data_df_acc_schier_map_14_12_2018.RData")

# adjust behaviours (no longer needed if AllData function is re-run over the gps.acc.data files per bird, as the function has been adjusted to remove the doubly assigned behaviours other/walk, and fly-flap/fly-soar)
#adjust.behaviour <- function(df) {
#  df <- df[df$pred.behav.short!="4.walk"&df$pred.behav.short!="8.fly-soar",]
#  df$pred.behav.short <- as.character(df$pred.behav.short)
#  df$pred.behav.short[df$pred.behav.short=="8.fly-flap"] <- "7.fly"
#  df$pred.behav.short <- as.factor(df$pred.behav.short)
#  df
#}
#df.760 <- adjust.behaviour(df.760)
#df.763 <- adjust.behaviour(df.763)
#df.584 <- adjust.behaviour(df.584)
#df.647 <- adjust.behaviour(df.647)
#df.651 <- adjust.behaviour(df.651)
#df.656 <- adjust.behaviour(df.656)
#df.6117 <- adjust.behaviour(df.6117)
#df.6118 <- adjust.behaviour(df.6118)
#df.6066 <- adjust.behaviour(df.6066)
#df.6067 <- adjust.behaviour(df.6067)
#df.6068 <- adjust.behaviour(df.6068)
#df.1606 <- adjust.behaviour(df.1606)
#df.1608 <- adjust.behaviour(df.1608)
#df.6282 <- adjust.behaviour(df.6282)
#df.6283 <- adjust.behaviour(df.6283)
#df.6284.2016 <- adjust.behaviour(df.6284.2016)
#df.6287 <- adjust.behaviour(df.6287)
#df.6288 <- adjust.behaviour(df.6288)
#df.6289 <- adjust.behaviour(df.6289)
#df.6291 <- adjust.behaviour(df.6291)
#df.6294 <- adjust.behaviour(df.6294)
#df.6284.2017 <- adjust.behaviour(df.6284.2017)
#df.6298 <- adjust.behaviour(df.6298)
#df.6358 <- adjust.behaviour(df.6358)

# adjust the birdID of 6284 in 2016 (to make it different from the new female transmitter with tracker 6284 in 2017)
df.6284.2016$birdID <- "6284_1"
df.6284.2017$birdID <- "6284_2"

# plot the shape file
ColHabitats = c('grey60','forestgreen','darkolivegreen','lightblue','lightblue','darkblue',
                'cyan','darkolivegreen2','cyan','forestgreen','lightblue','cyan','blue',
                'blue','lightgoldenrod3','darkolivegreen2','forestgreen','darkolivegreen','lightblue','lightblue')
windows()
#plot(schier_new84_sel, axes=TRUE, border=NA, col = ColHabitats[schier_new84_sel$HabitatNr], xlim=c(6.1,6.6), ylim=c(53.3,53.6))



dfb.760.2013 = determine.breeding.phases(df.760[df.760$yr==2013,], 140) # caught in 2012; estimated to be successful
dfb.584.2013 = determine.breeding.phases(df.584[df.584$yr==2013,], 175, 120, successful=FALSE) # caught in 2013 on 30/4, did not abandon the nest but a few days later, the breeding attempt failed and the bird renested; unsuccessful. 
dfb.763.2013 = determine.breeding.phases(df.763[df.763$yr==2013,], 131, day_hatched2=177, successful=FALSE) # caught in 2012; estimated to be successful
dfb.656.2013 = determine.breeding.phases(df.656[df.656$yr==2013,], 146, 120) # caught in 2013 on 30/4, successful (zat net een dag op de eieren toen hij gevangen werd!)
dfb.647.2013 = determine.breeding.phases(df.647[df.647$yr==2013,], 134, 123) # caught in 2013 on 3/5, successful, 16 mei hadden alle nesten kuikens van 2 tot 13 dagen oud, dus uiterlijke hatchdatum is 14 mei=day134
dfb.651.2013 = determine.breeding.phases(df.651[df.651$yr==2013,], 143, 129) # caught in 2013 on 9/5; 25-day old chick at 17/6, teruggerekend: 23mei hatched=day 143

# look at nest attendance of 2014 birds:
dfb.760.2014 = determine.breeding.phases(df.760[df.760$yr==2014,], 146) # caught in 2012: successful (1st chick hatched on 26 May = day 146)
dfb.763.2014 = determine.breeding.phases(df.763[df.763$yr==2014,], 137) # caught in 2012, successful, hatchday=17mei=day 137
dfb.651.2014 = determine.breeding.phases(df.651[df.651$yr==2014,], 149) # caught in 2013, successful, hatchday=29mei=149, broedt op Vlieland!!!
dfb.656.2014 = determine.breeding.phases(df.656[df.656$yr==2014,], 141, day_hatched2=180, successful=FALSE) # caught in 2013, unsuccessful, hatchday=21mei=day 141; had nog een tweede breeding attempt, maar ook niet succesvol, hatchday2 estimated at day  (ook weer direct van Schier kwelder na falen tweede nest).
dfb.6117.2014 = determine.breeding.phases(df.6117[df.6117$yr==2014,], 121, 119) # caught in 2014 on 29/4 (so misses first part of incubation), successful (but chicks were ringed at very young age! - this is true for all ringed chicks...), hatchday=1 mei = day 121
dfb.6118.2014 = determine.breeding.phases(df.6118[df.6118$yr==2014,], 165, 120) # caught in 2014 on 30/4, abandoned first nesting attempt but then renested (succesful?); hatchday moet ongeveer 165 zijn...
dfb.6066.2014 = determine.breeding.phases(df.6066[df.6066$yr==2014,], 143, 125) # caught in 2014 on 5/5 (so misses first part of incubation), successful, hatchday=23 mei=day 143
dfb.6067.2014 = determine.breeding.phases(df.6067[df.6067$yr==2014,], 180, 133, successful=FALSE) # caught in 2014 on 13/5, abandoned first nest but renested (probably unsuccessful), chick data is missing, but hatchday must be around 180
dfb.6068.2014 = determine.breeding.phases(df.6068[df.6068$yr==2014,], 128, 125) # caught in 2014 on 5/5 (so misses first part of incubation), successful, hatchday=8mei=day 128; 

dfb.763.2015 = determine.breeding.phases(df.763[df.763$yr==2015,], 134) # caught in 2012, successful, hatchday=14mei=day134, DATA ARE CORRUPT as frequencies 2d-speed different and hardly any GPS-fixes on nest
dfb.651.2015 = determine.breeding.phases(df.651[df.651$yr==2015,], 160) # caught in 2013, successful?, hatchday=9jun=day160 (broedt op Vlieland!)

#df.656_1609.2015 = determine.breeding.phases(df.656_1609[df.656_1609$yr==2015,], 144) # reloggered with logger 6019 in 2015 on 5/5. Successful. Heeft nog even voor zijn 'echte' attempt een paar dagen op een plek gezeten, vlakbij zijn uiteindelijke nestplek.
dfb.6118.2015 = determine.breeding.phases(df.6118[df.6118$yr==2015,], 162) # caught in 2014, successful, hatchday=11jun=day162
dfb.6066.2015 = determine.breeding.phases(df.6066[df.6066$yr==2015,], 188, successful=FALSE) # caught in 2014, unsuccessful, hatchday=7juli=188
dfb.6067.2015 = determine.breeding.phases(df.6067[df.6067$yr==2015,], 159, successful=FALSE) # caught in 2014, Unsuccessful (op basis van het verlaten van Schier als het jong 30 dagen oud is: dan is ienog lang niet klaar voor de overstap naar het Lauwersmeer!), echter wel een chick ringed at 29 days  old (day 188, 7 july) with mass 1530,and tarsus 129,9  hatchday=8jun=day159, so maybe succesfull after all
dfb.1606.2015 = determine.breeding.phases(df.1606[df.1606$yr==2015,], 136, 135) # caught in 2015 on 15/5, 1 jong geringd op dag 162 (11 juni), precies de dag waarop de oudervogel naar een andere plek is verhuisd. Om vanaf daar te voeren? Jong is nog wel jong (26) dagen om een afstand van 100 meter af te leggen van zijn nest naar de voederplek; aangenomen dat het jong nog leefde. JONG IS TERUGGEZIEN OP 4 AUG 2015: DUS WEL DEGELIJK IN LEVEN!
# als 1606 niet succesvol was is het wel gek dat hij zo lang nog op SChier bijft. (maar ok als hij wel succesvol is, blijft hij er wel erg lang hangen, veel langer dan andere vogels met jongen
dfb.1608.2015 = determine.breeding.phases(df.1608[df.1608$yr==2015,], 136, 135) # caught in 2015 on 15/5,  successful (jong teruggezien op 4 aug in Lauwersmeer), hatchday=16mei=day 136
#df.1609.2015 = determine.breeding.phases(df.1609[df.1609$yr==2015,], 144, 135) # een nieuwe logger opgekregen op 15/5, dit was 656, successful, hatchday=24mei=144

# 2016
dfb.763.2016 = determine.breeding.phases(df.763[df.763$yr==2016,], 175) # caught in 2012, successful (with partner 6291), hatchday=24juni=day175, DATA ARE CORRUPT as frequencies 2d-speed different and hardly any GPS-fixes on nest
dfb.6291.2016 = determine.breeding.phases(df.6291[df.6291$yr==2016,], 175) # caught in 2016 on 16/5; heeft na vangst nest verlaten, maar is opnieuw begonnen met 763; hatchday=24jun=day175
dfb.651.2016 = determine.breeding.phases(df.651[df.651$yr==2016,], 140) # caught in 2013, not successful, hatchday 19 of 20 mei (day139 or 140), as 2 nests were potential nests of 651 (4 chicks were labelled; but no longer in their own nest, with estimated hatchdates at 20,20,21 en 22 mei). (broedt op Vlieland) 
dfb.6118.2016 = determine.breeding.phases(df.6118[df.6118$yr==2016,], 153) # caught in 2014, successful, hatchday=2jun=day153
dfb.6067.2016 = determine.breeding.phases(df.6067[df.6067$yr==2016,], 161) # caught in 2014, successful, hatchday=10jun=day161
dfb.6282.2016 = determine.breeding.phases(df.6282[df.6282$yr==2016,], 153) # caught in 2016 on 27/4; is zelfde vogel als 656_1609! heeft nest na vangst verlaten, en is opnieuw begonnen, hatchday=2juni=day153
dfb.6283.2016 = determine.breeding.phases(df.6283[df.6283$yr==2016,]) # caught in 2016 on 28/4; heeft nest na vangst verlaten en heeft herlegsel gehad in Westerplas! Hatchdate onbekend.
dfb.6284.2016 = determine.breeding.phases(df.6284.2016[df.6284.2016$yr==2016,], 158) # caught in 2016 on 28/4; heeft nest na vangst verlaten en herlegsel gemaakt in Romke kolonie; hatchdate 7 juni = day158
dfb.6285.2016 = determine.breeding.phases(df.6285[df.6285$yr==2016,], 162) # caught in 2016 on 2/5; heeft nest na vangst verlaten (was partner van 6287) en herlegsel gemaakt in Romke kolonie; hatchdate 11 juni = day162
dfb.6287.2016 = determine.breeding.phases(df.6287[df.6287$yr==2016,], 165, successful=F) # caught in 2016 on 3/5; partner 6285 was daarna gevangen, en heeft 6287 en zijn nest verlaten; 6287 is opnieuw begonnen bij de 3e slenk, maar niet succesvol. Hatchdate 14juni=day165. His three labelled young were found dead on 8/7.
dfb.6288.2016 = determine.breeding.phases(df.6288[df.6288$yr==2016,], 132) # caught in 2016 on 12/5 (partner 6289); hatchdate 12/5=day132 (chick had not yet hatched when 6288 was caught; but was later estimated to have hatched on 11/5)
dfb.6289.2016 = determine.breeding.phases(df.6289[df.6289$yr==2016,], 132) # caught in 2016 on 15/5 (partner 6288); hatchdate 12/5=day132
dfb.6294.2016 = determine.breeding.phases(df.6294[df.6294$yr==2016,]) # caught in 2016 on 28/4; na vangst nest verlaten en zat toen in Westerplas; heeft niet gebroed.

# 2017
dfb.6283.2017 = determine.breeding.phases(df.6283[df.6283$yr==2017,], 125) # caught in 2016; heeft in Westerplas gebroed. Hatchdate 5/5 = day125
dfb.6284.2017 = determine.breeding.phases(df.6284.2017[df.6284.2017$yr==2017,], 162) # caught in 2017; heeft nest na vangst verlaten en herlegsel gemaakt met dezelfde partner (6288). Hatchdate 11/6=day162
dfb.6288.2017 = determine.breeding.phases(df.6288[df.6288$yr==2017,], 162) # caught in 2016; partner 6284 caught in 2017, who left the nest, but later renested with again 6288. Hatchdate 2nd nest: 11/6
# data of 6288 became corrupt during egg incubation stage: exclude for analysis
dfb.6287.2017 = determine.breeding.phases(df.6287[df.6287$yr==2017,], 162) # caught in 2016, Hatchdate 11/6=day162
dfb.6289.2017 = determine.breeding.phases(df.6289[df.6289$yr==2017,], 153, successful=F) # caught in 2016; partner was 6310 but he deserted the nest after capture (op nest 178). Eieren gelotterd op 26/5/2017, toen was het verste ei 2 mm. Gemiddeld duurt het dan nog een week voor het ei uitkomt. Dus op basis van lotterdata is hatchdate 2/6/2017 = day153. 6310 is gevangen op 30/5 en daarna heeft 6289 nog 3 dagen gebroed. Deze dagen moeten uit de analyse worden gehaald! 
dfb.6289.2017[[1]]$breeding.phase[dfb.6289.2017[[1]]$doy_CEST>149&dfb.6289.2017[[1]]$doy_CEST<156] <- "exclude"
# 6289 renested... maar was uiteindelijk niet succesvol... kuikenmetingen zijn niet ingevoerd!  
dfb.6294.2017 = determine.breeding.phases(df.6294[df.6294$yr==2017,], 137) # caught in 2016; broedde dit jaar in Westerplas waar ze op cameraval is vastgelegd; hatchdate = 24/5-7=17/5 (op basis van beelden cameraval)??? (kuikenmetingen niet ingevoerd ?! nest 130). Onbekend of ze succesvol jongen heeft grootgebracht. Op basis van nest attendance patroon lijkt het van wel. 
dfb.6118.2017 = determine.breeding.phases(df.6118[df.6118$yr==2017,], 174, successful=F) # caught in 2014; unsuccessful, hatchday=23jun=day174
dfb.6066.2017 = determine.breeding.phases(df.6066[df.6066$yr==2017,], 124) # caught in 2014, successful, hatchday=4mei=day124
dfb.6067.2017 = determine.breeding.phases(df.6067[df.6067$yr==2017,]) # caught in 2014, unsuccessful, hatchday? (waarschijnlijk nooit eieren gehad...)
dfb.763.2017 = determine.breeding.phases(df.763[df.763$yr==2017,], 183) # caught in 2012, had two breeding attempts this season, both with 760, and both with only one egg in the nest; hatchdate 2jul=day183
dfb.6291.2017 = determine.breeding.phases(df.6291[df.6291$yr==2017,]) # caught in 2016; kwam dit jaar pas heel laat aan en heeft niet gebroed.
dfb.6298.2017 = determine.breeding.phases(df.6298[df.6298$yr==2017,], 127) # caught in 2017 on 6/5; partner LLBT (which we didn't manage to catch). Hatchdate 7/5=day127
dfb.6310.2017 = determine.breeding.phases(df.6310[df.6310$yr==2017,]) # caught in 2017 on 30/5, was partner of 6289 but then deserted the nest :(. Moved to Westerplas, but didn't renest.

## 2018
dfb.6283.2018 = determine.breeding.phases(df.6283[df.6283$yr==2018,]) # caught in 2016; heeft in Westerplas gebroed. Hatchdate onbekend.
dfb.6284.2018 = determine.breeding.phases(df.6284.2017[df.6284.2017$yr==2018,], 146, successful=F) # caught in 2017; Hatchdate 26/5=day146
dfb.6289.2018 = determine.breeding.phases(df.6289[df.6289$yr==2018,], 144) # caught in 2016; Hatchdate 24/5=day144 
dfb.6294.2018 = determine.breeding.phases(df.6294[df.6294$yr==2018,]) # caught in 2016; heeft in Westerplas gebroed. Hatchdate onbekend. Nooit op Oosterkwelder geweest, daarom een error?
dfb.6118.2018 = determine.breeding.phases(df.6118[df.6118$yr==2018,], 176) # caught in 2014; successful, hatchday=25jun=day176. Chick has not been transmitted because hatched very late in the season.
dfb.6291.2018 = determine.breeding.phases(df.6291[df.6291$yr==2018,], 153) # caught in 2016; successful, hatchday=2jun=day153
dfb.6298.2018 = determine.breeding.phases(df.6298[df.6298$yr==2018,], 163, successful=F) # caught in 2017; unsuccessful, hatchday=12jun=163
dfb.6358.2018 = determine.breeding.phases(df.6358[df.6358$yr==2018,], 137) # caught on 14/5/2018; successful, hatchday=17may=137

### determine resting speed from the points of the birds at their nests (still with 5m radius!):
df.all.2013_2018 = rbind(dfb.760.2013[[1]], dfb.763.2013[[1]], dfb.656.2013[[1]], dfb.584.2013[[1]], dfb.647.2013[[1]], dfb.651.2013[[1]], 
                         dfb.760.2014[[1]], dfb.763.2014[[1]], dfb.656.2014[[1]], dfb.6117.2014[[1]], dfb.6066.2014[[1]], dfb.6067.2014[[1]], dfb.6068.2014[[1]], 
                         dfb.6118.2015[[1]], dfb.6066.2015[[1]], dfb.6067.2015[[1]], dfb.1606.2015[[1]], dfb.1608.2015[[1]],
                         dfb.6282.2016[[1]], dfb.6283.2016[[1]], dfb.6284.2016[[1]], dfb.6285.2016[[1]], dfb.6287.2016[[1]], dfb.6288.2016[[1]], dfb.6289.2016[[1]], dfb.6291.2016[[1]], dfb.6294.2016[[1]], dfb.6118.2016[[1]], dfb.6067.2016[[1]],
                         dfb.6283.2017[[1]], dfb.6284.2017[[1]], dfb.6287.2017[[1]], dfb.6289.2017[[1]], dfb.6294.2017[[1]], dfb.6298.2017[[1]], dfb.6118.2017[[1]], dfb.6066.2017[[1]], dfb.6067.2017[[1]],dfb.6283.2018[[1]],dfb.6284.2018[[1]],dfb.6289.2018[[1]],dfb.6118.2018[[1]],dfb.6291.2018[[1]],dfb.6298.2018[[1]],dfb.6358.2018[[1]])

# no breeding data for df.6118.2014, df.6294.2018 (hatchdate onbekend anyway)?

keep(df.all.2013_2018, schier_new84_sel, ColHabitats, sure=T)

save.image('Breeding.phases.data.sel.acc.RData')

#df.all.2013_2018 <- rbind(df.760.2013[[1]], df.584.2013[[1]], df.647.2013[[1]], df.760.2014[[1]])
load('Breeding.phases.data.sel.acc.RData')
table(df.all.2013_2018$pred.behav.short)
head(df.all.2013_2018[df.all.2013_2018$pred.behav.short=='4.other'|df.all.2013_2018$pred.behav.short=='4.walk',])

#
df.all.2013_2018$habitat_type = 'schier'
df.all.2013_2018$habitat_type[df.all.2013_2018$habitat=='LM_zoet'|df.all.2013_2018$habitat=='LM_land'|df.all.2013_2018$habitat=='wal_rest_zoet'|df.all.2013_2018$habitat=='wal_rest_land'] ='mainland'
df.all.2013_2018$habitat_type[df.all.2013_2018$habitat=='waddenzee'|df.all.2013_2018$habitat=='Wad_Kweldergeul_Brak'] ='wadden'
df.all.2013_2018$habitat_type[df.all.2013_2018$habitat=='Eilanden_Rest'|df.all.2013_2018$habitat=='Noordzee'] ='rest'
df.all.2013_2018$habitat_type[is.na(df.all.2013_2018$habitat)] = NA
unique(df.all.2013_2018[c('habitat','habitat_type')])


# categorize habitats as marine, brackish, freshwater, land, or NA (based on habitat map and Google Earth checks)
# We assume points on the Noordzee as outliers and don't assign them to a habitat.
df.all.2013_2018$habitat_salinity = NA
df.all.2013_2018$habitat_salinity[df.all.2013_2018$habitat=='Schier_Kwelder'|df.all.2013_2018$habitat=='wal_rest_land'|df.all.2013_2018$habitat=='LM_land'|df.all.2013_2018$habitat=='Schier_Land_Rest'|df.all.2013_2018$habitat=='Eilanden_Rest'] = 'land'
df.all.2013_2018$habitat_salinity[df.all.2013_2018$habitat=='waddenzee'] = 'marine'
df.all.2013_2018$habitat_salinity[df.all.2013_2018$habitat=='Schier_brak'|df.all.2013_2018$habitat=='Wad_Kweldergeul_Brak'] = 'brackish'
df.all.2013_2018$habitat_salinity[df.all.2013_2018$habitat=='LM_zoet'|df.all.2013_2018$habitat=='wal_rest_zoet'|df.all.2013_2018$habitat=='Schier_Zoet'] = 'freshwater'

# determine behaviour from acc data 
table(df.all.2013_2018$pred.behav.short)
df.all.2013_2018$behaviour[df.all.2013_2018$pred.behav.nr<4] = "foraging"
df.all.2013_2018$behaviour[df.all.2013_2018$pred.behav.short=="4.other"] = "other"
df.all.2013_2018$behaviour[df.all.2013_2018$pred.behav.short=="4.walk"] = "walking"
df.all.2013_2018$behaviour[df.all.2013_2018$pred.behav.nr==5|df.all.2013_2018$pred.behav.nr==6] = "resting"
df.all.2013_2018$behaviour[df.all.2013_2018$pred.behav.nr>6] = "flying"
table(df.all.2013_2018$behaviour, df.all.2013_2018$habitat)

## resting on nest (within 5 m of the nest position, dit wordt door de kolommen nest 1 en nest 2 aangegeven):
df.all.2013_2018$behaviour2 = df.all.2013_2018$behaviour
df.all.2013_2018$behaviour2[df.all.2013_2018$behaviour=='foraging'] = paste(df.all.2013_2018$behaviour[df.all.2013_2018$behaviour=='foraging'], df.all.2013_2018$habitat_salinity[df.all.2013_2018$behaviour=='foraging'], sep='_')
df.all.2013_2018$behaviour2[df.all.2013_2018$behaviour=='resting'] = paste(df.all.2013_2018$behaviour[df.all.2013_2018$behaviour=='resting'], df.all.2013_2018$habitat_type[df.all.2013_2018$behaviour=='resting'], sep='_')
df.all.2013_2018$behaviour2[df.all.2013_2018$nest1==1|df.all.2013_2018$nest2==1] = 'at_nest'
table(df.all.2013_2018$behaviour2)

df.all.2013_2018$speed_2d = round(df.all.2013_2018$speed_2d,2)

# add a column with sex
df.all.2013_2018$sex = ifelse(df.all.2013_2018$birdID==760|df.all.2013_2018$birdID==651|df.all.2013_2018$birdID==6068|df.all.2013_2018$birdID==6283|df.all.2013_2018$birdID=="6284_1"|df.all.2013_2018$birdID==6285|df.all.2013_2018$birdID==6289|df.all.2013_2018$birdID==6291|df.all.2013_2018$birdID==6294|df.all.2013_2018$birdID==6298|df.all.2013_2018$birdID=="6284_2","f","m")
unique(df.all.2013_2018[,c('birdID','birdID_new', 'sex')])

## numbering breeding phases:
df.all.2013_2018$breeding.phase.nr = 1
df.all.2013_2018$breeding.phase.nr[df.all.2013_2018$breeding.phase=='breeding'] = 2
df.all.2013_2018$breeding.phase.nr[df.all.2013_2018$breeding.phase=='eggs'] = 3
df.all.2013_2018$breeding.phase.nr[df.all.2013_2018$breeding.phase=='chicks'] = 4
df.all.2013_2018$breeding.phase.nr[df.all.2013_2018$breeding.phase=='fledglings'] = 5
df.all.2013_2018$breeding.phase.nr[df.all.2013_2018$breeding.phase=='post_breeding_successful'] = 6
df.all.2013_2018$breeding.phase.nr[df.all.2013_2018$breeding.phase=='post_breeding_unsuccessful'] = 7

df.all.2013_2018$breeding.phase2 = df.all.2013_2018$breeding.phase
df.all.2013_2018$breeding.phase = paste(df.all.2013_2018$breeding.phase.nr, df.all.2013_2018$breeding.phase, sep='.')
breeding.phase.cols = c('grey','white','dodgerblue','darkorange','darkorchid3','darkolivegreen3','red')

## add columns with week relative to the hatching day.
## change the attempt number of the pre-breeding phase, to make it the same attempt as the subsequent breeding attempt:
df.all.2013_2018$attempt[df.all.2013_2018$breeding.phase=='1.pre-breeding'] = df.all.2013_2018$attempt[df.all.2013_2018$breeding.phase=='1.pre-breeding']+1
df.all.2013_2018$week = week(df.all.2013_2018$date_time) # recalculate the week column because previously done on UTC date_time
df.all.2013_2018 = df.all.2013_2018[order(df.all.2013_2018$yr, df.all.2013_2018$birdID, df.all.2013_2018$doy_CEST),]
doys.yr.bird.phase = unique(df.all.2013_2018[,c('yr','birdID','sex','breeding.phase.nr','attempt','doy_CEST','week')])
dim(doys.yr.bird.phase) # 281 bird-days

doy.min.yr.bird.phase = aggregate(doy_CEST~birdID+sex+breeding.phase.nr+yr+attempt, data=doys.yr.bird.phase, min)
hatch_day_bird_yr = doy.min.yr.bird.phase[doy.min.yr.bird.phase$breeding.phase.nr==4,c('birdID','yr','attempt','doy_CEST')]
names(hatch_day_bird_yr)[4]='hatch_day'
df.all.2013_2018 = merge(df.all.2013_2018, hatch_day_bird_yr, all.x=T)
unique(df.all.2013_2018$hatch_day)
df.all.2013_2018$day_rel_hatching = df.all.2013_2018$doy_CEST-df.all.2013_2018$hatch_day
df.all.2013_2018$week_rel_hatching = floor(df.all.2013_2018$day_rel_hatching/7)

## Determine the total duration recorded per day, and remove the days with less than 23.1 hours of data (this can happen, as in a previous step we deleted points with a duration of more than 60 minutes)
# remove breeding phases that we will not use in the analysis (exclude, pre-breeding and breeding, i.e. breeding.phase.nr>2):
duration_doy_birdID_yr <- aggregate(duration/60~yr+birdID+sex+doy_CEST+week+breeding.phase+breeding.phase.nr, df.all.2013_2018[df.all.2013_2018$breeding.phase.nr>2,], sum)
dim(duration_doy_birdID_yr) # 260 bird-days
names(duration_doy_birdID_yr)[8] = 'dur_per_doy'
table(round(duration_doy_birdID_yr$dur_per_doy,1), duration_doy_birdID_yr$birdID)

## which days are removed when using the criterium of using only days with more than 23.1 hours of behaviours recorded?
duration_doy_birdID_yr[duration_doy_birdID_yr$dur_per_doy<=23.1,]

duration_doy_birdID_yr = duration_doy_birdID_yr[duration_doy_birdID_yr$dur_per_doy>23.1,] # remove days with summed durations of less than 23.1h 
dim(duration_doy_birdID_yr) ## 248 birddays left
duration_doy_birdID_yr = merge(doys.yr.bird.phase, duration_doy_birdID_yr[c('yr','birdID','doy_CEST','breeding.phase' ,'dur_per_doy')], by=c('yr','birdID','doy_CEST')) # to add the columns rel_day_hatching and rel_week_hatching to the duration_doy_birdID_yr dataframe.

df.sel.2013_2018 = merge(df.all.2013_2018, duration_doy_birdID_yr[,c('yr','birdID','doy_CEST')], by=c('birdID','yr','doy_CEST')) # remove the days with durations<=23.1 in the df.all.2013_2018 file.

df.sel.2013_2018 = df.sel.2013_2018[order(df.sel.2013_2018$yr, df.sel.2013_2018$birdID, df.sel.2013_2018$date_time),]

doys.yr.bird.phase = unique(df.sel.2013_2018[,c('yr','birdID','sex','breeding.phase.nr','attempt','doy_CEST','week','day_rel_hatching','week_rel_hatching')])

keep(breeding.phase.cols, ColHabitats, df.sel.2013_2018, doys.yr.bird.phase, duration_doy_birdID_yr, schier_new84_sel, sure=T)

save.image('Breeding.phases.data.sel.acc.CEST3.new.RData')
