con<-odbcConnectAccess2007("C:/Users/tlok/Documents/LepelaarDB/LepelaarDB_linked.accdb")
# for survival analysis:
#ringings = sqlFetch(con, "Qy_CaptureOverview_all")
#resightings = sqlFetch(con, "Qy_SightingOverview_all")
#save.image("data/raw/ringing_resighting_data.RData")
load("data/raw/ringing_resighting_data.RData")

# for breeding success analysis:
BS_nest = sqlFetch(con, "Schier_BS_step1")
parents_nest = sqlFetch(con, "Schier_parents_nests")
labelled_chicks = sqlFetch(con, "Schier_labelled_chicks")
odbcClose(con)

# SURVIVAL ANALYSIS 

## select birds with a GPS tracker: 
bird_data_tagged <- aggregate(FldDate~BirdID+TransmitterNr+ColourCode+Ringnumber+Location+YearRinged+FldDate+Country, ringings[is.na(ringings$TransmitterNr)==F & ringings$YearRinged%in%2012:2018 & ringings$Country=="The Netherlands" & ringings$Ageclass=='adult',], min)
dim(bird_data_tagged)
bird_data_tagged <- bird_data_tagged[order(bird_data_tagged$FldDate, bird_data_tagged$TransmitterNr),] # for this, I could also use the bird.data.csv file... 
bird_data_tagged$YearTagged <- bird_data_tagged$YearRinged
bird_data_tagged <- aggregate(cbind(YearTagged, TransmitterNr)~BirdID, bird_data_tagged, min) # 31 unique birds
names(bird_data_tagged)[3]='TagID'
## resightings
resightings$Year = year(resightings$Date)
resightings$Month = month(resightings$Date)
# only select resightings of birds in bird_data_uni:
resightsel_tagged <- merge(resightings, bird_data_tagged, by='BirdID')
table(resightsel_tagged$Year)
# select only resightings in March - October in The Netherlands:
resightsel_tagged <- resightsel_tagged[resightsel_tagged$Country=="The Netherlands" & resightsel_tagged$Month%in%3:10,]
# start encounter history in the year that the bird received its tracker: 
resightsel_tagged <- resightsel_tagged[resightsel_tagged$Year>resightsel_tagged$YearTagged,]
# select columns used in analysis:
names(bird_data_tagged)[2]="Year"
bird_data_tagged$tag_type = "heavy"
bird_data_tagged$tag_type[bird_data_tagged$Year>2013] = "light"

resightsel_tagged <- resightsel_tagged[,c('BirdID','Year')]
# combine tagging and resighting data of tagged birds, where recaptures are considered resightings:
cmr_data_tagged <- unique(rbind(bird_data_tagged[,1:2], resightsel_tagged))

# compare the GPS-tracked birds with colour-ringed birds that bred at least once on Schiermonnikoog, Oosterkwelder in the period 2012-2018.
table(resightings$Behaviour)
table(resightings$Age) # check the negative ages! presumably due to recaptures, involving the GPS-tracked birds...
# link data of tagged birds to resightings data to remove those birds from the untagged bird group (otherwise, birds enter into the untagged group until the moment they become tagged):
resightings <- merge(resightings, bird_data_tagged[,c('BirdID','tag_type')], by='BirdID', all.x=T)
resightings <- resightings[is.na(resightings$tag_type),]
resightings_breeders_Schier <- resightings[resightings$Age>2 & resightings$Location=="Schiermonnikoog, Oosterkwelder" & resightings$Year%in%2012:2018 & resightings$Behaviour%in%c("breeding","brings branches to the nest","chick guiding","copulating","feeds (parent) or being fed (young)","incubating","standing on nest","starts breeding (takes over from partner","quits breeding (is taken over by partner)","nest building","attack (parent) or being attacked (chick)"),c('BirdID','ColourCode','FldDate','Month','Year','Age','Location','Latitude','Longitude','Country')]
table(resightings_breeders_Schier$Age)
# use first resighting as an adult in this period as the "capture event":
first_resighting_breeder <- aggregate(Year~BirdID, resightings_breeders_Schier, min) 
names(first_resighting_breeder)[2]="YearFirstSeenAsBreeder"
dim(first_resighting_breeder) # 196 birds
# then use all resightings on Schier in the months March - October irrespective of whether they were showing 'breeding behaviour' as summer resightings.
resightsel_untagged <- merge(resightings, first_resighting_breeder, by='BirdID')
# select only resightings in March - October in The Netherlands:
resightsel_untagged <- resightsel_untagged[resightsel_untagged$Country=="The Netherlands" & resightsel_untagged$Month%in%3:10,]
# start encounter history at the first resighting as breeder and only select columns used in analysis:
resightsel_untagged <- unique(resightsel_untagged[resightsel_untagged$Year>resightsel_untagged$YearFirstSeenAsBreeder,c('BirdID','Year')])
bird_data_untagged <- first_resighting_breeder
names(bird_data_untagged)[2] <- 'Year'
bird_data_untagged$tag_type="none"
# combine tagging and resighting data of untagged birds:
cmr_data_untagged <- rbind(bird_data_untagged[,1:2], resightsel_untagged)

# combine data of tagged and untagged adults:
cmr_data <- rbind(cmr_data_tagged, cmr_data_untagged)
cmr_data$freq=1

# only select data until 2020:
cmr_data <- cmr_data[cmr_data$Year<2021,]

## 
cr = xtabs(freq~BirdID+Year, data=cmr_data)
head(cr) ## 2012-2020
ch<-data.frame(ch=apply(cr,1,paste,collapse=""), BirdID = row.names(cr))
ch$ch=as.character(ch$ch)
ch = merge(ch, rbind(bird_data_tagged[,c('BirdID','Year','tag_type')], bird_data_untagged))
ch = ch[order(ch$ch, decreasing=T),]
ch$tag_type = as.factor(ch$tag_type)
table(ch$tag_type) ## 196 untagged adults and 31 tagged adults (12 with heavy tag, 19 with a light tag)

ch[ch$tag_type%in%c('light','heavy'),]
write.table(ch, "data/raw/ch_data_tagged_untagged_adults.txt", row.names = F)

# survival analysis investigating tag effect on survival (explorative analysis showed that survival was best modelled as constant rather than with annual variation or a linear time trend)
proc = process.data(ch, begin.time= 2012, model = "CJS", groups="tag_type")
release.gof(proc) # TEST2 and TEST3 were not significant.
ddl = make.design.data(proc)
ddl$Phi$tag = ifelse(ddl$Phi$tag_type=='none','no','yes')
ddl$p$tag = ifelse(ddl$p$tag_type=='none','no','yes')
Phi.c = list(formula=~1)
Phi.tag = list(formula=~tag)
p.t = list(formula=~time)
cml = create.model.list("CJS")
models.run = mark.wrapper(cml, data=proc, ddl=ddl, begin.time=2012, adjust=T)
write.csv(models.run$model.table, "output/TableSX - Model selection survival.csv")
Phi.modavg = model.average(models.run, "Phi", vcv=T)$estimate
Phi.modavg$tag = ifelse(Phi.modavg$tag_type=='none','no','yes')
unique(Phi.modavg[,c('estimate','lcl','ucl','tag')])
p.modavg = model.average(models.run, "p", vcv=T)$estimate
p.modavg = unique(p.modavg[,c('time','estimate','lcl','ucl')])
write.csv(p.modavg, "output/TableS4 - p estimates.csv")

# BREEDING SUCCESS
BS_nest$Year = year(BS_nest$HatchDate)
BS_nest_sel <- BS_nest[which(BS_nest$Year>2015),]
parents_nest[which(parents_nest$TagID==760),]
nests_tagged_birds = na.omit(parents_nest[parents_nest$TagID>0,])
nests_tagged_birds[nests_tagged_birds$Year==2018,] # 6298 is in the DB with its original code, which does not have the metal ring. This ring was added in 2018.
nests_tagged_birds$ColourCode[nests_tagged_birds$TagID==6298]="B[Y6]/L[Y6]a"
# link to tagging data so that the tagged birds are only included AFTER they received their tag (i.e. from the tagging year onward):
bird.data$year.start = year(dmy(bird.data$start_deployment))
bird.data$year.end = year(dmy(bird.data$end_deployment))
nests_tagged_birds = merge(nests_tagged_birds, bird.data[,c('colourcode','logger','year.start')], by.x=c('ColourCode','TagID'), by.y=c('colourcode','logger'))
nests_tagged_birds = nests_tagged_birds[order(nests_tagged_birds$Year, nests_tagged_birds$BirdID),]
nests_tagged_birds = nests_tagged_birds[nests_tagged_birds$Year>=nests_tagged_birds$year.start,]
# per bird and year, the nest with the highest ID should be chosen in case of renesting after we caught the partner (or the bird itself) who deserted the nest:
nests_tagged_birds = aggregate(NestID~Year+TagID, nests_tagged_birds, max)
nests_tagged_birds = nests_tagged_birds[order(nests_tagged_birds$TagID),]
breeding.data <- read.csv("data/raw/breeding.data.csv")
breeding.data <- breeding.data[breeding.data$used==1,] # only select years with suitable data
breeding.data$hatchdate[breeding.data$hatchdate==""] <- NA
breeding.data$hatchday[is.na(breeding.data$hatchdate)==F]<-yday(dmy(breeding.data$hatchdate[is.na(breeding.data$hatchdate)==F]))
merge(nests_tagged_birds, breeding.data[,c('birdID','hatchday','successful')], by.x='TagID',by.y='birdID',all.x=T)
test = merge(breeding.data[,c('year','birdID','device_info_serial','hatchday','successful')], nests_tagged_birds, by.y=c('Year','TagID'), by.x=c('year','device_info_serial'), all.x=T)
#View(test) # nest data are complete in the database since 2017!
names(BS_nest_sel)[3]='BS'
BS_nest_tag = merge(BS_nest_sel, nests_tagged_birds, by=c('Year','NestID'), all.x=T)
BS_nest_tag$TagID[is.na(BS_nest_tag$TagID)]=0
BS_nest_tag$TagID[BS_nest_tag$TagID>10000] =0 
BS_nest_tag$tagged = "no"
BS_nest_tag$tagged[BS_nest_tag$TagID>0] = "yes"
BS_nest_tag$Year <- as.factor(BS_nest_tag$Year)
# the cases where birds deserted their nests and did not renest after they were caughgt (i.e. the case for 6294 in 2016 and 6310 in 2017) are removed as soon as the BS_nest_sel data are linked, as no BS data are available for these nests since chicks were never temporarily labelled. 
nests_tagged_birds[(nests_tagged_birds$TagID==6310 & nests_tagged_birds$Year==2017),]
BS_nest_tag[(BS_nest_tag$TagID==6310 & BS_nest_tag$Year==2017),]

# only analyse 2017-2019, as there are sufficient data of BS for untagged birds during these years, and these years are in the analysis:
BS_nest_tag_sel = unique(na.omit(BS_nest_tag[BS_nest_tag$Year%in%2017:2019,c('Year','NestID','tagged','BS')]))
table(BS_nest_tag_sel$tagged, BS_nest_tag_sel$Year)
BS_tagged_birds = BS_nest_tag[BS_nest_tag$Year%in%2017:2019 & BS_nest_tag$TagID>0,]
BS_tagged_birds[order(BS_tagged_birds$Year, BS_tagged_birds$NestID),] # 5 nests where two tagged parents were on (3 times 760&763 and 2 times 6288&6289)
m.BS.tag = glm(BS~tagged+Year, BS_nest_tag_sel, family="poisson", na.action='na.fail')
table(BS_nest_tag_sel$BS)
anova(m.BS.tag)
modsel.BS = dredge(m.BS.tag)
modsel.BS
TableS1 <- make.table.from.dredge.output(modsel.BS)
write.csv(TableS1, "output/TableS3 - Model selection breeding success.csv")


m.BS.tag.pars = glm(BS~Year, BS_nest_tag_sel, family="poisson", na.action='na.fail')
summary(m.BS.tag.pars)
data.pred = expand.grid(Year=as.factor(2017:2019), tagged=c("yes","no"))
data.pred$pred = predict(m.BS.tag, newdata=data.pred, se.fit=T)$fit
data.pred$se = predict(m.BS.tag, newdata=data.pred, se.fit=T)$se.fit
data.pred$li = data.pred$pred-1.96*data.pred$se
data.pred$ui = data.pred$pred+1.96*data.pred$se
# translate into response level:
data.pred[,c('pred','li','ui')] = exp(data.pred[,c('pred','li','ui')])
data.pred

write.csv(data.pred, "output/TableS2 - Breeding success estimates.csv")

# what was the average age at which birds were labelled and colour-ringed on Schiermonnikoog in the period 2017-2019?

# label age:
labelled_chicks_Schier_2017_2019 = na.omit(labelled_chicks[labelled_chicks$Location=="Schiermonnikoog, Oosterkwelder" & labelled_chicks$Year%in%2017:2019,])
labelled_chicks_Schier_2017_2019$Age = round(-log(-log(labelled_chicks_Schier_2017_2019$HeadBill/184))/0.052 + 7.9,0)
mean(labelled_chicks_Schier_2017_2019$Age)
quantile(labelled_chicks_Schier_2017_2019$Age, 0.125) # 20 days
quantile(labelled_chicks_Schier_2017_2019$Age, 0.875) # 29 days

# ringing age:
head(ringings)
ringings_juvs_Schier_2017_2019 = na.omit(ringings[ringings$Region=='Schiermonnikoog' & ringings$Ageclass=="pullus" & ringings$YearRinged%in%2017:2019 & ringings$'Recapture?'==0,c('YearRinged','8thPrimary')])
ringings_juvs_Schier_2017_2019$Age = -log(-log(ringings_juvs_Schier_2017_2019$'8thPrimary'/247))/0.095 + 19.3
mean(ringings_juvs_Schier_2017_2019$Age)
# 75% range
quantile(ringings_juvs_Schier_2017_2019$Age, 0.125) # 20 days
quantile(ringings_juvs_Schier_2017_2019$Age, 0.875) # 29 days
