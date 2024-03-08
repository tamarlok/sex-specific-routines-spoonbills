ch = read.csv("data/ch_data_tagged_untagged_adults.csv", colClasses = c('character','character','factor'))

# survival analysis investigating tag effect on survival (explorative analysis showed that survival was best modelled as constant rather than with annual variation or a linear time trend)
proc = process.data(ch, begin.time= 2012, model = "CJS", groups="tag")
release.gof(proc) # TEST2 and TEST3 were not significant.
ddl = make.design.data(proc)
Phi.c = list(formula=~1)
Phi.tag = list(formula=~tag)
p.t = list(formula=~time)
cml = create.model.list("CJS")
models.run = mark.wrapper(cml, data=proc, ddl=ddl, begin.time=2012, adjust=T)
write.csv(models.run$model.table, "output/TableS6 - Model selection survival.csv")
Phi.modavg = model.average(models.run, "Phi", vcv=T)$estimate
unique(Phi.modavg[,c('estimate','lcl','ucl','tag')])
p.modavg = model.average(models.run, "p", vcv=T)$estimate
p.modavg = unique(p.modavg[,c('time','estimate','lcl','ucl')])
write.csv(p.modavg, "output/TableS7 - p estimates.csv")

# Load BS data
BS_nest_tag = read.csv("data/BS_data_tagged_untagged.csv", header=T)
BS_nest_tag$year = as.factor(BS_nest_tag$year)

# Analyse data on nest level (which means that nests can have either one or two tagged parents)
BS_nest_tag_sel = aggregate(tagged~year+nestID+BS, BS_nest_tag, max)
BS_tagged_birds = BS_nest_tag[BS_nest_tag$year%in%2017:2019 & BS_nest_tag$tagID>0,]
BS_tagged_birds[order(BS_tagged_birds$year, BS_tagged_birds$nestID),] # 6 nests where two tagged parents were on (1 time 6284&6288, 3 times 760&763 and 2 times 6288&6289)
m.BS.tag = glm(BS~tagged+year, BS_nest_tag_sel, family="poisson", na.action='na.fail')
table(BS_nest_tag_sel$BS)
anova(m.BS.tag)
modsel.BS = dredge(m.BS.tag)
modsel.BS
TableS4 <- make.table.from.dredge.output(modsel.BS)
write.csv(TableS4, "output/TableS4 - Model selection breeding success.csv")

m.BS.tag.pars = glm(BS~year, BS_nest_tag_sel, family="poisson", na.action='na.fail')
summary(m.BS.tag.pars)
data.pred = expand.grid(year=as.factor(2017:2019), tagged=c("yes","no"))
data.pred$pred = predict(m.BS.tag, newdata=data.pred, se.fit=T)$fit
data.pred$se = predict(m.BS.tag, newdata=data.pred, se.fit=T)$se.fit
data.pred$li = data.pred$pred-1.96*data.pred$se
data.pred$ui = data.pred$pred+1.96*data.pred$se
# translate into response level:
data.pred[,c('pred','li','ui')] = exp(data.pred[,c('pred','li','ui')])
data.pred

write.csv(data.pred, "output/TableS5 - Breeding success estimates.csv")