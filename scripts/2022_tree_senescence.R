# A new script with to attempt to get 50% senescence metric for leaf color in 2022
# -----------------------------------
library(ggplot2)
#------------------------------------
source("clean_google_form.R")
# Reading phenooogy data for 2022
acer22 <- clean.google(collection="Acer", dat.yr=2022)
acer22$Collection <- as.factor("Acer")
acer22$Year <- lubridate::year(acer22$Date.Observed)
summary(acer22)

quercus22 <- clean.google(collection="Quercus", dat.yr=2022)
quercus22$Collection <- as.factor("Quercus")
quercus22$Year <- lubridate::year(quercus22$Date.Observed)
summary(quercus22)

ulmus22 <- clean.google(collection="Ulmus", dat.yr=2022)
ulmus22$Collection <- as.factor("Ulmus")
ulmus22$Year <- lubridate::year(ulmus22$Date.Observed)
summary(ulmus22)

tilia22 <- clean.google(collection="Tilia", dat.yr=2022)
tilia22$Collection <- as.factor("Tilia")
tilia22$Year <- lubridate::year(tilia22$Date.Observed)
summary(tilia22)

###binding into a df 
dat.22 <- rbind(quercus22,acer22, ulmus22, tilia22)
#yday, but it can be changed if needed 
dat.22$yday <- lubridate::yday(dat.22$Date.Observed)
summary(dat.22)


#only looking at trees that showed fall color from 8/1 on
dat.22 <- dat.22 [dat.22$yday>=150,]

#Seperating out the columns I want. I might be able to skip this step, however,I wanted to make sure I was only grabbing trees that recorded "Yes" for leaves.
dat.se <- dat.22[dat.22$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "leaf.color.intensity", "Collection", "leaf.present.intensity")]
summary(dat.se)

##Creating a df containing only leaves that expressed the critera of senescence 
dat.22o <-dat.22[dat.22$leaf.present.intensity %in% c( "<5","5-24%", "25-49%")| dat.22$leaf.color.intensity %in% c(">95", "75-94%", "50-74%"),]
head(dat.22o)

#aggregating out NA'S and superfluous columns 
leaf.sen <- aggregate(yday ~ PlantNumber + Species + Collection, data=dat.22o, FUN=min, na.rm=T)
summary(leaf.sen)
head(leaf.sen)

#writing .csv commented out for now
write.csv(leaf.sen,"~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations//Tree_senescence__TMA_2022.CSV")

#Graphing
ggplot(data=leaf.sen) +
  facet_grid(Collection~.,scales="free_y" ) + 
  geom_point(alpha=0.5, aes(x=yday, y=PlantNumber, fill=as.factor(yday), color=as.factor(yday))) +
 #theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
       # axis.ticks.y=element_blank())+
  theme(legend.position = "none")+
  labs(title="testing", x="Day of Year")
dev.off()
