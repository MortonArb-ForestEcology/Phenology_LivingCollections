# A new script with some combined collections analyses/graphs for the end of the year report
library(ggplot2)
library(lubridate)
library(tidyverse)
library(gganimate)
library(dplyr)
###setting the file path to mac or windows##
path.google <- "/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive" # Mac
path.out <- file.path(path.google, "/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive/LivingCollections_Phenology/Reports")
#path.figs <- file.path(path.google, "LivingCollections_Phenology/Reports/2022_02_EndOfYear_Report/figures_2022_end")
# this is for google -># 
path.figs <- "/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive/LivingCollections_Phenology/Reports/2022_02_EndOfYear_Report/figures_2022_end"
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

# -----------------------------------
# 1. Arb Data
# -----------------------------------
source("clean_google_form.R")
#year 2022
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

#########generating a 2022 only df 
dat.22 <- rbind(quercus22,acer22, ulmus22, tilia22)
dat.22$yday <- lubridate::yday(dat.22$Date.Observed)
summary(dat.22)


##########################################
dat.lb22 <- dat.22[dat.22$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed","leaf.breaking.buds.observed","leaf.present.observed", "Collection")] 
dat.lb22 <- dat.lb22[!is.na(dat.lb22$PlantNumber),]
summary(dat.lb22)
   
dat.lp22 <- dat.22[dat.22$leaf.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed","leaf.breaking.buds.observed","leaf.present.observed", "Collection")] 
dat.lp22 <- dat.lp22[!is.na(dat.lp22$PlantNumber),]
summary(dat.lp22)

dat.lc22 <- dat.22[dat.22$leaf.color.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed", "leaf.breaking.buds.observed","leaf.present.observed", "Collection")]  
dat.lc22 <- dat.lc22[!is.na(dat.lc22$PlantNumber),]
summary(dat.lc22)

dat.22lam <- rbind(dat.lc22,dat.lp22,dat.lb22)
summary(dat.22lam)
dat.22lam$yday <- lubridate::yday(dat.22lam$Date.Observed)
summary(dat.22lam)

dat.22y <- rbind(dat.lc22,dat.lp22,dat.lb22)
summary(dat.22y)
dat.22y$yday <- lubridate::yday(dat.22y$Date.Observed)
summary(dat.22y)


#x <- function(dat.22lam) {
 # dat.22lam$leaf.present.observed= "Yes" <- ifelse(dat.22lam$leaf.color.observed!="Yes", dat.22lam$leaf.color.observed,NA)
 # dat.22lam$leaf.breaking.buds.observed = "Yes" <- ifelse(dat.22lam$leaf.present.observed!=Yes| dat.22lam$leaf.color.observed!="Yes", dat.22$leaf.breaking.buds.observed,NA)
 # return (dat.22lam)
#}


#x <- function(dat.22lam) {
 # dat.22lam$leaf.color.observed ="Yes" <- ifelse(dat.22lam$leaf.color.observed!= "No", dat.22lam$leaf.color.observed,NA)
 # dat.22lam$leaf.present.observed = "Yes" <- ifelse(dat.22lam$leaf.color.observed!="Yes" & dat.22lam$leaf.present.observed!"No", dat.22lam$leaf.present.observed,NA)
 # dat.22lam$leaf.breaking.buds.observed = "Yes" <- ifelse(dat.22lam$leaf.present.observed!="Yes" & dat.22lam$leaf.color.observed!="Yes", dat.22$leaf.breaking.buds.observed,NA)
 # return (dat.22lam)
#}


#x <- function(dat.22lam) {
 # dat.22lam$leaf.color.observed <- if(dat.22lam$leaf.color.observed!= "No")
#  dat.22lam$leaf.present.observed <- ifelse(dat.22lam$leaf.color.observed!="Yes" & dat.22lam$leaf.present.observed!="No", dat.22lam$leaf.present.observed,NA)
#  dat.22lam$leaf.breaking.buds.observed <- ifelse(dat.22lam$leaf.present.observed!="Yes" | dat.22lam$leaf.color.observed!="Yes", dat.22$leaf.breaking.buds.observed,NA)
#  return (dat.22lam)
#}
#dat.22x <- x(dat.22lam=dat.22lam)
#summary(dat.22x)

#dat.22y$leaf.breaking.buds.observed <-ifelse(dat.22y$leaf.breaking.buds.observed=="Yes", "Yes",NA)
#dat.22y$leaf.present.observed <-ifelse(dat.22y$leaf.present.observed=="Yes", "Yes",NA)
#dat.22y$leaf.color.observed <-ifelse(dat.22y$leaf.color.observed=="Yes", "Yes",NA)


#dat.22y$pheno <- if( dat.22y$leaf.present.observed=="Yes" & dat.22y$leaf.breaking.buds.observed=="Yes" & dat.22y$leaf.color.observed=="Yes"  "Colored Leaves")
 # dat.22y$pheno <- ifelse( dat.22y$leaf.present.observed!="Yes" & dat.22y$leaf.breaking.buds.observed!="Yes" & dat.22y$leaf.color.observed=="Yes", "Colored Leaves", "Leaves")
#dat.22y$pheno <- ifelse( dat.22y$leaf.present.observed!="Yes" & dat.22y$leaf.breaking.buds.observed=="Yes" & dat.22y$leaf.color.observed!="Yes", "Breaking Leaf Buds", "Leaves")
#dat.22y$pheno <- ifelse( dat.22y$leaf.present.observed=="Yes"  & dat.22y$leaf.color.observed!="Yes", "Leave", "Colored Leaves")


#dat.22y$pheno <- ifelse( dat.22y$leaf.present.observed=="Yes" & dat.22y$leaf.breaking.buds.observed=="Yes" & dat.22y$leaf.color.observed!="Yes", "Leaves","Breaking Leaf ")
#dat.22y$pheno <- ifelse( dat.22y$leaf.present.observed=="Yes" & dat.22y$leaf.breaking.buds.observed=="Yes" & dat.22y$leaf.color.observed!="Yes", "Leaves","Breaking Leaf ")



#dat.22lam$pheno<-  ifelse(dat.22$leaf.present.observed!="No" & dat.22lam$leaf.color.observed!="Yes", dat.22lam)



#Finally realized I could make another ifelse statment the "else" in and ifelse statment- astounding levels of stupidity here. 
dat.22y$pheno = with(dat.22y, ifelse(dat.22y$leaf.color.observed=="Yes", "Leaf Color Observed",
                                     ifelse(dat.22y$leaf.present.observed=="Yes" & dat.22y$leaf.breaking.buds.observed=="Yes", "Leaves Present Observed",
                                            ifelse(dat.22y$leaf.breaking.buds.observed=="Yes", "Leaf Breaking Buds Observed", "Leaves Present Observed"))))




####### many versions of the same graph

# Large pole dark
p<-ggplot(dat.22y) + 
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(-100,325) +
  theme_dark()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 


animate(p, fps=8)

# small pole dark

p<-ggplot(dat.22y) + 
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(-50,325) +
  theme_dark()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 


animate(p, fps=8)

# no pole dark
p<-ggplot(dat.22y) + 
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(0,325) +
  theme_dark()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 


animate(p, fps=8)


## Light
#Large pole light
p<-ggplot(dat.22y) + 
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(-100,325) +
  theme_bw()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 


animate(p, fps=8)

#small pole light

p<-ggplot(dat.22y) + 
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(-50,325) +
  theme_bw()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 


animate(p, fps=8)
# no pole light
p<-ggplot(dat.22y) + 
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(0,325) +
  theme_bw()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 


animate(p, fps=8)



###
#Small pole light- facet- collections
p<-ggplot(dat.22y) + 
  facet_wrap_interactive(Collection~ .)+
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(-50,200) +
  theme_bw()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 

animate(p, fps=8)

#dfm <- melt(dat.22lam[,c("yday", "leaf.color.observed", "leaf.breaking.buds.observed","leaf.present.observed")],id.vars = 1)

#ggplot(dfm,aes(x = yday,y = value)) + 
 # geom_bar(aes(fill = variable),stat = "identity",position = "dodge") 
#dev.off()

#ggplot(dfm) + 
 # geom_bar(alpha=1.5,aes(x=yday, fill=as.factor(value),))+ ylim(-100,120) +
  #theme_dark()+
  #labs(title="Leaf color", x="Day of Year",)+
  #coord_polar(start = 200)+
  #transition_states(yday, transition_length = 30, state_length =30)+
  #ease_aes(x = 'sine-out', y = 'sine-out') + 
  #shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
   #           falloff = 'sine-in', exclude_phase = 'enter') 


dat.lb22i <- dat.22[dat.22$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed", "leaf.breaking.buds.observed","leaf.present.observed", "Collection", "leaf.breaking.buds.intensity", "leaf.present.intensity", "leaf.color.intensity" )] 
dat.lb22i <- dat.lb22i[!is.na(dat.lb22i$PlantNumber),]
summary(dat.lb22i)

dat.lp22i <- dat.22[dat.22$leaf.present.observed=="Yes",c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed", "leaf.breaking.buds.observed","leaf.present.observed", "Collection", "leaf.breaking.buds.intensity", "leaf.present.intensity", "leaf.color.intensity" )]
dat.lp22i <- dat.lp22i[!is.na(dat.lp22i$PlantNumber),]
summary(dat.lp22i)

dat.lc22i <- dat.22[dat.22$leaf.color.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed", "leaf.breaking.buds.observed","leaf.present.observed", "Collection", "leaf.breaking.buds.intensity", "leaf.present.intensity", "leaf.color.intensity" )]  
dat.lc22i <- dat.lc22i[!is.na(dat.lc22i$PlantNumber),]
summary(dat.lc22i)


dat.22z <- rbind(dat.lc22i,dat.lp22i,dat.lb22i)
summary(dat.22z)
dat.22z$yday <- lubridate::yday(dat.22z$Date.Observed)
summary(dat.22z)

#doesn't work exactly
dat.22z$pheno = with(dat.22z, ifelse(dat.22z$leaf.color.intensity %in% c("50-74%", "75-94%",">95%")| dat.22z$leaf.present.intensity %in% c("0%", "<5%", "5-24%","25-49%") , "Leaf Color Observed",
                                     ifelse(dat.22z$leaf.present.intensity %in% c("50-74%", "75-94%", ">95%") & dat.22z$leaf.breaking.buds.observed=="Yes", "Leaves Present Observed",
                                            ifelse(dat.22z$leaf.breaking.buds.observed=="Yes", "Leaf Breaking Buds Observed", "Leaves Present Observed"))))


#sort of works
 dat.22z$pheno = with(dat.22z, ifelse(leaf.breaking.buds.observed=="Yes" & leaf.present.observed!="Yes", "Leaf Breaking Buds Observed",
                                     ifelse(leaf.breaking.buds.observed=="Yes" & leaf.present.observed=="Yes", "Leaves Present Observed",
                                            ifelse(leaf.color.intensity %in% c("50-74%", "75-94%",">95%")| leaf.present.intensity %in% c("0%", "<5%", "5-24%","25-49%") , "Leaf Color Observed", "Leaves Present Observed"))))


dat.22z$pheno = with(dat.22z, ifelse(dat.22z$leaf.color.intensity %in% c("50-74%", "75-94%",">95%")| dat.22z$leaf.present.intensity %in% c("0%", "<5%", "5-24%","25-49%") , "Leaf Color",
                                    ifelse(dat.22z$leaf.present.observed== "Yes" & dat.22z$leaf.breaking.buds.observed=="Yes", "Leaves Present ",
                                           ifelse(dat.22z$leaf.breaking.buds.observed=="Yes" & dat.22z$leaf.present.observed!= "Yes","Leaf Breaking Buds ", 
                                                  ifelse(dat.22z$leaf.breaking.buds.observed=="Yes" | dat.22z$leaf.color.observed=="Yes","Leaf Breaking Buds", "Leaves Present")))))

p<-ggplot(dat.22z) + 
  geom_bar(alpha=0.5,aes(x=yday, fill=pheno,))+ ylim(-50,325) +
  theme_bw()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
 coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 

animate(p, fps=7.5)

