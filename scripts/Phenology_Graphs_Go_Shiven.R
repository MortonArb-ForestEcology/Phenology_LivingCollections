# Making some better graphs to show what's going on with the phenology in the living collections

library(googlesheets4)

source("clean_google_form.R")

library(ggplot2); library(grid) # graphing packages

library(plotly)

# ----------------
# get the data from each collection
# ----------------
# Source my cleaning function
source("clean_google_form.R")

quercus <- clean.google(collection="Quercus", dat.yr=lubridate::year(Sys.Date()))
summary(quercus)
tail(quercus)
# quercus[quercus$Date.Observed>Sys.Date(),1:6]

# Checking the date distributions to see if they make sense
ggplot(data=quercus) +
  geom_histogram(aes(x=Date.Observed, fill=Observer), binwidth=7)

# acer <- clean.google(collection="Acer", dat.yr=lubridate::year(Sys.Date()))
# summary(acer)
# 
# ulmus <- clean.google(collection="Ulmus", dat.yr=lubridate::year(Sys.Date()))
# summary(ulmus)
# ----------------

theme.formatting <- 

summary(quercus)

# #An example of one of our QAQC figures for one species and one phenophase
# #graph was not working since there was no such thing as dat.all(line 43)
# ggplot(data=quercus[quercus$Species=="Quercus alba",]) +
#   ggtitle("leaf.present.observed") +
#   facet_grid(Species*PlantNumber~., scales="free_y", switch="y") +
#   geom_bin2d(aes(x=Date.Observed, y=PlantNumber, fill=leaf.present.observed), binwidth=7) +
#   scale_fill_manual(values=c("gray50", "green4", "blue2", "black") ) +
#   scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
#   scale_y_discrete(expand=c(0,0)) +
#   scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
#   theme(legend.position="bottom",
#         legend.text = element_text(size=rel(1)),
#         legend.title = element_text(size=rel(1)),
#         plot.title = element_text(size=rel(1), face="bold", hjust=0.5),
#         panel.grid = element_blank(),
#         panel.background=element_rect(fill=NA, color="black"),
#         panel.spacing=unit(0, "lines"),
#         axis.text.x=element_text(size=rel(1)),
#         axis.title.x=element_text(size=rel(1), face="bold"),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         strip.text.y=element_text(size=rel(1), angle=180))

#extracting the month to put into graph
library(lubridate)
summary(quercus)
quercus$Date.Observed
quercus$Time

quercus$Month <- month(as.POSIXlt(quercus$Date.Observed, format="%Y/%m/%d"))
quercus$Day <- day(as.POSIXlt(quercus$Date.Observed, format="%Y/%m/%d"))
quercus$Year <- year(as.POSIXlt(quercus$Date.Observed, format="%Y/%m/%d"))
quercus$Time <- format(quercus$Timestamp, '%H:%M:%S')


#making graph interactive
leafObserve <- ggplot(data=quercus[quercus$Species=="Quercus alba",]) + # data being used
                  ggtitle("leaf.present.observed") + # title
                  facet_grid(Species*PlantNumber~., scales="free_y", switch="y") + # lines for different species
                  geom_bin2d(aes(x=Date.Observed, y=PlantNumber, fill=leaf.present.observed), binwidth=7) + # green filling & actual data
                  scale_fill_manual(values=c("gray50", "green4", "blue2", "black") ) + # color scheme
                  scale_x_date(name="Date", limits = range(quercus$Date.Observed), expand=c(0,0)) + # x-axis and other stuff?
                  scale_y_discrete(expand=c(0,0)) + # fills in graph to make it solid
                  scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) + # I'm not sure
                  theme(legend.position="bottom",
                     legend.text = element_text(size=rel(1)),
                     legend.title = element_text(size=rel(1)),
                     plot.title = element_text(size=rel(1), face="bold", hjust=0.5),
                     panel.grid = element_blank(),
                     panel.background=element_rect(fill=NA, color="black"),
                     panel.spacing=unit(0, "lines"),
                     axis.text.x=element_text(size=rel(1)),
                     axis.title.x=element_text(size=rel(1), face="bold"),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     strip.text.y=element_text(size=rel(1), angle=180))
leafObserve
ggplotly(leafObserve)

quercus[quercus$Species=="Quercus alba", ]
