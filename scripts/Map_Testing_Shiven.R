#This is a practice script where I can just practice learning how to use maps
#Lines 5-122 are just getting the data that I am using ready
#Deleted comments but left them on QAQC_Graph_Final script so in case any errors pop up maybe look there

# Set file paths, load libraries etc.
library(googlesheets4)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages
library(lubridate)

# Source my cleaning function
source("clean_google_form.R")
dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"

path.dat <- file.path(dir.base, "Observing Lists/2020_Quercus")
maps.out <- file.path(path.dat)

# Access & format the observations
# get the data from each collection
quercus <- clean.google(collection="Quercus", dat.yr=lubridate::year(Sys.Date()))
summary(quercus)

acer <- clean.google(collection="Acer", dat.yr=lubridate::year(Sys.Date()))
summary(acer)

ulmus <- clean.google(collection="Ulmus", dat.yr=lubridate::year(Sys.Date()))
summary(ulmus)

# Put the data together
dat.all <- rbind(quercus, acer, ulmus)
summary(dat.all)
summary(dat.all[is.na(dat.all$leaf.buds.observed),])

phenophase.obs <- names(dat.all)[grep(".observed", names(dat.all))] 

summary(dat.all$Observer)
summary(dat.all[,"Observer"])

range(dat.all$Date.Observed)
dat.all[lubridate::year(dat.all$Date.Observed)<lubridate::year(Sys.Date()),1:6]
dat.all[lubridate::year(dat.all$Date.Observed)>lubridate::year(Sys.Date()),1:6]
dat.all[dat.all$Date.Observed>Sys.Date(),1:6]

# For QAQC, get rid of trees that have been removed
# NOTE: This is broken and needs to be updated to GoogleSheets4!
# # Querying the googlesheet for missing trees up front to make it easier
# Also merge in the observing lists and volunteer assignments
quercus.list <- read.csv(file.path("../data/ObservingLists", "ObservingLists_Quercus.csv"))
acer.list <- read.csv(file.path("../data/ObservingLists", "ObservingLists_Acer.csv"))
ulmus.list <- read.csv(file.path("../data/ObservingLists", "ObservingLists_Ulmus.csv"))
quercus.list$collection <- "Quercus"
acer.list$collection <- "Acer"
ulmus.list$collection <- "Ulmus"
quercus.list$Obs.List <- paste(quercus.list$collection, quercus.list$Obs.List, sep="-")
acer.list$Obs.List <- paste(acer.list$collection, acer.list$Obs.List, sep="-")
ulmus.list$Obs.List <- paste(ulmus.list$collection, ulmus.list$Obs.List, sep="-")

summary(quercus.list)
summary(acer.list)
summary(ulmus.list)
head(acer.list)

obs.list <- rbind(quercus.list, acer.list, ulmus.list)
summary(obs.list)
head(obs.list)

dat.all <- merge(dat.all, obs.list[,c("Obs.List", "collection", "PlantNumber", "BgLatitude", "BgLongitude")])
dat.all$Obs.List <- as.factor(dat.all$Obs.List)
dat.all$collection <- as.factor(dat.all$collection)
summary(dat.all)
head(dat.all)

#Put a ton of packages just to make sure everything works in case
library(plotly)
library(car)
library(dplyr)
library(rgdal)
library(ggmap)
library(raster)
library(rgeos)
library(xts)
library(tigris)
library(shiny)
library(shinydashboard)
library(htmltools)
library(stringr)

#shiny usage from internet: https://gallery.shinyapps.io/093-plot-interaction-basic/?_ga=2.159045370.1313669618.1595826759-1262602594.1594961620
#adding leaf.falling.intensity to dat.all so I can add intensity to dat.all.stack
dat.all$leaf.falling.intensity <- NA # need to make it so that the intensity and phenophases can be equal in size
colnames(dat.all)
dat.all <- dat.all[, c(1,2,3,4,5,6,7,31,32,8,9,10,11,12,13,14,15,30,16,17,18,19,20,21,22,23,24,25,26,27,28,29)]
head(dat.all)
# quercus.spp <- paste(unique(quercus$Species))
phenophases <- names(dat.all)[grep(".observed", names(dat.all))]
intensity <- names(dat.all)[grep(".intensity", names(dat.all))]

# Converting phenophases in to character columns instead of factors
for(i in 1:length(phenophases)){
  dat.all[,phenophases[i]] <- as.character(dat.all[,phenophases[i]])
}

for(i in 1:length(intensity)){
  dat.all[, intensity[i]] <- as.character(dat.all[, intensity[i]])
}

intensity
phenophases
dat.all.stack <- stack(dat.all[,phenophases], drop=F)
dat.all.stack.2 <- stack(dat.all[,intensity], drop=F)
names(dat.all.stack) <- c("Phenophase.Status", "Phenophase")
names(dat.all.stack.2) <- c("Intensity.Status", "Intensity")
head(dat.all.stack)
head(dat.all.stack.2)
nrow(dat.all.stack)
nrow(dat.all.stack.2)
dat.all.stack <- cbind(dat.all.stack, dat.all.stack.2) #not working anymore
dat.all.stack[,c("Observer", "Date.Observed", "Species", "PlantNumber", "Obs.List",  "collection", "BgLatitude", "BgLongitude", "Notes")] <- 
  dat.all[,c("Observer", "Date.Observed", "Species", "PlantNumber", "Obs.List", "collection", "BgLatitude", "BgLongitude", "Notes")]
head(dat.all.stack)
tail(dat.all.stack)
summary(dat.all.stack)

#tried to make it so that coloring would be consistent
color.levels <- factor(dat.all.stack$Phenophase.Status ,levels = c("Yes", "No", "Unsure", "No Observation"))



#Graphs for Map

#I realized that the points for different phenophases would pop on top of each other
ggplotly(ggplot(data=dat.all.stack[dat.all.stack$Obs.List=="Acer-6", ]) + 
           geom_point(aes(x=BgLatitude, y=BgLongitude)) + coord_equal())

#Don't have the issue with the phenophases on top of each other as I pick phenophases
#need to work on slider: not working for single value or range
#used this website (can make range or single value for date but both are not working): https://shiny.rstudio.com/articles/sliders.html
#worked with obs.list and phenophase dropdowns, stopped working with slider for date, not because it is a slider but because it is a date: tried with dropdown
ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  titlePanel("Customizable Map for QAQC Data"),
  tags$head(
    tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))),
  sidebarPanel(sliderInput("Date.Observed", "Choose a Date:", min = min(dat.all.stack$Date.Observed), max = max(dat.all.stack$Date.Observed),
                value = max(dat.all.stack$Date.Observed))),
  selectInput("collection", "Choose a collection:", list(collection=as.list(paste(unique(dat.all.stack$collection))))),
  selectInput("Phenophase", "Choose a Phenophase:", list(Phenos=as.list(paste(unique(dat.all.stack$Phenophase))))), 
  verbatimTextOutput("hover_info"),			    
  mainPanel(plotlyOutput("plot1", width = 850, height = 750),
            # hover=hoverOpts(id="plot_hover")
  ))


#goal: try to get a geom_bin2d graph to work in shiny
#so far almost everything of the graph is working: Date Observed is now normal but it has to be repeated twice for it to work
#Weirds that on scale_x_date I have to have the range as quercus
#Not working with Obs.List as x

server <- function(input, output) {
  
  output$plot1 <- renderPlotly({
    # if (input$plot_type == "base") {
    # plot(mtcars$wt, mtcars$mpg)
    # } else if (input$plot_type == "ggplot2") {
    # ggplot(mtcars, aes(wt, mpg, color=carb)) + geom_point()
    # }
    ggplotly(ggplot(data=dat.all.stack[dat.all.stack$collection==input$collection & dat.all.stack$Phenophase==input$Phenophase & dat.all.stack$Date.Observed==as.Date(input$Date.Observed), ]) + # data being used
               ggtitle(paste("Map of ", input$Phenophase, "for", input$collection, sep=" ")) + # title
               #facet_grid(Obs.List*PlantNumber~., scales="free_y", switch="y") + # lines for different species +
               geom_point(aes(x=BgLongitude, y=BgLatitude, color=Phenophase.Status,
                              text=paste('Obs.List:',Obs.List,'<br>','Observer: ',Observer,'<br>', 'Phenophase Status: ',Phenophase.Status,'<br>', 'Intensity: ',Intensity,'<br>', 
                                         'Intensity Status: ',Intensity.Status,'<br>','Plant Number: ', PlantNumber,'<br>','Species: ', Species,'<br>','Notes: ', Notes)), binwidth=7) + # green filling & actual data
               scale_color_manual(dat.all.stack$Phenophase.Status, values = c("Yes"="green4", "No"="firebrick3", "Unsure"="gray50", "No Observation"="black"))  + # color scheme
               coord_equal() + #makes scaling of graph 1:1
               #scale_x_date(name="Date", limits = range(quercus$Date.Observed), expand=c(0,0)) + # x-axis and other stuff?, only works when range is quercus
               #scale_y_discrete(expand=c(0,0)) + # fills in graph to make it solid
               #scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +  # I'm not sure
               theme(legend.position="bottom", #need to move legend position
                     legend.text = element_text(size=rel(1)),
                     legend.title=element_blank(),
                     plot.title = element_text(size=rel(1), face="bold", hjust=1), #formats title to be bold and in center
                     #panel.grid = element_blank(),
                     #panel.background=element_rect(fill=NA, color="black"), #divider lines in , makes background white
                     #panel.spacing=unit(0.05, "lines"), #connects all the individual trees together
                     axis.text.x=element_text(size=rel(1)),
                     axis.title.x=element_text(size=rel(1), face="bold"), #makes x axis name bolded
                     #axis.title.y=element_blank(), #gets rid of y-axis name: I think it should be there
                     #axis.text.y=element_blank(), #makes it so that tree number is not displayed outside of gray part
                     #axis.ticks.y=element_blank(), #gets rid of ticks outside gray box of y-axis
                     #strip.text = element_text(angle=90), 
                     #plot.margin=unit(c(0,5,0,0), "lines"),
                     strip.text.y=element_text(size=rel(1), angle=0)), tooltip="text") %>% 
      layout(legend = list(orientation = "h", x = 0.4, y = -0.2, 
                           margin = list(b = 50, l = 50))) #gets rid of ticks outside gray box of y-axis, also puts y-axis upside down which I fixed by changing angle to 0
    
  })
}


shinyApp(ui, server)

dat.all.stack$O

#not a plotly issue
ggplotly(ggplot(data=dat.all.stack[dat.all.stack$Date.Observed==max(dat.all.stack$Date.Observed) & dat.all.stack$Phenophase==dat.all.stack$Phenophase[1], ]) + # data being used
           ggtitle(paste("Map of ", dat.all.stack$Phenophase[1], "for", unique(dat.all.stack$Date.Observed)[1], sep=" ")) + # title
           geom_point(aes(x=BgLongitude, y=BgLatitude, fill=Phenophase.Status, size=10,
                          text=paste('Obs.List:',Obs.List,'<br>','Observer: ',Observer,'<br>', 'Phenophase Status: ',Phenophase.Status,'<br>', 'Intensity: ',Intensity,'<br>', 
                                     'Intensity Status: ',Intensity.Status,'<br>','Plant Number: ', PlantNumber,'<br>','Species: ', Species,'<br>','Notes: ', Notes)), binwidth=7) + # green filling & actual data
           scale_fill_manual(dat.all.stack$Phenophase.Status, values = c("Yes"="green4", "No"="firebrick3", "Unsure"="gray50", "No Observation"="black"))  + # color scheme
           coord_equal() + #makes scaling of graph 1:1
           theme(legend.position="bottom", #need to move legend position
                 legend.text = element_text(size=rel(1)),
                 legend.title=element_blank(),
                 plot.title = element_text(size=rel(1), face="bold", hjust=1), #formats title to be bold and in center
                 axis.text.x=element_text(size=rel(1)),
                 axis.title.x=element_text(size=rel(1), face="bold"), #makes x axis name bolded
                 strip.text.y=element_text(size=rel(1), angle=0)), tooltip="text") %>% 
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2, 
                       margin = list(b = 50, l = 50))) #gets rid of ticks outside gray box of y-axis, also puts y-axis upside down which I fixed by changing angle to 0

