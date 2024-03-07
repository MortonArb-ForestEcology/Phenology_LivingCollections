# Set file paths, load libraries etc.
library(googlesheets4)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages
library(lubridate)

# Source my cleaning function
source("clean_google_form.R")

# dir.base <- "/Volumes/"
dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)

path.dat <- file.path(dir.base, "Observing Lists/2020_Quercus")
maps.out <- file.path(path.dat)
# path.gis <- "/Volumes/GIS/Collections" # Path on a Mac
# path.gis <- "Y:/Collections" # Path on a PC

# Access & format the observations
# get the data from each collection
quercus <- clean.google(collection="Quercus", dat.yr=lubridate::year(Sys.Date()))
summary(quercus)
# quercus[quercus$Date.Observed>Sys.Date(),1:6]

acer <- clean.google(collection="Acer", dat.yr=lubridate::year(Sys.Date()))
summary(acer)

ulmus <- clean.google(collection="Ulmus", dat.yr=lubridate::year(Sys.Date()))
summary(ulmus)
# ----------------

# Put the data together
dat.all <- rbind(quercus, acer, ulmus)
summary(dat.all)
summary(dat.all[is.na(dat.all$leaf.buds.observed),])

phenophase.obs <- names(dat.all)[grep(".observed", names(dat.all))] 


summary(dat.all$Observer)
summary(dat.all[,"Observer"])

# Shoudln't need this anymore
# for(PHENO in phenophase.obs){
#   dat.all[is.na(dat.all[,PHENO]),PHENO] <- "No Observation"
#   # dat.all[,PHENO] <- factor(dat.all[,PHENO], levels=c("No", "Yes", "Unsure", "Did not look for"))
# }
# summary(dat.all)

range(dat.all$Date.Observed)
dat.all[lubridate::year(dat.all$Date.Observed)<lubridate::year(Sys.Date()),1:6]
dat.all[lubridate::year(dat.all$Date.Observed)>lubridate::year(Sys.Date()),1:6]
dat.all[dat.all$Date.Observed>Sys.Date(),1:6]

# dim(dat.all)

# For QAQC, get rid of trees that have been removed
# NOTE: This is broken and needs to be updated to GoogleSheets4!
# # Querying the googlesheet for missing trees up front to make it easier
# sheet.gone <- gs_title("Removed Trees - Phenology_LivingCollections")
# sheet.gone # Prints all the metadata
# 
# # Get the particular sheet & coerce it into a data frame rather than something special
# df.gone <- data.frame(gs_read(sheet.gone, ws="Removed Trees"))
# summary(df.gone)
# 
# dim(dat.all)
# dat.all <- dat.all[!dat.all$PlantNumber %in% df.gone$PlantNumber,]
# summary(dat.all)

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

dat.all <- merge(dat.all, obs.list[,c("Obs.List", "collection", "PlantNumber")])
dat.all$Obs.List <- as.factor(dat.all$Obs.List)
dat.all$collection <- as.factor(dat.all$collection)
summary(dat.all)

# Making some better graphs to show what's going on with the phenology in the living collections

#source("clean_google_form.R")

library(plotly)

library(car)


#Practice with Shiny: dropdown of different columns
#Put a ton of packages just to make sure everything works in case
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
dat.all <- dat.all[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,30,16,17,18,19,20,21,22,23,24,25,26,27,28,29)]
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
dat.all.stack[,c("Observer", "Date.Observed", "Species", "PlantNumber", "Obs.List", "Notes")] <- dat.all[,c("Observer", "Date.Observed", "Species", "PlantNumber", "Obs.List", "Notes")]
head(dat.all.stack)
tail(dat.all.stack)
summary(dat.all.stack)

#tried to make it so that coloring would be consistent
color.levels <- factor(dat.all.stack$Phenophase.Status ,levels = c("Yes", "No", "Unsure", "No Observation"))

ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))),
  
  selectInput("Obs.List", "Choose a Obs.List:", list(Obs.List=as.list(paste(sort(unique(dat.all.stack$Obs.List)))))), 
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
    ggplotly(ggplot(data=dat.all.stack[dat.all.stack$Obs.List==input$Obs.List & dat.all.stack$Phenophase==input$Phenophase, ]) + # data being used
               ggtitle(paste(input$Phenophase, "for", input$Obs.List, sep=" ")) + # title
               facet_grid(Obs.List*PlantNumber~., scales="free_y", switch="y") + # lines for different species +
               geom_bin2d(aes(x=Date.Observed, y=PlantNumber, fill=Phenophase.Status, 
                              text=stringr::str_wrap(paste('Date Observed:',Date.Observed,'<br>','Observer: ',Observer,'<br>', 'Phenophase Status: ',Phenophase.Status,'<br>', 'Intensity: ',Intensity,'<br>', 'Intensity Status: ',Intensity.Status,'<br>','Plant Number: ', PlantNumber,'<br>','Species: ', Species,'<br>','Notes: ', Notes)), binwidth=7)) + # green filling & actual data
               scale_fill_manual(dat.all.stack$Phenophase.Status, values = c("Yes"="green4", "No"="firebrick3", "Unsure"="gray50", "No Observation"="black"))  + # color scheme
               scale_x_date(name="Date", limits = range(quercus$Date.Observed), expand=c(0,0)) + # x-axis and other stuff?, only works when range is quercus
               scale_y_discrete(expand=c(0,0)) + # fills in graph to make it solid
               scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +  # I'm not sure
               theme(legend.position="bottom", #need to move legend position
                     legend.text = element_text(size=rel(1)),
                     legend.title=element_blank(),
                     plot.title = element_text(size=rel(1), face="bold", hjust=1), #formats title to be bold and in center
                     panel.grid = element_blank(),
                     panel.background=element_rect(fill=NA, color="black"), #divider lines in , makes background white
                     panel.spacing=unit(0.05, "lines"), #connects all the individual trees together
                     axis.text.x=element_text(size=rel(1)),
                     axis.title.x=element_text(size=rel(1), face="bold"), #makes x axis name bolded
                     axis.title.y=element_blank(), #gets rid of y-axis name: I think it should be there
                     axis.text.y=element_blank(), #makes it so that tree number is not displayed outside of gray part
                     axis.ticks.y=element_blank(), #gets rid of ticks outside gray box of y-axis
                     strip.text = element_text(angle=90), 
                     plot.margin=unit(c(0,5,0,0), "lines"),
                     strip.text.y=element_text(size=rel(1), angle=0)), tooltip="text") %>% 
      layout(legend = list(orientation = "h", x = 0.4, y = -0.2, 
                           margin = list(b = 50, l = 50))) #gets rid of ticks outside gray box of y-axis, also puts y-axis upside down which I fixed by changing angle to 0
    
  })
}

shinyApp(ui, server)
