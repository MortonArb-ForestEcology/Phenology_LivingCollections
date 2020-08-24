# Making some better graphs to show what's going on with the phenology in the living collections

library(googlesheets4)

source("clean_google_form.R")

library(ggplot2); library(grid) # graphing packages

library(plotly)

library(car)

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

# theme.formatting <- 

#extracting the month to put into graph
library(lubridate)

summary(quercus)
quercus$Date.Observed
quercus$Time

quercus$Month <- month(as.POSIXlt(quercus$Date.Observed, format="%Y/%m/%d"))
quercus$Day <- day(as.POSIXlt(quercus$Date.Observed, format="%Y/%m/%d"))
quercus$Year <- year(as.POSIXlt(quercus$Date.Observed, format="%Y/%m/%d"))
quercus$Time <- format(quercus$Timestamp, '%H:%M:%S')
is.numeric(quercus$Date.Observed)

summary(quercus)



#making graph interactive
leafObserve <- ggplot(data=quercus[quercus$Species == "Quercus alba", ]) + # data being used
  ggtitle("Leaf Present") + # title
  facet_grid(Species*PlantNumber~., scales="free_y", switch="y") + # lines for different species +
  geom_bin2d(aes(x=Date.Observed, y=PlantNumber, fill=leaf.present.observed, 
                 text = sprintf("Timestamp: %s<br>Observer: %s", paste(Timestamp), Observer) # its not working because text is not working
  ), binwidth=7) + # green filling & actual data
  scale_fill_manual(values=c("gray50", "green4", "blue2", "black") ) + # color scheme
  scale_x_date(name="Date", limits = range(quercus$Date.Observed), expand=c(0,0)) + # x-axis and other stuff?
  scale_y_discrete(expand=c(0,0)) + # fills in graph to make it solid
  scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) + # I'm not sure
  theme(legend.position='top', #need to move legend position 
        legend.text = element_text(size=rel(1)),
        legend.title = element_text(size=rel(1)),
        plot.title = element_text(size=rel(1), face="bold", hjust=0.5), #formats title to be bold and in center
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"), #divider lines in , makes background white
        panel.spacing=unit(0, "lines"), #connects all the individual trees together
        axis.text.x=element_text(size=rel(1)),
        axis.title.x=element_text(size=rel(1), face="bold"), #makes x axis name bolded
        axis.title.y=element_blank(), #gets rid of y-axis name: I think it should be there
        axis.text.y=element_blank(), #makes it so that tree number is not displayed outside of gray part
        axis.ticks.y=element_blank(), #gets rid of ticks outside gray box of y-axis
        strip.text.y=element_text(size=rel(1), angle=0)) #gets rid of ticks outside gray box of y-axis, also puts y-axis upside down which I fixed by changing angle to 0

leafObserve
QuercusAlba.Interactive <- ggplotly(leafObserve + theme(strip.text = element_text(angle=90), 
                                                        plot.margin=unit(c(0,5,0,0), "lines"), 
                                                        legend.title=element_blank())) %>% 
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2, 
                       margin = list(b = 50, l = 50)))  #only works sometimes: but then y-axis disappears with layout part of legend to the bottom

QuercusAlba.Interactive



# #testing with plotly vs ggplot: looks like plotly did not display nearly as many values when done with the entire quercus dataset
# plotlyquercus.test <- plot_ly(
#   data = quercus,
#   type = 'scatter', x = ~Date.Observed, y = ~PlantNumber, mode = 'markers')
# plotlyquercus.test
# 
# ggplotquercus.test <- ggplot(data=quercus) + 
#   geom_point(aes(x=Date.Observed, y=PlantNumber))
# ggplotquercus.test
# ggplotly(ggplotquercus.test)



# # Just testing saving it to the desktop.  We need to figure out how to work with the formatting, but it works!
# htmlwidgets::saveWidget(as_widget(QuercusScatter), "~/Desktop/QuercusScatter.html")



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


#shiny usage from internet: https://gallery.shinyapps.io/093-plot-interaction-basic/?_ga=2.159045370.1313669618.1595826759-1262602594.1594961620
#adding leaf.falling.intensity to dat.all so I can add intensity to dat.all.stack
dat.all$leaf.falling.intensity <- rep(NA, 4680) # need to make it so that the intensity and phenophases can be equal in size
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
dat.all.stack <- cbind(dat.all.stack, dat.all.stack.2)
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
  mainPanel(plotlyOutput("plot1", height = 750),
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
                     text=paste('Date Observed:',Date.Observed,'<br>','Observer: ',Observer,'<br>', 'Phenophase Status: ',Phenophase.Status,'<br>', 'Intensity: ',Intensity,'<br>', 'Intensity Status: ',Intensity.Status,'<br>','Plant Number: ', PlantNumber,'<br>','Species: ', Species,'<br>','Notes: ', Notes)), binwidth=7) + # green filling & actual data
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

#Links of websites that talk about converting shiny to HTML
#Looks like I'm going to have to create a Shiny Server in order to convert it into HTML
#https://stackoverflow.com/questions/46575316/output-pure-html-file-from-r-shiny-app
#https://shiny.rstudio.com/articles/rmarkdown.html
#http://rstudio.github.io/shiny/tutorial/#deployment-web
#https://shiny.rstudio.com/articles/generating-reports.html