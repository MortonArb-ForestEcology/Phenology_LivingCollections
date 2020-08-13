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

# quercus.spp <- paste(unique(quercus$Species))
phenophases <- names(quercus)[grep(".observed", names(quercus))]
intensity <- names(quercus)[grep(".intensity", names(quercus))]

# Converting phenophases in to character columns instead of factors
for(i in 1:length(phenophases)){
  quercus[,phenophases[i]] <- as.character(quercus[,phenophases[i]])
}
for(i in 1:length(intensity)){
  quercus[, intensity[i]] <- as.character(quercus[, intensity[i]])
}



quercus.stack <- stack(quercus[,phenophases], drop=F)
names(quercus.stack) <- c("status", "phenophase")
quercus.stack[,c("Observer", "Date.Observed", "Species", "PlantNumber")] <- quercus[,c("Observer", "Date.Observed", "Species", "PlantNumber")]
summary(quercus.stack) 
head(quercus.stack)

#adding day, month, and year individually to quercus.stack because Date.Observed is not working in shiny graph: works now
quercus.stack$Month <- month(as.POSIXlt(quercus$Date.Observed, format="%Y/%m/%d"))
quercus.stack$Day <- day(as.POSIXlt(quercus$Date.Observed, format="%Y/%m/%d"))
quercus.stack$Year <- year(as.POSIXlt(quercus$Date.Observed, format="%Y/%m/%d"))
quercus.stack$Time <- format(quercus$Timestamp, '%H:%M:%S')
summary(quercus.stack)
head(quercus.stack)



ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))),
  
  selectInput("Species", "Choose a Species:", list(Quercus=as.list(paste(unique(quercus.stack$Species))))), 
  selectInput("Phenophase", "Choose a Phenophase:", list(Phenos=as.list(paste(unique(quercus.stack$phenophase))))), 
  verbatimTextOutput("hover_info"),			    
  mainPanel(plotlyOutput("plot1"),
            # hover=hoverOpts(id="plot_hover")
  ))


#goal: try to get a geom_bin2d graph to work in shiny
#so far almost everything of the graph is working: Date Observed is now normal but it has to be repeated twice for it to work
server <- function(input, output) {
  
  output$plot1 <- renderPlotly({
    # if (input$plot_type == "base") {
    # plot(mtcars$wt, mtcars$mpg)
    # } else if (input$plot_type == "ggplot2") {
    # ggplot(mtcars, aes(wt, mpg, color=carb)) + geom_point()
    # }
    print(ggplotly(ggplot(data=quercus.stack[quercus.stack$Species==input$Species & quercus.stack$phenophase==input$Phenophase, ]) + # data being used
      ggtitle(paste(input$Phenophase, "for", input$Species, sep=" ")) + # title
      facet_grid(Species*PlantNumber~., scales="free_y", switch="y") + # lines for different species +
      geom_bin2d(aes(x=Date.Observed, y=PlantNumber, fill=status, 
                     #label=Observer, label2=paste(Year, Month, Day, sep="-"), 
                     text=paste('Date Observed:',Date.Observed,'<br>','Observer: ',Observer,'<br>', 'Status: ',status,'<br>','Plant Number: ', PlantNumber)), binwidth=7) + # green filling & actual data
      scale_fill_manual(values=c("green4", "gray50", "blue2", "black") )  + # color scheme
      scale_x_date(name="Date", limits = range(quercus$Date.Observed), expand=c(0,0)) + # x-axis and other stuff?
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
                           margin = list(b = 50, l = 50)))) #gets rid of ticks outside gray box of y-axis, also puts y-axis upside down which I fixed by changing angle to 0
    
  })
}


shinyApp(ui, server)

#Links of websites that talk about converting shiny to HTML
#Looks like I'm going to have to create a Shiny Server in order to convert it into HTML
#https://stackoverflow.com/questions/46575316/output-pure-html-file-from-r-shiny-app
#https://shiny.rstudio.com/articles/rmarkdown.html
#http://rstudio.github.io/shiny/tutorial/#deployment-web
#https://shiny.rstudio.com/articles/generating-reports.html
