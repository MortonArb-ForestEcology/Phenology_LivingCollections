# Making some better graphs to show what's going on with the phenology in the living collections

library(googlesheets4)

source("clean_google_form.R")

library(ggplot2); library(grid) # graphing packages

library(plotly)

library(car)
library(shiny)

# ----------------
# get the data from each collection
# ----------------
dat.pheno <- read.csv("pheno_qaqc_shiny/pheno_compiled.csv")
dat.pheno$Date.Observed <- as.Date(dat.pheno$Date.Observed)
# dat.pheno$Timestamp <- strptime(dat.pheno$Timestamp, format="")

# dat.pheno <- dat.pheno[dat.pheno$Date.Observed>=as.Date("2020-08-01") & dat.pheno$collection=="Quercus",]
summary(dat.pheno)
# https://stackoverflow.com/questions/27965931/tooltip-when-you-mouseover-a-ggplot-on-shiny
coll.list <- paste(unique(dat.pheno$collection)[order(unique(dat.pheno$collection))])
pheno.list <- paste(unique(dat.pheno$phenophase))


ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  sliderInput("DateRange", "Start Date", min=min(dat.pheno$Date.Observed), max=max(dat.pheno$Date.Observed), value=c(min(dat.pheno$Date.Observed), max(dat.pheno$Date.Observed))),
  # selectInput("Collection", "Choose a Collection:", choices=c(list(Collection=as.list(paste(unique(dat.pheno$collection))))), 
  selectInput("Collection", "Choose a Collection:", list(Collection=as.list(coll.list))), 
  selectInput("Phenophase", "Choose a Phenophase:", list(Phenophase=as.list(pheno.list))), 
  mainPanel(plotOutput("plot1", click="plot_click"), height="100%"),
  verbatimTextOutput("info"))			    


#goal: try to get a geom_bin2d graph to work in shiny
#so far almost everything of the graph is working: Date Observed is now normal but it has to be repeated twice for it to work
server <- function(input, output) {
  output$plot1 <- renderPlot({
    dat.subs <- dat.pheno$Date.Observed>=min(input$DateRange) & dat.pheno$Date.Observed<=max(input$DateRange) & dat.pheno$collection==input$Collection & dat.pheno$phenophase==input$Phenophase & !is.na(dat.pheno$status)
    
    # cols.use <- colors.all$color.code[colors.all$pheno.status %in% dat.pheno$status[dat.subs]]
    
    ggplot(data=dat.pheno[dat.subs, ]) + # data being used
                 ggtitle(paste(input$Phenophase, "for", input$Collection, sep=" ")) + # title
                 facet_grid(Species~., scales="free", space="free", switch="y") + # lines for different species +
                 geom_point(aes(x=Date.Observed, y=PlantNumber, color=status), size=5, alpha=0.8) + # green filling & actual data
                 scale_color_manual(values= c("No" = "gray50", "Yes"="green4", "Unsure"="blue3", "No Observation"="black"))  + # color scheme
                 scale_x_date(name="Date") + # x-axis and other stuff?
                 # scale_y_discrete(expand=c(0,0)) + # fills in graph to make it solid
                 scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +  # I'm not sure
                 theme(legend.position="top", #need to move legend position
                       legend.text = element_text(size=rel(1)),
                       legend.title=element_blank(),
                       legend.key = element_rect(fill=NA),
                       plot.title = element_text(size=rel(1), face="bold", hjust=1), #formats title to be bold and in center
                       panel.grid = element_blank(),
                       panel.background=element_rect(fill=NA, color="black"), #divider lines in , makes background white
                       panel.spacing=unit(0.05, "lines"), #connects all the individual trees together
                       axis.text.x=element_text(size=rel(1)),
                       axis.title.x=element_text(size=rel(1), face="bold"), #makes x axis name bolded
                       axis.title.y=element_blank(), #gets rid of y-axis name: I think it should be there
                       axis.text.y=element_blank(), #makes it so that tree number is not displayed outside of gray part
                       axis.ticks.y=element_blank(), #gets rid of ticks outside gray box of y-axis
                       plot.margin=unit(c(0,5,0,0), "lines"),
                       strip.text.y.left=element_text(margin=unit(c(1,0,1,0), "lines"), angle=0)) #gets rid of ticks outside gray box of y-axis, also puts y-axis upside down which I fixed by changing angle to 0
    
  }, width=600, height=2000)
  
  output$info <- renderPrint({
    dat.subs <- dat.pheno$Date.Observed>=min(input$DateRange) & dat.pheno$Date.Observed<=max(input$DateRange) & dat.pheno$collection==input$Collection & dat.pheno$phenophase==input$Phenophase & !is.na(dat.pheno$status)
    
    txthere <- nearPoints(dat.pheno[dat.subs,c("phenophase", "Species", "PlantNumber", "Obs.List", "Observer","Date.Observed", "Timestamp", "status", "Notes")], 
                          input$plot_click, threshold =10, maxpoints=5)
    t(txthere)
    # names(txthere) <- "observation"
  })
}


shinyApp(ui, server)


