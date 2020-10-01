# Making some better graphs to show what's going on with the phenology in the living collections

# library(googlesheets4)

# source("clean_google_form.R")

library(ggplot2); library(grid) # graphing packages

# library(plotly)

library(car)
library(shiny)
library(shinyWidgets)
# ----------------
# get the data from each collection
# ----------------
dat.pheno <- read.csv("pheno_qaqc_shiny/pheno_compiled.csv")
dat.pheno$Date.Observed <- as.Date(dat.pheno$Date.Observed)
dat.pheno$pheno.label <- gsub(".observed", "", dat.pheno$phenophase)
dat.pheno$pheno.label <- car::recode(dat.pheno$pheno.label, 
                                     "'leaf.breaking.buds'='Leaves - Breaking Buds'; 
                                     'leaf.present'='Leaves - Present';
                                     'leaf.increasing'='Leaves - Increasing Size'; 
                                     'leaf.color'='Leaves - Fall Color'; 
                                     'leaf.falling'='Leaves - Falling'; 
                                     'flower.buds'='Flowers - Present (inc. buds)'; 
                                     'flower.open'='Flowers - Open Flowers'; 
                                     'flower.pollen'='Flowers - Pollen Release'; 
                                     'fruit.present'='Fruit - Present'; 
                                     'fruit.ripe'='Fruit - Ripe Fruit'; 
                                     'fruit.drop'='Fruit - Recent Drop'")
dat.pheno$pheno.label <- factor(dat.pheno$pheno.label, levels=c("Leaves - Present", 
                                                                "Leaves - Breaking Buds", 
                                                                "Leaves - Increasing Size",
                                                                "Leaves - Fall Color",
                                                                "Leaves - Falling",
                                                                "Flowers - Present (inc. buds)",
                                                                "Flowers - Open Flowers",
                                                                "Flowers - Pollen Release",
                                                                "Fruit - Present", 
                                                                "Fruit - Ripe Fruit", 
                                                                "Fruit - Recent Drop"))
dat.pheno$Week <- lubridate::week(dat.pheno$Date.Observed)
dat.pheno$Week.Date <- as.Date(paste(lubridate::year(dat.pheno$Date.Observed), dat.pheno$Week, 1, sep="-"), "%Y-%U-%u")

# dat.pheno <- dat.pheno[dat.pheno$Date.Observed>=as.Date("2020-08-01") & dat.pheno$collection=="Quercus",]
summary(dat.pheno)
# https://stackoverflow.com/questions/27965931/tooltip-when-you-mouseover-a-ggplot-on-shiny
coll.list <- paste(unique(dat.pheno$collection)[order(unique(dat.pheno$collection))])
pheno.list <- levels(dat.pheno$pheno.label)

# Calculating a number of individuals for each collection
nind <- list()
for(GENUS in coll.list){
  nind[[GENUS]] <- length(unique(dat.pheno$PlantNumber[dat.pheno$collection==GENUS]))
}


ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$style(HTML("
                  #info {
                    height:250px; width:250px;
                    overflow-y:scroll; word-wrap:break-word;
                    font-size:10px
                  }
                  ")),
  fluidRow(
    column(4,
    sliderInput("DateRange", "Start Date", min=min(dat.pheno$Date.Observed), max=max(dat.pheno$Date.Observed), value=c(min(dat.pheno$Date.Observed), max(dat.pheno$Date.Observed))),
    # selectInput("Collection", "Choose a Collection:", choices=c(list(Collection=as.list(paste(unique(dat.pheno$collection))))), 
    selectInput("Collection", "Choose a Collection:", list(Collection=as.list(coll.list))), 
    selectInput("Phenophase", "Choose a Phenophase:", list(Phenophase=as.list(pheno.list))), 
    # uiOutput("select_Species"),
    verbatimTextOutput("info"), style="position:fixed"), #, style="position:fixed"
  column(8,offset=4,mainPanel(uiOutput("plot.ui", click="plot_click")))
  ))			    


#goal: try to get a geom_bin2d graph to work in shiny
#so far almost everything of the graph is working: Date Observed is now normal but it has to be repeated twice for it to work
server <- function(input, output) {
  nInd <- reactive({
    length(unique(dat.pheno$PlantNumber[dat.pheno$Collection==input$Collection]))
  })
  plotHeight <- reactive(15*nInd())
  
  # output$select_Species <- renderUI({
  #   
  #   spp.avail <- unique(paste(dat.pheno$Species[dat.pheno$collection==input$Collection]))
  #   #selectizeInput('Species', 'Select Species', choices = c("select" = "", choice_Species()), multiple=TRUE) # <- put the reactive element here
  #   pickerInput('Species','Choose a Species: ', choices = c("select" = "", sort(spp.avail)), options = list(`actions-box` = TRUE, 'live-search' = TRUE),multiple = T)
  #   
  # })
  # 
  output$plot1 <- renderPlot({
    dat.subs <- dat.pheno$Date.Observed>=min(input$DateRange) & dat.pheno$Date.Observed<=max(input$DateRange) & dat.pheno$collection==input$Collection & dat.pheno$pheno.label==input$Phenophase & !is.na(dat.pheno$status) #& dat.pheno$Species %in% input$Species
    
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
                       plot.title = element_text(size=rel(2), face="bold", hjust=0.5), #formats title to be bold and in center
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
    
  })
  
  output$plot.ui <- renderUI({
    plotOutput("plot1", click="plot_click", width=500, height=plotHeight())
  })
  
  output$info <- renderPrint({
    dat.subs <- dat.pheno$Date.Observed>=min(input$DateRange) & dat.pheno$Date.Observed<=max(input$DateRange) & dat.pheno$collection==input$Collection & dat.pheno$pheno.label==input$Phenophase & !is.na(dat.pheno$status)
    
    txthere <- nearPoints(dat.pheno[dat.subs,c("pheno.label", "Species", "PlantNumber", "Obs.List", "Observer","Date.Observed", "Timestamp", "status", "Notes")], 
                          input$plot_click, threshold =5, maxpoints=5)
    txthere <- t(txthere)
    row.names(txthere) <- c("Phenophase", "Species", "PlantNumber", "Observing List", "Observer", "Date Observed", "Time Entered", "Phenophase Status", "Notes")
    txthere
    # names(txthere) <- "observation"
  })
}


shinyApp(ui, server)
