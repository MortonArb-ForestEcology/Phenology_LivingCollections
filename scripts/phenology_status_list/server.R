# https://shiny.rstudio.com/articles/shinyapps.html?_ga=2.159806585.535201199.1597241310-391903967.1597085012
library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(shinyWidgets)

dat.pheno <- read.csv("pheno_compiled.csv")
dat.pheno$Date.Observed <- as.Date(dat.pheno$Date.Observed)
dat.pheno$status <- factor(dat.pheno$status, levels=c("No", "Yes", "Unsure", "No Observation", NA))
dat.pheno$pheno.label <- gsub(".observed", "", dat.pheno$phenophase)
dat.pheno$pheno.label <- car::recode(dat.pheno$pheno.label, 
                                     "'leaf.present'='Leaves - Present';
                                     'leaf.breaking.buds'='Leaves - Breaking Buds'; 
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
dat.pheno$Status.Intensity <- ifelse(dat.pheno$status=="No", "Absent", ifelse(dat.pheno$status=="Unsure", "Unsure", ifelse(dat.pheno$status=="No Observation", "No Observation",
                                     ifelse(dat.pheno$intensity %in% c(NA, "0", "0%", "None"), "Present", dat.pheno$intensity))))

dat.pheno$Status.Intensity <- factor(dat.pheno$Status.Intensity, levels=c("Absent", "Present", "Unsure", "No Observation", "<3", "3-10", "11-100", "101-1,000", "1,001-10,000", ">10,000", "<5%", "5-24%", "25-49%", "50-74%", "75-94%", ">95%", "Little", "Some", "Lots"))

summary(dat.pheno$Status.Intensity)
# dat.pheno <- quercus.stack

function(input, output) {
  
  output$select_Species <- renderUI({
    
    spp.avail <- unique(paste(dat.pheno$Species[dat.pheno$collection==input$Collection]))
    #selectizeInput('Species', 'Select Species', choices = c("select" = "", choice_Species()), multiple=TRUE) # <- put the reactive element here
    pickerInput('Species','Choose a Species: ', choices = c(sort(spp.avail)), selected= sort(spp.avail), options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = T)
    
  })
  
  nInd <- reactive({
    length(unique(dat.pheno$PlantNumber[dat.pheno$collection==input$Collection & dat.pheno$Species %in% input$Species]))
  })
  plotHeight <- reactive(15*nInd())
  

  
  output$plot1 <- renderPlot({
    dat.subs <- dat.pheno$Date.Observed>=min(input$DateRange) & dat.pheno$Date.Observed<=max(input$DateRange) & dat.pheno$collection==input$Collection & dat.pheno$pheno.label==input$Phenophase & !is.na(dat.pheno$status) & 
      dat.pheno$Species %in% input$Species
    
    # cols.use <- colors.all$color.code[colors.all$pheno.status %in% dat.pheno$status[dat.subs]]
    
    ggplot(data=dat.pheno[dat.subs, ]) + # data being used
      ggtitle(paste(input$Phenophase, "for", input$Collection, sep=" ")) + # title
      facet_grid(Species~., scales="free", space="free", switch="y") + # lines for different species +
      geom_point(aes(x=Date.Observed, y=PlantNumber, color=Status.Intensity), size=5, alpha=0.8) + # green filling & actual data
#       scale_color_manual(values = c("No Observation"="gray90", "Unsure"="#d95f02", "Absent"="#7570b3",  "Present"="#1b9e77", "<3"="#c51b7d", "3-10"="#e9a3c9", "11-100"="#fde0ef
# ", "101-1,000"="#e6f5d0", "1,001-10,000"="#a1d76a", ">10,000"="#4d9221",
#                                     "<5%"="#c51b7d", "5-24%"="#e9a3c9", "25-49%"="#fde0ef", "50-74%"="#e6f5d0", "75-94%"="#a1d76a", ">95%"="#4d9221",
#                                     "Little"="#e6f5d0", "Some"="#a1d76a", "Lots"="#4d9221"))  + # color scheme      
      scale_color_manual(values = c("No Observation"="gray90", "Unsure"="#fc8d62", "Absent"="#8da0cb",  "Present"="#66c2a5", "<3"="#ffffcc", "3-10"="#d9f0a3", "11-100"="#addd8e", "101-1,000"="#78c679", "1,001-10,000"="#31a354", ">10,000"="#006837",
                                    "<5%"="#ffffcc", "5-24%"="#d9f0a3", "25-49%"="#addd8e", "50-74%"="#78c679", "75-94%"="#addd8e", ">95%"="#31a354",
                                    "Little"="#f7fcb9", "Some"="#a1d76a", "Lots"="#4d9221"))  + # color scheme      
      scale_x_date(name="Date") + # x-axis and other stuff?
      # scale_y_discrete(expand=c(0,0)) + # fills in graph to make it solid
      scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +  # I'm not sure
      theme(legend.position="top", #need to move legend position
            legend.text = element_text(size=rel(1)),
            legend.title=element_blank(),
            legend.key = element_rect(fill=NA),
            plot.title = element_text(size=rel(1), face="bold", hjust=1), #formats title to be bold and in center
            panel.grid = element_blank(),
            panel.background=element_rect(fill="gray30", color="black"), #divider lines in , makes background white
            panel.spacing=unit(0.05, "lines"), #connects all the individual trees together
            axis.text.x=element_text(size=rel(1)),
            axis.title.x=element_text(size=rel(1), face="bold"), #makes x axis name bolded
            axis.title.y=element_blank(), #gets rid of y-axis name: I think it should be there
            axis.text.y=element_blank(), #makes it so that tree number is not displayed outside of gray part
            axis.ticks.y=element_blank(), #gets rid of ticks outside gray box of y-axis
            plot.margin=unit(c(0,5,0,0), "lines"),
            strip.text.y.left=element_text(margin=unit(c(1,0,1,0), "lines"), angle=0),
            strip.background = element_rect(fill="gray80")) #gets rid of ticks outside gray box of y-axis, also puts y-axis upside down which I fixed by changing angle to 0
    
  })
  
  output$plot.ui <- renderUI({
    plotOutput("plot1", click="plot_click", width=600, height=plotHeight())
  })
  
  
  output$info <- renderPrint({
    dat.subs <- dat.pheno$Date.Observed>=min(input$DateRange) & dat.pheno$Date.Observed<=max(input$DateRange) & dat.pheno$collection==input$Collection & dat.pheno$pheno.label==input$Phenophase & !is.na(dat.pheno$status)
    
    txthere <- nearPoints(dat.pheno[dat.subs,c("pheno.label", "Species", "PlantNumber", "Obs.List", "Observer","Date.Observed", "Timestamp", "status", "intensity", "Notes")], 
                          input$plot_click, threshold =10, maxpoints=5)
    txthere <- t(txthere)
    row.names(txthere) <- c("Phenophase", "Species", "PlantNumber", "Observing List", "Observer", "Date Observed", "Time Entered", "Phenophase Status", "Phenophase Intensity", "Notes")
    txthere
    # names(txthere) <- "observation"
  })
}
