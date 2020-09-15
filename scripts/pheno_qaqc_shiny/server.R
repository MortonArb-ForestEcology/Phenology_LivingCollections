# https://shiny.rstudio.com/articles/shinyapps.html?_ga=2.159806585.535201199.1597241310-391903967.1597085012
library(shiny)
library(ggplot2)
library(plotly)
library(stringr)

dat.pheno <- read.csv("pheno_compiled.csv")
dat.pheno$Date.Observed <- as.Date(dat.pheno$Date.Observed)
dat.pheno$status <- factor(dat.pheno$status, levels=c("No", "Yes", "Unsure", "No Observation", NA))
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
summary(dat.pheno)
# dat.pheno <- quercus.stack

function(input, output) {
  output$plot1 <- renderPlot({
    dat.subs <- dat.pheno$Date.Observed>=min(input$DateRange) & dat.pheno$Date.Observed<=max(input$DateRange) & dat.pheno$collection==input$Collection & dat.pheno$pheno.label==input$Phenophase & !is.na(dat.pheno$status)
    
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
    dat.subs <- dat.pheno$Date.Observed>=min(input$DateRange) & dat.pheno$Date.Observed<=max(input$DateRange) & dat.pheno$collection==input$Collection & dat.pheno$pheno.label==input$Phenophase & !is.na(dat.pheno$status)
    
    txthere <- nearPoints(dat.pheno[dat.subs,c("pheno.label", "Species", "PlantNumber", "Obs.List", "Observer","Date.Observed", "Timestamp", "status", "Notes")], 
                          input$plot_click, threshold =10, maxpoints=5)
    t(txthere)
    # names(txthere) <- "observation"
  })
}
