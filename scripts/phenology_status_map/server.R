# https://shiny.rstudio.com/articles/shinyapps.html?_ga=2.159806585.535201199.1597241310-391903967.1597085012
library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(rgdal)
library(shinyWidgets)

roads <- readOGR("data_spatial/roads/circ_veh_rd_2011-2020_ctrln.shp")
paths <- readOGR("data_spatial/trails/paths.shp")

roads <- spTransform(roads, CRS("+proj=longlat"))
paths <- spTransform(paths, CRS("+proj=longlat"))


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
dat.pheno$Week <- lubridate::week(dat.pheno$Date.Observed)
dat.pheno$Week.Date <- as.Date(paste(lubridate::year(dat.pheno$Date.Observed), dat.pheno$Week, 1, sep="-"), "%Y-%U-%u")
dat.pheno$Status.Intensity <- ifelse(dat.pheno$status=="No", "Absent", ifelse(dat.pheno$status=="Unsure", "Unsure", ifelse(dat.pheno$status=="No Observation", "No Observation",
                                                                                                                           ifelse(dat.pheno$intensity %in% c(NA, "0", "0%", "None"), "Present", dat.pheno$intensity))))

dat.pheno$Status.Intensity <- factor(dat.pheno$Status.Intensity, levels=c("Absent", "Present", "Unsure", "No Observation", "<3", "3-10", "11-100", "101-1,000", "1,001-10,000", ">10,000", "<5%", "5-24%", "25-49%", "50-74%", "75-94%", ">95%", "Little", "Some", "Lots"))
summary(dat.pheno)

# summary(dat.pheno)
# dat.pheno <- quercus.stack

server <- function(input, output) {
  
  #Used this website to create changing Species dropdown: https://www.davidsolito.com/post/conditional-drop-down-in-shiny/ 
  #Up to output plot is used to make Species dropdown reactive
  # Species.Choice <- reactive({
  # 
  #   dat.pheno %>%
  #     filter(collection == input$collection)
  # 
  # })
  
  output$select_Species <- renderUI({
    
    spp.avail <- unique(paste(dat.pheno$Species[dat.pheno$collection==input$collection]))
    #selectizeInput('Species', 'Select Species', choices = c("select" = "", choice_Species()), multiple=TRUE) # <- put the reactive element here
    pickerInput('Species','Choose a Species: ', choices = c(sort(spp.avail)), select=sort(spp.avail), options = list(`actions-box` = TRUE, 'live-search' = TRUE),multiple = T)
    
  })
  
  
  output$plot1 <- renderPlotly({
    dat.subs <- dat.pheno$collection==input$collection &  
      dat.pheno$pheno.label %in% input$Phenophase &  #allows multiple phenophases to show up
      dat.pheno$Week.Date==as.Date(input$Week) & 
      dat.pheno$Species %in% input$Species
    ggplotly(ggplot(data=dat.pheno[dat.subs, ]) + # data being used
               ggtitle(paste("Map of ", input$Phenophase, "for", input$collection, "on Week", input$Week,  sep=" ")) + # title
               facet_wrap(~pheno.label, ncol=2) + # lines for different species +
               geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=5, color="gray80") +
               geom_path(data=paths, aes(x=long, y=lat, group=group), size=1.5, linetype="solid", color="brown") +
               geom_point(aes(x=BgLongitude, y=BgLatitude, color=Status.Intensity,
                              text=paste('Date Observed:',Date.Observed,'<br>','Obs.List:',Obs.List,'<br>','Observer: ',Observer,'<br>', 'Phenophase Status: ',status,'<br>', 'Intensity: ',intensity,'<br>','Plant Number: ', PlantNumber,'<br>','Species: ', Species,'<br>','Notes: ', Notes,'<br>','Latitude: ', BgLatitude,'<br>','Longitude: ', BgLongitude)), size=3, alpha=0.8) + # green filling & actual data
               #       scale_color_manual(values = c("No Observation"="gray90", "Unsure"="#d95f02", "Absent"="#7570b3",  "Present"="#1b9e77", "<3"="#c51b7d", "3-10"="#e9a3c9", "11-100"="#fde0ef
               # ", "101-1,000"="#e6f5d0", "1,001-10,000"="#a1d76a", ">10,000"="#4d9221",
               #                                     "<5%"="#c51b7d", "5-24%"="#e9a3c9", "25-49%"="#fde0ef", "50-74%"="#e6f5d0", "75-94%"="#a1d76a", ">95%"="#4d9221",
               #                                     "Little"="#e6f5d0", "Some"="#a1d76a", "Lots"="#4d9221"))  + # color scheme      
               scale_color_manual(values = c("No Observation"="gray90", "Unsure"="#fc8d62", "Absent"="#8da0cb",  "Present"="#66c2a5", "<3"="#ffffcc", "3-10"="#d9f0a3", "11-100"="#addd8e", "101-1,000"="#78c679", "1,001-10,000"="#31a354", ">10,000"="#006837",
                                             "<5%"="#ffffcc", "5-24%"="#d9f0a3", "25-49%"="#addd8e", "50-74%"="#78c679", "75-94%"="#addd8e", ">95%"="#31a354",
                                             "Little"="#f7fcb9", "Some"="#a1d76a", "Lots"="#4d9221"))  + # color scheme      
               coord_equal(xlim=range(dat.pheno$BgLongitude[dat.subs], na.rm=T), 
                           ylim=range(dat.pheno$BgLatitude[dat.subs], na.rm=T)) +
               theme(legend.position="top", #need to move legend position
                     legend.text = element_text(size=rel(1)),
                     legend.title=element_blank(),
                     plot.title = element_text(size=rel(1), face="bold", hjust=1), #formats title to be bold and in center
                     panel.grid = element_blank(),
                     panel.background=element_rect(fill="gray30", color="black"), #divider lines in , makes background white
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