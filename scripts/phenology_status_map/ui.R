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
dat.pheno$Week <- lubridate::week(dat.pheno$Date.Observed)
dat.pheno$Week.Date <- as.Date(paste(lubridate::year(dat.pheno$Date.Observed), dat.pheno$Week, 1, sep="-"), "%Y-%U-%u")

# dat.pheno$Timestamp <- strptime(dat.pheno$Timestamp, format="")
coll.list <- paste(unique(dat.pheno$collection)[order(unique(dat.pheno$collection))])
pheno.list <- levels(dat.pheno$pheno.label)

fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  # titlePanel("Phenology Map for QAQC Data"),
  tags$head(
    tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))),
  sidebarPanel(sliderInput("Week", "Choose a Week:", min = min(as.Date(dat.pheno$Week.Date)), max = max(as.Date(dat.pheno$Week.Date)), value = max(as.Date(dat.pheno$Week.Date)), step=7),
               selectInput("collection", "Choose a collection:", list(collection=coll.list)),
               pickerInput("Phenophase", "Choose a Phenophase:", choices = list(Phenophase=pheno.list), select=pheno.list[1], options = list(`actions-box` = TRUE),multiple = T),
               uiOutput("select_Species")),
  #selectInput("Species", "Choose a Species:", choices = list(Species=as.list(paste(sort(unique(dat.all.stack$Species))))), selected = NULL, multiple = FALSE), #not filtering to only relevant species
  verbatimTextOutput("hover_info"),			    
  mainPanel(plotlyOutput("plot1", width = 850, height = 750),
            # hover=hoverOpts(id="plot_hover")
  ))