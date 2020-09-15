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
# dat.pheno$Timestamp <- strptime(dat.pheno$Timestamp, format="")
coll.list <- paste(unique(dat.pheno$collection)[order(unique(dat.pheno$collection))])
pheno.list <- paste(unique(dat.pheno$pheno.label))

fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))),
  
    sliderInput("DateRange", "Start Date", min=min(dat.pheno$Date.Observed), max=max(dat.pheno$Date.Observed), value=c(min(dat.pheno$Date.Observed), max(dat.pheno$Date.Observed))),
    # selectInput("Collection", "Choose a Collection:", choices=c(list(Collection=as.list(paste(unique(dat.pheno$collection))))), 
    selectInput("Collection", "Choose a Collection:", list(Collection=as.list(coll.list))), 
    selectInput("Phenophase", "Choose a Phenophase:", list(Phenophase=as.list(pheno.list))), 
    mainPanel(plotOutput("plot1", click="plot_click"), height="100%"),
    verbatimTextOutput("info")
)
