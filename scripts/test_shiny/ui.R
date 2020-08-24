# https://shiny.rstudio.com/articles/shinyapps.html?_ga=2.159806585.535201199.1597241310-391903967.1597085012
library(shiny)
library(ggplot2)
library(plotly)
library(stringr)

dat.pheno <- read.csv("pheno_compiled.csv")
dat.pheno$Date.Observed <- as.Date(dat.pheno$Date.Observed)
# dat.pheno$Timestamp <- strptime(dat.pheno$Timestamp, format="")
obs.list <- paste(unique(dat.pheno$Obs.List)[order(unique(dat.pheno$Obs.List))])
pheno.list <- paste(unique(dat.pheno$phenophase))

ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))),
  
    sliderInput("DateRange", "Start Date", min=min(dat.pheno$Date.Observed), max=max(dat.pheno$Date.Observed), value=c(min(dat.pheno$Date.Observed), max(dat.pheno$Date.Observed))),
    # selectInput("Collection", "Choose a Collection:", choices=c(list(Collection=as.list(paste(unique(dat.pheno$collection))))), 
    selectInput("ObsList", "Choose an Observing List:", list(ObsList=as.list(obs.list))), 
    selectInput("Phenophase", "Choose a Phenophase:", list(Phenophase=as.list(pheno.list))), 
  # verbatimTextOutput("hover_info"),
  
   plotOutput("plot1", hover=hoverOpts(id="plot_hover", delay=0)),
  uiOutput("hover_info")
)
