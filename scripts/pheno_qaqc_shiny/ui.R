# https://shiny.rstudio.com/articles/shinyapps.html?_ga=2.159806585.535201199.1597241310-391903967.1597085012
library(shiny)
library(ggplot2)
library(plotly)

dat.pheno <- read.csv("pheno_compiled.csv")
dat.pheno$Date.Observed <- as.Date(dat.pheno$Date.Observed)

fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  # tags$head(
  #   tags$style(HTML("
  #     pre, table.table {
  #       font-size: smaller;
  #     }
  #   "))),
  
  sidebarPanel(
    sliderInput("StartDate", "Start Date", min=min(dat.pheno$Date.Observed), max=max(dat.pheno$Date.Observed), value=min(dat.pheno$Date.Observed)),
    selectInput("Species", "Choose a Species:", list(Quercus=as.list(paste(unique(dat.pheno$Species))))), 
    selectInput("Phenophase", "Choose a Phenophase:", list(Phenos=as.list(paste(unique(dat.pheno$phenophase)))))
    ), 
  verbatimTextOutput("hover_info"),			    
  mainPanel(plotlyOutput("plot1"),
            # hover=hoverOpts(id="plot_hover")
))
