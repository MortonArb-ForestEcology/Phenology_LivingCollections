# https://shiny.rstudio.com/articles/shinyapps.html?_ga=2.159806585.535201199.1597241310-391903967.1597085012
# https://gitlab.com/snippets/16220
library(shiny)
library(ggplot2)
library(plotly)
library(stringr)

dat.pheno <- read.csv("pheno_compiled.csv")
dat.pheno$Date.Observed <- as.Date(dat.pheno$Date.Observed)
dat.pheno$status <- factor(dat.pheno$status, levels=c("No", "Yes", "Unsure", "No Observation", NA))
summary(dat.pheno)
# dat.pheno <- quercus.stack

server <- function(input, output) {
  output$plot1 <- renderPlot({
    dat.subs <- dat.pheno$Date.Observed>=min(input$DateRange) & dat.pheno$Date.Observed<=max(input$DateRange) & dat.pheno$Obs.List==input$ObsList & dat.pheno$phenophase==input$Phenophase & !is.na(dat.pheno$status)
    # cols.use <- colors.all$color.code[colors.all$pheno.status %in% dat.pheno$status[dat.subs]]
    
    print(ggplot(data=dat.pheno[dat.subs, ]) + # data being used
                     ggtitle(paste(input$Phenophase, "for", input$ObsList, sep=" ")) + # title
                     facet_grid(Species~., scales="free", space="free", switch="y") + # lines for different species +
                     geom_bin2d(aes(x=Date.Observed, y=PlantNumber, fill=status),  binwidth=7)) 
    
  })
  
  output$hover_info <- renderUI({
    dat.subs <- dat.pheno$Date.Observed>=min(input$DateRange) & dat.pheno$Date.Observed<=max(input$DateRange) & dat.pheno$Obs.List==input$ObsList & dat.pheno$phenophase==input$Phenophase & !is.na(dat.pheno$status)
    
    hover <- input$plot_hover
    point <- nearPoints(dat.pheno[dat.subs, ], hover, threshold=10, maxpoints=2, addDist=T)
    if(nrow(point) == 0) return(NULL)
    
    verbatimTextOutput(point)

  })

}




# text=stringr::str_wrap(paste('Date Observed:',Date.Observed,'<br>', "Date Entered: ", Timestamp, '<br>','Observer: ',Observer,'<br>', 'Status: ',status,'<br>','Plant Number: ', PlantNumber, '<br>', "Notes: ", Notes), indent=0, exdent=5)),