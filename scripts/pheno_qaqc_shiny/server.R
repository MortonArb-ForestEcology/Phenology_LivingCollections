# https://shiny.rstudio.com/articles/shinyapps.html?_ga=2.159806585.535201199.1597241310-391903967.1597085012
library(shiny)
library(ggplot2)
library(plotly)

dat.pheno <- read.csv("pheno_compiled.csv")
dat.pheno$Date.Observed <- as.Date(dat.pheno$Date.Observed)
dat.pheno$status <- factor(dat.pheno$status, levels=c("No", "Yes", "Unsure", "No Observation", NA))
summary(dat.pheno)
# dat.pheno <- quercus.stack

colors.all <- data.frame(pheno.status = c("No", "Yes", "Unsure", "No Observation"), 
                         color.code = c("gray50","green4", "blue2", "black"))

function(input, output) {
  
  # dat.pheno <- reactive({
  #   quercus.stack[quercus.stack$Date.Observed>=input$StartDate,]
  #   })
  # cols.use <- reactive({
  #   paste(colors.all$color.code[colors.all$pheno.status %in% dat.pheno$status[dat.pheno$Date.Observed>=input$StartDate & dat.pheno$Obs.List==input$ObsList & dat.pheno$phenophase==input$Phenophase & !is.na(dat.pheno$status)]])}
  #   )
 
  
  output$plot1 <- renderPlotly({
    # if (input$plot_type == "base") {
    # plot(mtcars$wt, mtcars$mpg)
    # } else if (input$plot_type == "ggplot2") {
    # ggplot(mtcars, aes(wt, mpg, color=carb)) + geom_point()
    # }
    dat.subs <- dat.pheno$Date.Observed>=min(input$DateRange) & dat.pheno$Date.Observed<=max(input$DateRange) & dat.pheno$Obs.List==input$ObsList & dat.pheno$phenophase==input$Phenophase & !is.na(dat.pheno$status)
    
    cols.use <- colors.all$color.code[colors.all$pheno.status %in% dat.pheno$status[dat.subs]]
    
    print(
      ggplotly(ggplot(data=dat.pheno[dat.subs, ]) + # data being used
                     ggtitle(paste(input$Phenophase, "for", input$ObsList, sep=" ")) + # title
                     facet_grid(Species~., scales="free_y", switch="y") + # lines for different species +
                     geom_bin2d(aes(x=Date.Observed, y=PlantNumber, fill=status, 
                                    #label=Observer, label2=paste(Year, Month, Day, sep="-"), 
                                    text=paste('Date Observed:',Date.Observed,'<br>','Observer: ',Observer,'<br>', 'Status: ',status,'<br>','Plant Number: ', PlantNumber, '<br>', "Notes: ", Notes)), binwidth=7) + # green filling & actual data
                     scale_fill_manual(values= cols.use)  + # color scheme
                     scale_x_date(name="Date", expand=c(0,0)) + # x-axis and other stuff?
                     scale_y_discrete(expand=c(0,0)) + # fills in graph to make it solid
                     scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +  # I'm not sure
                     theme(legend.position="bottom", #need to move legend position
                           legend.text = element_text(size=rel(1)),
                           legend.title=element_blank(),
                           plot.title = element_text(size=rel(1), face="bold", hjust=1), #formats title to be bold and in center
                           panel.grid = element_blank(),
                           panel.background=element_rect(fill=NA, color="black"), #divider lines in , makes background white
                           panel.spacing=unit(0.05, "lines"), #connects all the individual trees together
                           axis.text.x=element_text(size=rel(1)),
                           axis.title.x=element_text(size=rel(1), face="bold"), #makes x axis name bolded
                           axis.title.y=element_blank(), #gets rid of y-axis name: I think it should be there
                           axis.text.y=element_blank(), #makes it so that tree number is not displayed outside of gray part
                           axis.ticks.y=element_blank(), #gets rid of ticks outside gray box of y-axis
                           strip.text = element_text(angle=90), 
                           plot.margin=unit(c(0,5,0,0), "lines"),
                           strip.text.y=element_text(size=rel(1), angle=0)), tooltip="text") %>% 
            layout(legend = list(orientation = "h", x = 0.4, y = -0.2, 
                                 margin = list(b = 50, l = 50)))) #gets rid of ticks outside gray box of y-axis, also puts y-axis upside down which I fixed by changing angle to 0
    
  })
}
