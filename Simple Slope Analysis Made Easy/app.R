#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(PerformanceAnalytics)
library(shiny)
library(jtools)
#library(effects)
library(ggplot2)
library(grid)
library(ggeffects)
library(dplyr)

# get the dataset here 
data(mtcars)

data <- mtcars
# #
# sapply(data, class) # to check the types of each column
# #
# # ############ use the following function to make all of the factor columns as numeric
# for (i in 1:ncol(data)){     
#     data[,i] <- as.numeric(data[[i]])
# }
# 
# 
# write.csv(data, 'new_data.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
    # tags$head(tags$style(
    #   HTML('
    #      #sidebar {
    #         background-color: black;
    #     }
    # 
    #     body, label, input, button, select { 
    #       font-family: "Arial";
    #     }'))),
    # Application title
    titlePanel("Showing a correlation Matrix of the variables in a dataset"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(id="sidebar",
                     HTML('</br>'),
                     HTML('</br>'),
                     uiOutput('OutcomeVariable'),
                     HTML('</br>'),
                     uiOutput('PredictorVariable'),
                     HTML('</br>'),
                     uiOutput('Moderator'),
                     HTML('</br>'),
                     uiOutput("ControlV"),
                     HTML('</br>'),
                     #selectInput("dataset", h5("Choose the dataset:"), choices = c("data")),
                     HTML('</br>')
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Normality Plots", plotOutput("histNorm", height="500px", width = "500px"), plotOutput("plot2", height="500px", width = "500px"), align = 'center'),
                tabPanel("Correlations", plotOutput("plot", height="500px", width = "500px"), plotOutput("corr", height="500px", width = "500px"), align = 'center'),
                tabPanel("Regression", verbatimTextOutput("sslope")),
                tabPanel("Simple Slopes", plotOutput('SimpleSlope', height="500px", width = "600px"), plotOutput('datatable', height="500px", width = "500px"), align="center")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # 
    # datasetInput <- reactive({
    #   switch(input$dataset,
    #         "data" = data)
    #   })
  
    # dependent variable
    output$OutcomeVariable = renderUI({
      selectInput('OutcomeVariable', h5('Dependent Variable'), choices = names(data))
      })
  
    # independent variable
    output$PredictorVariable = renderUI({
     selectInput('PredictorVariable', h5('Independent Variable'), choices = names(data))
     })
    
    # dependent variable
    output$Moderator = renderUI({
      selectInput('Moderator', h5('Moderating Variable'), choices = names(data))
    })
    
    # independent variable
    output$ControlV = renderUI({
      selectInput('ControlV', h5('Control Variable'), choices = names(data))
    })
    
    
    
    # selectedData <- reactive({
    #     data[, c(input$OutcomeVariable, input$PredictorVariable, input$Moderator, input$ControlV)]
    # })
    
    #### centering them
    centeredData <- reactive({
      df <- data
      df[, input$PredictorVariable] <- as.numeric(scale(df[, input$PredictorVariable]), center=T, scale = F)
      df[, input$Moderator] <- as.numeric(scale(df[, input$Moderator]), center=T, scale = F)
      df[, input$ControlV] <- as.numeric(scale(df[, input$ControlV]), center=T, scale = F)
      df <- as.data.frame(df)
    })
    
    
    # regression formula
    
    regFormula <- reactive({
      
      as.formula(paste(input$OutcomeVariable, '~', paste(paste(input$PredictorVariable, input$Moderator, sep = '*'), input$ControlV, sep = '+') ))
    })
    
    # running the regression model model
    model <- reactive({
      lm(regFormula(), data = centeredData())
    })

    output$plot <- renderPlot({
      x <- as.numeric(data[, input$PredictorVariable])
      y <- as.numeric(data[, input$OutcomeVariable])
      z <- as.numeric(data[, input$Moderator])
      w <- as.numeric(data[, input$ControlV])
      corr <- data.frame(x, y, z, w)
        # 
        # x <- as.data.frame(c(datasetInput()[,input$OutcomeVariable],datasetInput()[,input$PredictorVariable], datasetInput()[,input$Moderator], 
        #                    datasetInput()[,input$ControlV]))
        # # generate bins based on input$bins from ui.R
        # draw the histogram with the specified number of bins
        chart.Correlation(corr, histogram=TRUE, method = c("pearson"), pch=19)
      
    })
    
    output$corr <- renderPlot({
      x <- as.numeric(data[, input$PredictorVariable])
      y <- as.numeric(data[, input$OutcomeVariable])
      plot(x, y,
           xlab="value of X", ylab="value of Y",
           main="Correlation",
           cex = 0.7, 
           col = "black"
           
      )
    })
    
    output$plot2 <- renderPlot({
      x <- as.numeric(data[, input$OutcomeVariable])
        qqnorm(x, pch = 1, frame = FALSE)
        qqline(x, col = "steelblue", lwd = 2)
    })
    
    output$histNorm <- renderPlot({
      x <- as.numeric(data[, input$OutcomeVariable])
      hist(x,probability=T, main="Histogram of normal data",xlab="Approximately normally distributed data", breaks = 20)
      lines(density(x),col=2)
    })
    
    output$sslope <- renderPrint({
      # bivariate model
        summary(model())
    })
    
    output$datatable <- renderPlot({
      
      ##### get the things based on the model
      p <- ggpredict(model(), c(input$PredictorVariable, input$Moderator))
      
      ## get the moderator as numeric
      p$group <- as.numeric(as.character(p$group))
      # get teh mean of moderator to exclude (1 + and - SD)
      mea <- mean(p$group)
      p <- filter(p, group !=mea)
      
      ## grouping based on the predictor (1SD lower as low and 1SD higher as High)
      p <- mutate(p, abc = ifelse(group > mea, 'High', 'Low'))
      
      ## DO THE SAME FOR THE PREDICTOR (THE FIRST 15 ARE LOW THE REST HIGH)
      p$x[1:(length(p$x)/2)] <- 'Low'
      p$x[(length(p$x)/2 + 1):length(p$x)] <- 'High'
      
      
      
      p <- p %>%
        group_by(x, abc) %>%
        summarise_at(vars(c(predicted, std.error, conf.low, conf.high)), funs(mean(., na.rm=TRUE)))
      
      plot <-  ggplot(data=p, aes(x=x, y=predicted, group=abc))+
         geom_line(size = 1) +
         geom_point(aes(shape = abc), stroke = 1.5, size = 5) +
         scale_shape_manual(values=c(0, 19)) +
         scale_color_grey() +
         labs(x = input$PredictorVariable, y= input$OutcomeVariable, shape = input$Moderator) +
         theme_bw() +
         theme(axis.line = element_line(size = 0.8, colour = "black"),
               axis.text = element_text( family = "arial", colour = "black", size = 11, face = "plain"),
               axis.ticks = element_line(size = 1.3),
               axis.title.y = element_text(family = "arial", size = 12, angle = 90),
               axis.title.x = element_text(family = "arial", size = 12),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               legend.title = element_text(family = "arial" ,face = "plain", color = 'black', size = 12),
               legend.text = element_text(family = "arial", face = "plain", colour = "black", size = 12))
      plot 
    })
    
    output$SimpleSlope <- renderPlot({
      ##### get the things based on the model
      pbar <- ggpredict(model(), c(input$PredictorVariable, input$Moderator))
      
      ## get the moderator as numeric
      pbar$group <- as.numeric(as.character(pbar$group))
      # get teh mean of moderator to exclude (1 + and - SD)
      meabar <- mean(pbar$group)
      pbar <- filter(pbar, group !=meabar)
      
      ## grouping based on the predictor (1SD lower as low and 1SD higher as High)
      pbar <- mutate(pbar, abc = ifelse(group > meabar, 'High', 'Low'))
      
      ## DO THE SAME FOR THE PREDICTOR (THE FIRST 15 ARE LOW THE REST HIGH)
      pbar$x[1:(length(pbar$x)/2)] <- 'Low'
      pbar$x[(length(pbar$x)/2 + 1):length(pbar$x)] <- 'High'
      
      
      
      pbar <- pbar %>%
        group_by(x, abc) %>%
        summarise_at(vars(c(predicted, std.error, conf.low, conf.high)), funs(mean(., na.rm=TRUE)))
      
      barchart <- ggplot(data = pbar, aes(x=x, y=predicted, fill=abc) ) +
        geom_bar(stat="identity", position="dodge", colour="black", legend=FALSE, width = 0.7) + 
        geom_errorbar(aes(ymin=predicted -std.error , ymax=predicted + std.error, width = 0.2), position=position_dodge(width=0.70)) +
        scale_fill_manual(values=c("grey80", "white")) + 
        scale_color_grey() +
        labs(x = input$PredictorVariable, y= input$OutcomeVariable, shape = input$Moderator) + 
        #scale_y_continuous(breaks=seq(0, 31, 3)) +  ## setting the limits of the y 
        theme_bw() +
        theme(axis.line = element_line(size = 0.6, colour = "black"), 
              axis.text = element_text( family = "arial", colour = "black", size = 11, face = "plain"),
              axis.ticks = element_line(size = 0.8),
              axis.title.y = element_text(family = "arial", size = 12, angle = 90),
              axis.title.x = element_text(family = "arial", size = 12),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              legend.title = element_text(family = "arial" ,face = "plain", color = 'black', size = 12),
              legend.text = element_text(family = "arial", face = "plain", colour = "black", size = 12))
      
      
      barchart
      
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
