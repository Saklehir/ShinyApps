#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#load shiny app
library(shiny)

# create a function to pre-process the data
DataGetFunc <- function(datPath){
    library(soscisurvey)
    library(dplyr)
    library(reshape2)
    library(ggplot2)
    library(plotly)
    
    # get data from soscisurvey
    df <- read_sosci(datPath,
                     vSkip = "CASE, SERIAL, REF, MODE,STARTED",
                     vSkipTime = TRUE)
    
    # delete data labels
    df <- labelled::remove_labels(df)
    
    # change the data
    ### Renaming the columns 
    df <- df %>% mutate(MSC1 = case_when(
        SC48_01 == 2 ~ "Pow", 
        SC48_02 == 2 ~ "zero",
        SC48_03 == 2 ~ "Aff",
        SC48_04 == 2 ~ "Pow",
        SC48_05 == 2 ~ "Ach",
        SC48_06 == 2 ~ "Aff", 
        SC48_07 == 2 ~ "zero"))
    
    # SCC2
    df <- df %>% mutate(MSC2 = case_when(
        SC49_01 == 2 ~ "Pow", 
        SC49_02 == 2 ~ "zero",
        SC49_03 == 2 ~ "Aff",
        SC49_04 == 2 ~ "zero",
        SC49_05 == 2 ~ "Pow",
        SC49_06 == 2 ~ "Aff", 
        SC49_07 == 2 ~ "zero"))
    
    # SCC3
    df <- df %>% mutate(MSC3 = case_when(
        SC50_01 == 2 ~ "zero", 
        SC50_02 == 2 ~ "Pow",
        SC50_03 == 2 ~ "Ach",
        SC50_04 == 2 ~ "Pow",
        SC50_05 == 2 ~ "Aff",
        SC50_06 == 2 ~ "Pow", 
        SC50_07 == 2 ~ "zero"))
    
    # SCC4
    df <- df %>% mutate(MSC4 = case_when(
        SC51_01 == 2 ~ "Ach", 
        SC51_02 == 2 ~ "Aff",
        SC51_03 == 2 ~ "Pow",
        SC51_04 == 2 ~ "Pow",
        SC51_05 == 2 ~ "zero",
        SC51_06 == 2 ~ "zero"))
    
    # SCC5
    df <- df %>% mutate(MSC5 = case_when(
        SC52_01 == 2 ~ "Pow", 
        SC52_02 == 2 ~ "Ach",
        SC52_03 == 2 ~ "Ach",
        SC52_04 == 2 ~ "zero",
        SC52_05 == 2 ~ "zero"))
    
    # SCC6
    df <- df %>% mutate(MSC6 = case_when(
        SC53_01 == 2 ~ "Aff", 
        SC53_02 == 2 ~ "Ach",
        SC53_03 == 2 ~ "Pow",
        SC53_04 == 2 ~ "Pow",
        SC53_05 == 2 ~ "zero",
        SC53_06 == 2 ~ "zero",
        SC53_07 == 2 ~ "zero"))
    
    # SCC7
    df <- df %>% mutate(MSC7 = case_when(
        SC54_01 == 2 ~ "Pow", 
        SC54_02 == 2 ~ "zero",
        SC54_03 == 2 ~ "zero",
        SC54_04 == 2 ~ "Pow",
        SC54_05 == 2 ~ "Aff",
        SC54_06 == 2 ~ "zero",
        SC54_07 == 2 ~ "zero"))
    
    # SCC8
    df <- df %>% mutate(MSC8 = case_when(
        SC55_01 == 2 ~ "Aff", 
        SC55_02 == 2 ~ "Ach",
        SC55_03 == 2 ~ "Pow",
        SC55_04 == 2 ~ "zero",
        SC55_05 == 2 ~ "Pow",
        SC55_06 == 2 ~ "Pow",
        SC55_07 == 2 ~ "zero"))
    
    # SCC9
    df <- df %>% mutate(MSC9 = case_when(
        SC56_01 == 2 ~ "Ach", 
        SC56_02 == 2 ~ "Aff",
        SC56_03 == 2 ~ "zero",
        SC56_04 == 2 ~ "Pow",
        SC56_05 == 2 ~ "zero",
        SC56_06 == 2 ~ "zero"))
    
    # SCC10
    df <- df %>% mutate(MSC10 = case_when(
        SC57_01 == 2 ~ "Aff", 
        SC57_02 == 2 ~ "Ach",
        SC57_03 == 2 ~ "Pow",
        SC57_04 == 2 ~ "zero",
        SC57_05 == 2 ~ "Ach",
        SC57_06 == 2 ~ "zero",
        SC57_07 == 2 ~ "zero"))
    
    # SCC11
    df <- df %>% mutate(MSC11 = case_when(
        SC58_01 == 2 ~ "Ach", 
        SC58_02 == 2 ~ "Pow",
        SC58_03 == 2 ~ "Ach",
        SC58_04 == 2 ~ "zero",
        SC58_05 == 2 ~ "zero",
        SC58_06 == 2 ~ "zero",
        SC58_07 == 2 ~ "zero"))
    
    # SCC12
    df <- df %>% mutate(MSC12 = case_when(
        SC59_01 == 2 ~ "Pow", 
        SC59_02 == 2 ~ "Aff",
        SC59_03 == 2 ~ "zero",
        SC59_04 == 2 ~ "Pow",
        SC59_05 == 2 ~ "Ach",
        SC59_06 == 2 ~ "zero"))
    
    # SCC13
    df <- df %>% mutate(MSC13 = case_when(
        SC60_01 == 2 ~ "Pow", 
        SC60_02 == 2 ~ "zero",
        SC60_03 == 2 ~ "Pow",
        SC60_04 == 2 ~ "Ach",
        SC60_05 == 2 ~ "zero",
        SC60_06 == 2 ~ "Pow",
        SC60_07 == 2 ~ "zero"))
    
    # SCC14
    df <- df %>% mutate(MSC14 = case_when(
        SC61_01 == 2 ~ "Aff", 
        SC61_02 == 2 ~ "Ach",
        SC61_03 == 2 ~ "Pow",
        SC61_04 == 2 ~ "Ach",
        SC61_05 == 2 ~ "Pow",
        SC61_06 == 2 ~ "zero",
        SC61_07 == 2 ~ "zero"))
    
    # SCC15
    df <- df %>% mutate(MSC15 = case_when(
        SC62_01 == 2 ~ "Ach",
        SC62_02 == 2 ~ "Pow",
        SC62_03 == 2 ~ "zero",
        SC62_04 == 2 ~ "Aff",
        SC62_05 == 2 ~ "Pow",
        SC62_06 == 2 ~ "zero"))
    
    ## get individual implicit motive scores
    df$MSCAFFSUM <- rowSums(df[, c('MSC1', 'MSC2', 'MSC3', 'MSC4', 'MSC5', 'MSC6', 'MSC7','MSC8', 
                                   'MSC9', 'MSC10', 'MSC11', 'MSC12', 'MSC13', 'MSC14', 'MSC15')] == 'Aff')
    df$MSCACHSUM <- rowSums(df[, c('MSC1', 'MSC2', 'MSC3', 'MSC4', 'MSC5', 'MSC6', 'MSC7','MSC8', 
                                   'MSC9', 'MSC10', 'MSC11', 'MSC12', 'MSC13', 'MSC14', 'MSC15')] == 'Ach')
    df$MSCPOWSUM <- rowSums(df[, c('MSC1', 'MSC2', 'MSC3', 'MSC4', 'MSC5', 'MSC6', 'MSC7','MSC8', 
                                   'MSC9', 'MSC10', 'MSC11', 'MSC12', 'MSC13', 'MSC14', 'MSC15')] == 'Pow')
    
    
    # get only the columns needed
    df <- df[, c("CODE",'MSCACHSUM','MSCAFFSUM', 'MSCPOWSUM')]
    
    # delete na's
    df <- df[!is.na(df$MSCAFFSUM),]
    
    # melt data 
    df <- melt(df, id.vars = "CODE")
    
    # change the column names
    colnames(df)[c(2,3)] <- c('Motives', 'Value')
    
    
    ## now return the data
    df
}

datPath <- "https://www.soscisurvey.de/iemnMABA/?act=1mYym0m8hQg4t3RXkQOUaVuZ"

df <- DataGetFunc(datPath)

### create a function to show the other errors
errFunc <- function(UsInput){
    if(nchar(UsInput)< 8){
        print("Please Enter Your Individual Code!")
    }
    else if(grepl("[0-9][0-9][A-Z][A-Z][0-9][0-9][0-9][0-9]", UsInput, perl=TRUE)==FALSE){
        print("The Code is Invalid. Please Enter a Valid Code!")
    }
    else {
        NULL
    }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-size: 28px;
      }
    "))
    ),
    
    # Application title
    titlePanel(h1("Implicit Motive Profiles", align = "center")),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput('pers_code', 'Please enter your personal code here!', ''),
            actionButton('submit', "Submit"),
            br(), 
            br(), 
            h4("How to Interpret Your Profile?", align = "center"),
            p("The picture story you completed in the survey provides you with a personal profile on your so-called implicit motives. 
              During the excercise, you were shown 15 pictures and asked to come up with a story for each picture. Afterwards, you had to 
              select one statement that fitted your story best. The profile at hand is created by evaluating the statement you have selected 
              for each of the 15 pictures. Each statement represents either one of the three implicit motives (power, affiliation, and achievement) or an other category that covers motive irrelevant content. 
              Depending on the statement you selected for a given picture, you receive 1 point on either the power, affiliation, achievement motive or the other category. 
              In the end, your final score for each motive is calculated by taking the sum of points you have received for all 15 pictures. 
              Your final score for each motive therefore ranges from 0-15, considering you have not chosen any motive irrelevant statements. Let us assume, you have selected one motive irrelevant statement. In this case, your sum motive score across all motives is 14(e.g., 3 power, 5 affiliation, and 6 achievement).
              "),
            br(), 
            p("Unfortunately, there are no clear-cut criteria to determine when a motive is high or low. However, you can compare your motive scores with 
              each other to determine a more or less dominant motive. In the case that you have received a score of 3 on power, 5 on affiliation, and 6 on achievement, 
              you could consider your implicit achievement motive to be relatively high or dominant."), 
            br(),
            p("If you want to know more about implicit motives and/or your profile, feel free contact me at "),  strong("cafer.bakac@tum.de")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distPlot"),
            br(),
            h3("Implicit Motives", align="center"),
            p("Implicit motives reflect an individual's unconscious dispositions to experience certain kinds of incentives as pleasurable and inherently satisfying. 
            This is different for each individual. According to research, implicit motives are formed during first years childhood and are claimed to be rather unchangeable. 
            As the name implicit suggests, individuals are mostly unaware of their unconscious motives. Despite their relative unawareness, implicit motives are supposed to 
            energize, direct and maintain certain behaviors over time as individuals derive pleasure from them. For example, some people may derive pleasure from leading other, 
              others from climbing the Mount Everest, and yet others from spending time with loved ones. It is also possible to derive pleasure from all of these different activities. 
              In the end, creating a better understanding about your own implicit motives profile may help you to set the right goals in accordance with your motives which, in turn, 
              should not only make you goal-pursuit easier but also more pleasurable."),
            br(),
            h5(strong(em("1. Implicit Achievement Motive")), align='left'),
            p("Individuals who have high implicit achievement motive receive pleasure from tasks that are mid-challenging. They are concerned with 
              improving their performance and thus, seek feedback from others. They get pleasure from the task itself. For example, learning something that is 
              mid-difficult could be pleasureable in itself, not because learning is associated with good grades and so, better career."),
            h5(strong(em("2. Implicit Power Motive")), align='left'),
            p("Individuals who have high implicit power motive receive pleasure from having impact on other individuals. They want to have a good reputation and prestige.
                These people often want to be leaders and get pleasure from leading. One example of such a person could be Donald Trump, who enjoyed
                leading the United States regardless of what people think of him. Power motive is mostly regarded as something bad by individuals. However, 
                motives are not bad. Someone having a high power motive can influence the world/company in a positive way and at the same time, find this pleasureable."),
            h5(strong(em("3. Implicit Affiliation Motive")), align='left'),
            p("Individuals who have high implicit affiliation motive seek to build and maintain positive relationships with others. They avoid conflict situations, which 
              could hurt their relationships with others. They can find woking in companies/groups where personal relationships are also important better.
              They can take pleasure from working in groups, have meetings or coffee breaks with others."), 
            br(), 
            uiOutput("tab"),
            br()
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    # give the link to Schultheiss website for more information
    url <- a("Oliver Schultheiss", href="https://www.psych2.phil.uni-erlangen.de/team/schultheiss.shtml")
    output$tab <- renderUI({
        tagList("If you want to know more about implicit motives, here is the website of a great researcher in the area:", url)
    })
    
    # select the participant, whose text is entered (individual code)
    search.criteria <- reactive({
        validate(errFunc(input$pers_code))
        out <- which(df$CODE == input$pers_code)
        out
        
    })
    
    # plot the output
    output$distPlot <- renderPlotly({
        # draw the 
        p <- ggplot(df[search.criteria(),], aes(x=Motives, y=Value, group=1)) +
            geom_point(stat='summary', fun.y=sum, color="darkred", pch = 15) +
            stat_summary(fun.y=sum, geom="line", color = "darkred") + ylim(0,15) + coord_flip() + xlab('') + ylab('Motive Scores') + scale_x_discrete(labels=c('Implicit Achievement Motive', 'Implicit Affiliation Motive', 'Implicit Power Motive')) +
            theme(panel.grid.major = element_line(colour = "black"), 
                  panel.border = element_blank(), 
                  panel.background = element_rect(fill = NA), 
                  panel.ontop = TRUE) 
        fig <- ggplotly(p)
        fig
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
