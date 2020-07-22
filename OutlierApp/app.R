library(rsconnect)
library(shiny)
library(tidyverse)
library(dplyr)
library(knitr)
library(broom)
library(RColorBrewer)
library(learnr)
library(shinythemes)
library(bootstraplib)
library(shinyBS)

initial <- read_csv("data/airq-no-outliers.csv")
initial$outlier <- c(rep("no", 23))


randX1 <- runif(1, min = 5500, max=7000) # x value for mid income outlier
randX2 <- runif(1, min = 11000, max= 12000) # x value for first high income outlier
randX3 <- runif(1, min = 11000, max= 12000) # x value for second high income outlier
randX4 <- runif(1, min = 12000, max = 12000)
randY1 <- 0
randY2 <- runif(1, min = 13000, max = 15000)
randY3 <- runif(1, min = 12000, max = 15000)
randY4 <- runif(1, min = 400, max = 1500)

determiner <- runif(1, min = 0, max = 1)
ifelse (determiner<0.5, randY1 <- runif(1, min = 50, max = 100),
                                        randY1 <- runif(1, min = 10000, max = 11000))

df1 = data.frame(X = c(31:34), airq = c(rep(1, 4)), vala = c(randY1, randY2, randY3, randY4), 
                 rain = c(rep(1, 4)), coas = c(rep("yes", 4)), dens = c(rep(1, 4)),
                 medi = c(randX1, randX2, randX3, randX4), outlier = rep("yes", 4)) # Create data frame from random outliers

df2 = data.frame(X = 31, airq = 1, vala = randY1, 
                 rain = 1, coas = "yes", dens = 1, 
                 medi = randX1, outlier = "yes") # Create data frame for noHighTab2

df3 = data.frame(X = c(32:34), airq = c(rep(1, 3)), vala = c(randY2, randY3, randY4), 
                 rain = c(rep(1, 3)), coas = c(rep("yes", 3)), dens = c(rep(1, 3)), 
                 medi = c(randX2, randX3, randX4), outlier = rep("yes", 3)) # Create data frame for noMedTab2

initialTab2 <- rbind(initial, df1)

noHighTab2 <- rbind(initial, df2)

noMedTab2 <- rbind(initial, df3)

set.seed(33)

initialTab2Rows <- sample(nrow(initialTab2))
noHighTab2Rows <- sample(nrow(noHighTab2))
noMedTab2Rows <- sample(nrow(noMedTab2))


initialTab2 <- initialTab2[initialTab2Rows, ]
noHighTab2 <- noHighTab2[noHighTab2Rows, ]
noMedTab2 <- noMedTab2[noMedTab2Rows, ]


# Now, create random values for added sample size
randX5 <- runif(5, min = 400, max = 4000)
randX6 <- runif(5, min = 4000, max = 9000)
randY5 <- runif(5, min = 200, max = 1200)
randY6 <- runif(5, min = 800, max = 4000)
randX7 <- runif(4, min = 9000, max = 11000)
randX8 <- runif(5, min = 11000, max = 13000)
randY7 <- runif(4, min = 4000, max = 10000)
randY8 <- runif(5, min = 100, max = 15000)

largeSampleX <- c(randX5, randX6, randX7, randX8)
largeSampleY <- c(randY5, randY6, randY7, randY8)

df4 = data.frame(X = c(35:53), airq = c(rep(1, 19)), vala = largeSampleY, 
                 rain = c(rep(1, 19)), coas = c(rep("yes", 19)), dens = c(rep(1, 19)), 
                 medi = largeSampleX, outlier = rep("no", 19)) # Create data frame for largeSample

largeSample <- rbind(initialTab2, df4)
largeSampleNoMed <- rbind(noMedTab2, df4)
largeSampleNoHigh <- rbind(noHighTab2, df4)
largeSampleNoOutliers <- rbind(initial, df4)

# Create augmented data frame with predictions
initialModel <- lm(vala ~ medi, data = initialTab2)

initial_aug <- augment(initialModel) %>%
    mutate(obs_num = row_number())

leverageThresh = 2*(2 + 1) / nrow(initial_aug) # define leverage threshold

outlierData <- initial_aug %>% #Filter to find outliers
    filter(.cooksd > 1 | abs(.std.resid) > 2 | .hat > leverageThresh)

trueOutliers <- outlierData$obs_num #Create vector for true outlier obs nums
outlierString <- toString(trueOutliers)

sidebarPanel2 <- function (..., out = NULL, width = 12) 
{
    div(class = paste0("col-sm-", width), 
        tags$form(class = "well", ...),
        out
    )
}

# Define UI for application
ui <- fluidPage(
theme = shinytheme("simplex"),
# Edit style for checkboxes and radio buttons
tags$style("
      input[type='checkbox']{ /* style for checkboxes */
        width: 10px; /*Desired width*/
        height: 10px; /*Desired height*/
        line-height: 8px; 
      }
      span { 
          margin-left: 5px;  /*set the margin, so boxes don't overlap labels*/
          line-height: 20px; 
      }
  "),
    bootstraplib::bootstrap(),
    titlePanel("How to Identify and Deal with Outliers"),
    withMathJax(),
    tags$script("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']],
    processEscapes: true}});"),
    navbarPage("Tabs",
               tabPanel("About",
                        fluidRow(
                            column(3,
                                   h2("About")
                            )
                        ), # end of title row
                        wellPanel(
                        fluidRow(
                            column(9, offset = 0.1,
                            "Outliers can be tricky to pin down. Once you've figured out where 
                            your outliers are, it's hard to know what do with them. In this app, 
                            you will learn methods for recognizing and treating outliers in your 
                            data. These include:",
                            style='border-right: 1px solid red'
                        ),
                            column(3,
                                   tags$i("Note: Google Chrome ", tags$b("strongly"), 
                                   " recommended for best user experience."))),
                        fluidRow(
                            column(6,
                                   h5(
                                       tags$b("Identifying Outliers")),
                                   tags$br(),
                                   tags$ul(
                                       tags$li("Leverage"),
                                       tags$li("Standardized residuals"),
                                       tags$li("Cook's Distance")
                                   ),
                                   tags$br(),
                                   tags$br()
                            ),
                            column(6,
                                   h5(
                                       tags$b("Dealing with Outliers")),
                                   tags$br(),
                                   tags$ul(
                                       tags$li("Transforming the data"),
                                       tags$li("Increasing sample size"),
                                       tags$li("Removing outliers")
                                   ),
                                   tags$br(),
                                   tags$br()
                            )
                        ), #end of second row
                        fluidRow(
                            column(12, offset = 0.1,
                           "Click on the next tab, 'Identify Outliers', to get started!"))),
                           tags$br(), tags$br(), tags$br(), tags$br(), #end of third row
                        
                        fluidRow(
                            "The data on this site is a sample from an air quality data set of California metro areas.
                             It has been altered to include randomly generated outliers for educational purposes. See the original data ",
                            tags$a(href="https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Airq.csv", " here."),
                            tags$br(), tags$br()
                        ), # end of fourth row
                        fluidRow(
                            tags$em("This site was created by Glen Morgenstern and is hosted by the Duke Statistical Science Department.")
                       ) #end of fifth row
               ), #end of first tab
               
               tabPanel("Identify Outliers",
                        
                        # Sidebar with checkboxes
                        sidebarLayout(position = "left",
                            sidebarPanel(
                                h4("Identify Outliers"),
                                span(textOutput("identifyText"), style="color:black"),
                                tags$br(),
                                checkboxGroupInput("include",
                                                   "Include:",
                                                   c("Middle Income Outlier",
                                                     "High Income Outliers"), 
                                                   selected = c("Middle Income Outlier",
                                                                "High Income Outliers")),
                                sidebarPanel2(
                                tags$br(),
                                tags$br(),
                                wellPanel(tags$b("Goal: "), "See how the model changes when you include 
                                          and exclude outliers from the data set!")),
                            ),
                            
                            # Show a plot of the generated model
                            mainPanel(
                                plotOutput("outlierGraph"),
                                verbatimTextOutput("outlierModel")
                            ), fluid = TRUE
                        ) 
               ), #end of second tab
               
               tabPanel("Measure Outliers",
                        sidebarLayout(
                            sidebarPanel(
                                h4("Measure Outliers"),
                                span(textOutput("measureText"), style="color:black"),
                                tags$br(),
                                radioButtons("measure",
                                             "Choose your measure:",
                                             c("Leverage" = "leverage",
                                               "Standardized residuals" = "standardizedResiduals",
                                               "Cook's Distance" = "cooksDistance"
                                             ))
                                ,
                                htmlOutput("measureDefinition"),
                                tags$br(),
                                uiOutput("measureFormula"),
                                sidebarPanel2(
                                    tags$br(),
                                    tags$br(),
                                    wellPanel(tags$b("Goal: "), "Learn three methods to identify 
                                              outliers in a data set."))
                            ),
                            mainPanel(
                                plotOutput("measureGraph"),
                                wellPanel(
                                tags$b("Common Thresholds for Outliers"),
                                tags$br(),
                                "An observation can be considered an outlier if:",
                                tags$br(),
                                tags$b("1. "), withMathJax("Leverage > $\\large \\frac{2(p+1)}{n}$, where (p)=num. of parameters + 1 and (n) is number of observations, OR"),
                                tags$br(),
                                tags$b("2. "), "|Std. Resid.| > 2, OR",
                                tags$br(),
                                tags$b("3. "), "Cook's Distance > 1",
                                tags$br(),
                                tags$br(),
                                checkboxInput('answerVisible', tags$i('Show true outliers'), FALSE),
                                uiOutput('outlierAnswers')
                                )
                        )
               )
               ), #end of third tab
               
               tabPanel("Solutions for Outliers",
                        sidebarLayout(
                            sidebarPanel(
                                h4("Deal with Outliers"),
                                "What should you do with your outliers? See whether the actions below are appropriate.",
                                tags$br(),
                                tags$br(),
                                checkboxGroupInput("solution",
                                                   "Click to perform the indicated action on the model:",
                                                   choices = c("Remove middle income outlier",
                                                     "Remove high income outliers",
                                                     "Increase sample size",
                                                     "Log transform the data"))
                            ),
                            mainPanel(
                                    plotOutput("solutionsGraph"),
                                    "$R^{2}$ below:",
                                    verbatimTextOutput("solutionsR2"),
                                    tags$h4(
                                        tags$b("Did I Choose the Right Solution?")),
                                    wellPanel(htmlOutput("solutionsDescription", 
                                                         style = "font-size: 14px"))
                            )
                        )
               ), #end of fourth tab
               tabPanel("Quiz",
                        "Insert Quiz"
               )
    )
)

# Define server logic required to draw model
server <- function(input, output) {

#Start of tab 2 code
    output$identifyText <- renderText({"On the right, you see a simple linear model 
        relating median household income and value added by businesses. Toggle the 
        checkboxes below to include or exclude outliers from the model."})
    
    g <- ggplot(data=initialTab2, aes(x=medi, y=vala)) + geom_point() + 
        labs(title = "Business Value Added vs. Median Income",
             x = "Median Household Income", y = "Business Value Added") + 
        geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
        xlim(0, 13000) + ylim(0, 15000) + scale_color_brewer(palette = "Dark2")

    # Initial plot to graph when session starts

    
    output$outlierGraph <- renderPlot({
        if(is.null(input$include)) { # When no checkboxes checked, change data frame to initialTab2
            g <- ggplot(data=initial, aes(x=medi, y=vala, color=outlier)) + geom_point() + 
                labs(title = "Business Value Added vs. Median Income",
                     x = "Median Household Income", y = "Business Value Added") + 
                geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
                xlim(0, 13000) + ylim(0, 15000) + 
                scale_color_brewer(palette = "Dark2")
        }
        if(length(input$include) == 1) { # When one checkbox checked
            
            if (input$include == "Middle Income Outlier") { # When only include Middle Income Outlier checked
                g <- ggplot(data=noHighTab2, aes(x=medi, y=vala, color = outlier)) + geom_point() + 
                    labs(title = "Business Value Added vs. Median Income",
                         x = "Median Household Income", y = "Business Value Added") + 
                    geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
                    xlim(0, 13000) + ylim(0, 15000) + 
                    scale_color_brewer(palette = "Dark2")
            }
            if(input$include == "High Income Outliers") { #When only include High Income Outliers is checked
                g <- ggplot(data=noMedTab2, aes(x=medi, y=vala, color = outlier)) + geom_point() + 
                    labs(title = "Business Value Added vs. Median Income",
                         x = "Median Household Income", y = "Business Value Added")+ 
                    geom_smooth(method = "lm", se = FALSE, aes(group=1), colour = "black") + 
                    xlim(0, 13000) + ylim(0, 15000) + 
                    scale_color_brewer(palette = "Dark2")
            }
        }
        else if (length(input$include == 2)){ # When both checkboxes are checked, use initialTab2 dataframe
            g <- ggplot(data=initialTab2, aes(x=medi, y=vala, color = outlier)) + geom_point() + 
                labs(title = "Business Value Added vs. Median Income",
                     x = "Median Household Income", y = "Business Value Added") + 
                geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
                xlim(0, 13000) + ylim(0, 15000) + 
                scale_color_brewer(palette = "Dark2")
            
        }
        g

    })

    
    output$outlierModel <- renderPrint({
    tab2Model <- lm(vala ~ medi, data = initialTab2)
    
    if(is.null(input$remove)) {
        tab2Model <- lm(vala ~ medi, data = initialTab2)
    }
    if(length(input$remove) == 1) {
        if (input$remove == "Middle Income Outlier") {
            tab2Model <- lm(vala ~ medi, data = noMedTab2)
        }
        if(input$remove == "High Income Outliers") {
            tab2Model <- lm(vala ~ medi, data = noHighTab2)
        }
    }
    else if (length(input$remove == 2)){
        tab2Model <- lm(vala ~ medi, data = initial)
        
    }
    
    tab2Model %>%
        tidy(conf.int = TRUE) %>%
        kable(format = "markdown", digits = 3)
   
     })

# End of tab 2 code
    
# Start of tab 3 code

output$measureText <- renderText({
    "Toggle the radio buttons to learn how to calculate 
    measures for outliers and what they mean."
})
#Code for measureDefinition below
output$measureDefinition <- renderText({
    definition = ""
    vocabWord = ""
    if(input$measure == "leverage") {
        vocabWord<-"Leverage "
        definition = "is the measure of the distance between 
        an observation's values of the predictor variables and the 
        average values of the predictor variables for the whole data set. 
        Formula below: \n \n"
    }
    
    else if(input$measure == "standardizedResiduals") {
        vocabWord <- "The standardized residual "
        definition <- "is the residual divided by 
        the standard error. The residual is an observation's actual value minus 
        the model's predicted value for that observation. Formula below: \n \n"
    }
    
    else {
        vocabWord <- "Cook's Distance "
        definition = "is a measure of an observation's 
        overall impact. It's the effect that removing the observation has on 
        the estimated coefficients. Formula below: \n \n"
    }
    paste0("<font-weight:bold><b>", vocabWord, "</b></font>", definition)
})

#Code for outlierAnswers below
output$outlierAnswers <- renderUI({
    if (!input$answerVisible) return()
    paste0("True outliers in this data set are observations ", outlierString)
})

# Code for measureFormula below
formula = ''
output$measureFormula <- renderUI({

    if (input$measure == "leverage") {
        withMathJax(
            '$\\large h_{i} = \\frac{1}{n}+\\frac{(x_{i}-\\bar{x})^{2}}{\\sum_{j=1}^{n}(x_{j}-\\bar{x})^{2}}$ 
            where (n) is the number of observations')
    }
    else if (input$measure == "standardizedResiduals") {
        withMathJax(
            '$\\large std. resid._{i}=\\frac{e_{i}}{\\widehat{\\sigma}\\sqrt{1-h_{i}}}$ 
            where $e_{i}$ is the residual (y-\\hat{y}) and $h_{i}$ is leverage.'
        )
    }
    else {
        withMathJax(
        '$\\large D_{i}=\\frac{1}{p}(std. res._{i})^{2}(\\frac{h_{i}}{1-h_{i}})$ 
        where (p) is the number of parameters in the model and (h) is leverage.'
        )
    }
    
    })

# Code for measureGraph below
measurePlot <- ggplot(data = initial_aug, aes(x = obs_num, y = .hat)) +
    geom_point() +
    geom_hline(yintercept = leverageThresh, color = "red") +
    labs(title = "Leverage by Observation",
         x = "Observation Number",
         y = "Leverage")

    output$measureGraph <- renderPlot({
        if(input$measure == "leverage") {
            measurePlot <- ggplot(data = initial_aug, aes(x = obs_num, y = .hat)) +
                geom_point() +
                geom_hline(yintercept = leverageThresh, color = "red") +
                labs(title = "Leverage by Observation",
                     x = "Observation Number",
                     y = "Leverage") + 
                geom_text(aes(label = ifelse(.hat > leverageThresh,as.character(obs_num),""))) + 
                scale_color_brewer(palette = "Dark2")
        }
        if(input$measure == "standardizedResiduals") {
            measurePlot <- ggplot(data = initial_aug, aes(x = .fitted, y = .std.resid)) +
                geom_point() +
                geom_hline(yintercept = -2, color = "red") +
                geom_hline(yintercept = 2, color = "red") +
                labs(title = "Standard Residuals vs. Predicted Value",
                     x = "Predicted",
                     y = "Standard Residuals") + 
                geom_text(aes(label = ifelse(abs(.std.resid) > 2,paste0("Obs. ", as.character(obs_num)),""))) + 
                scale_color_brewer(palette = "Dark2")
        }
        else if(input$measure == "cooksDistance"){
            measurePlot <- ggplot(data = initial_aug, aes(x = obs_num, y = .cooksd)) +
                geom_point(alpha = 0.7) +
                geom_hline(yintercept=1,color = "red") +
                labs(x = "Observation Number", y = "Cook's Distance", title = "Cook's Distance") + 
                geom_text(aes(label = ifelse(.cooksd > 1,paste0("Obs. ",as.character(obs_num)),""))) + 
                scale_color_brewer(palette = "Dark2")
        }
        
        measurePlot
    })


# Code for tab 3 start
    
    # Code for originalGraph
    # output$originalGraph <- renderPlot({
    #     ggplot(data=initialTab2, aes(x=medi, y=vala, color=outlier)) + geom_point() + 
    #         labs(title = "(Unchanged) Value Added vs. Median Income",
    #              x = "Median Household Income", y = "Business Value Added") + 
    #         geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
    #         xlim(0, 13000) + ylim(0, 15000) + scale_color_brewer(palette = "Dark2")
    # })
    
    # Code for solutionsGraph
    output$solutionsGraph <- renderPlot({
        
        solnGraph = ggplot()
        if (is.null(input$solution)) {
            solnGraph <- ggplot(data = initialTab2, aes(x = medi, y = vala, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "(Unchanged) Value Added vs. Household Income", 
                     x = "Median Household Income", y = "Business Value Added") + 
                scale_color_brewer(palette = "Dark2")
        }
        
        med4     <- "Remove middle income outlier" %in% input$solution
        high4    <- "Remove high income outliers"  %in% input$solution
        sample4 <- "Increase sample size"  %in% input$solution
        logTrans <- "Log transform the data" %in% input$solution
        
        if(med4 & high4 & sample4 & logTrans) {
            solnGraph <- ggplot(data = largeSampleNoOutliers, aes(x = medi, y = log(vala), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000) + 
                scale_color_brewer(palette = "Dark2")
        }
        else if (med4 & high4 & sample4){
            solnGraph <- ggplot(data = largeSampleNoOutliers, aes(x = medi, y = vala, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 15000)+ 
                scale_color_brewer(palette = "Dark2")
        } 
        else if (med4 & high4 & logTrans){
            solnGraph <- ggplot(data = initial, aes(x = medi, y = log(vala), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000)+ 
                scale_color_brewer(palette = "Dark2")
        } 
        else if (med4 & sample4 & logTrans){
            solnGraph <- ggplot(data = largeSampleNoMed, aes(x = medi, y = log(vala), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000)+ 
                scale_color_brewer(palette = "Dark2")
        } 
        else if (high4 & sample4 & logTrans) {
            solnGraph <- ggplot(data = largeSampleNoHigh, aes(x = medi, y = log(vala), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000) + 
                scale_color_brewer(palette = "Dark2")
        }  
        else if (med4 & high4){
            solnGraph <- ggplot(data = initial, aes(x = medi, y = vala, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 15000) + 
                scale_color_brewer(palette = "Dark2")
        } 
        else if (med4 & logTrans){
            solnGraph <- ggplot(data = noMedTab2, aes(x = medi, y = log(vala), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000)+ 
                scale_color_brewer(palette = "Dark2")
        } 
        else if (med4 & sample4){
            solnGraph <- ggplot(data = largeSampleNoMed, aes(x = medi, y = vala, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 15000)+ 
                scale_color_brewer(palette = "Dark2")
        } 
        else if (high4 & sample4) {
            solnGraph <- ggplot(data = largeSampleNoHigh, aes(x = medi, y = vala, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 15000)+ 
                scale_color_brewer(palette = "Dark2")
        }  
        else if(high4 & logTrans) {
            solnGraph <- ggplot(data = noHighTab2, aes(x = medi, y = log(vala), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000)+ 
                scale_color_brewer(palette = "Dark2")
        }
        else if(sample4 & logTrans) {
            solnGraph <- ggplot(data = largeSample, aes(x = medi, y = log(vala), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000)+ 
                scale_color_brewer(palette = "Dark2")
        }
        else if(med4) {
            solnGraph <- ggplot(data = noMedTab2, aes(x = medi, y = vala, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 15000)+ 
                scale_color_brewer(palette = "Dark2")
        }
        else if(high4) {
            solnGraph <- ggplot(data = noHighTab2, aes(x = medi, y = vala, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 15000)+ 
                scale_color_brewer(palette = "Dark2")
        }
        else if(sample4) {
            solnGraph <- ggplot(data = largeSample, aes(x = medi, y = vala, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 15000)+ 
                scale_color_brewer(palette = "Dark2")
        }
        else if(logTrans) {
            solnGraph <- ggplot(data = initialTab2, aes(x = medi, y = log(vala), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000)+ 
                scale_color_brewer(palette = "Dark2")
        }
        
        solnGraph
    })
    
    # Code for solutionsDescription
    output$solutionsDescription <- renderText({
        correct = ""
        description = ""
        if (is.null(input$solution)) {
            description = "Click one or more checkboxes to find the acceptable 
            solutions for these outliers."
        }
        
        med4     <- "Remove middle income outlier" %in% input$solution
        high4    <- "Remove high income outliers"  %in% input$solution
        sample4 <- "Increase sample size"  %in% input$solution
        logTrans <- "Log transform the data" %in% input$solution
        
        if(med4 & high4 & sample4 & logTrans) {
            correct <- "Not quite"
            description <- ". Increasing sample size and log transformation are considered 
            acceptable here, but removing an outlier because of an unusually high/low 
            response variable is not acceptable. The medium income outlier is a legitimate 
            observation. Note that with a larger sample size, the old high income outlier 
            is no longer an outlier. No need to exclude it!"
        }
        else if (med4 & high4 & sample4){
            correct <- "Not quite"
            description <- ". Increasing sample size is considered 
            acceptable here, but removing an outlier because of an unusually high/low 
            response variable is NOT acceptable. The medium income outlier is a legitimate 
            observation. Also note that with a larger sample size, the old high income outlier 
            is no longer an outlier. No need to exclude it!"
        } 
        else if (med4 & high4 & logTrans){
            correct <- "Not quite"
            description <- ". A log transformation is acceptable here, but 
            removing an outlier because of an unusually high/low response variable is 
            NOT acceptable. The medium income outlier is a legitimate observation."
        } 
        else if (med4 & sample4 & logTrans){
            correct <- "Not quite"
            description <- ". Increasing sample size and log transformation are considered 
            acceptable here, but removing an outlier because of an unusually high/low 
            response variable is NOT acceptable. The medium income outlier is a legitimate 
            observation. Good job noting that with a larger sample size, the old high income outlier 
            is no longer an outlier. No need to exclude it!"
        } 
        else if (high4 & sample4 & logTrans) {
            correct <- "Almost"
            description <- ". Increasing sample size and log transformation are considered 
            acceptable here. Also, good job noting that the middle income outlier is a legitimate 
            observation that you should keep. However, note that with a larger sample size, the 
            old high income outlier is no longer an outlier. No need to exclude it!"
        }  
        else if (med4 & high4){
            correct <- "No!"
            description <- " There are better strategies than simply removing the outliers. 
            Removing an outlier because of an unusually high/low response variable is NOT acceptable. 
            The medium income outlier is a legitimate observation."
        } 
        else if (med4 & logTrans){
            correct <- "No!"
            description <- " The log transformation is considered 
            acceptable here, but removing an outlier because of an unusually high/low 
            response variable is NOT acceptable. The medium income outlier is a legitimate 
            observation. However, removing an outlier because of unusual values in a 
            predictor variable is acceptable, though the predictive range of the model 
            will diminish."
        } 
        else if (med4 & sample4){
            correct <- "No!"
            description <- " Increasing sample size is considered 
            acceptable here, but removing an outlier because of an unusually high/low 
            response variable is NOT acceptable. The medium income outlier is a legitimate 
            observation. However, removing an outlier because of unusual values in a 
            predictor variable is acceptable, though the predictive range of the model 
            will diminish."
        } 
        else if (high4 & sample4) {
            correct <- "Almost"
            description <- ". Increasing sample size was a good idea, but 
            note that with a larger sample size, the old high income outlier is 
            no longer an outlier. No need to exclude it!"
        }  
        else if(high4 & logTrans) {
            correct <- "Yes"
            description <- ", this is an acceptable solution! If for some reason, 
            you cannot increase sample size, try a log transformation. Removing 
            outliers with unusually high or low values in a predictor variable is 
            acceptable. Good job! Now, try to find the other acceptable solutions."
        }
        else if(sample4 & logTrans) {
            correct <- "Yes"
            description <- ", this is the optimal solution! Good job seeing 
            that with a larger sample size, the old high income outlier is no 
            longer an outlier. The log transformation corrects for the 'fan' that 
            came with more observations. Try to find the other acceptable solutions."
        }
        else if(med4) {
            correct <- "No!"
            description <- " Removing an outlier because of an unusually high/low 
            response variable is NOT acceptable. The medium income outlier is a legitimate 
            observation. However, removing an outlier because of unusual values in a 
            predictor variable is acceptable, though the predictive range of the model 
            will diminish."
        }
        else if(high4) {
            correct <- "Yes"
            description <- ", this is considered acceptable. Removing an outlier because 
            of unusual values in a predictor variable is acceptable, though the predictive 
            range of the model will diminish. There are better solutions available, though. 
            Try to find them."
        }
        else if(sample4) {
            correct <- "Almost"
            description <- ". Increasing sample size was a good idea because 
            your old high income outliers are no longer outliers. However, there is 
            now a 'fan' pattern in the data. What method might correct this?"
        }
        else if(logTrans) {
            correct <- "Yes"
            description <- ", this is an acceptable solution. The log transformation 
            helps account for the outliers. But there are even better solutions! Try to 
            find them."
        }
        
        if (correct == "No!" | correct =="Not quite") {
            paste0("<font color=\"#FF0000\"><b>", correct, "</b></font>", description)
        }
        else if (correct == "Almost") {
            paste0("<font color=\"#e6e619\"><b>", correct, "</b></font>", description)
        }
        else {
            paste0("<font color=\"#008000\"><b>", correct, "</b></font>", description)
        }
    })
    
    # Code for solutionsR2
    output$solutionsR2 <- renderPrint({
    solnModel <- lm(vala ~ medi, data = initialTab2) # default
    
    if (is.null(input$solution)) {
        solnModel <- lm(vala ~ medi, data = initialTab2) # default
    }
    
    med4     <- "Remove middle income outlier" %in% input$solution
    high4    <- "Remove high income outliers"  %in% input$solution
    sample4 <- "Increase sample size"  %in% input$solution
    logTrans <- "Log transform the data" %in% input$solution
    
    if(med4 & high4 & sample4 & logTrans) {
        solnModel <- lm(log(vala) ~ medi, data = largeSampleNoOutliers)
    }
    else if (med4 & high4 & sample4){
        solnModel <- lm(vala ~ medi, data = largeSampleNoOutliers)
    } 
    else if (med4 & high4 & logTrans){
        solnModel <- lm(log(vala) ~ medi, data = initial)
    } 
    else if (med4 & sample4 & logTrans){
        solnModel <- lm(log(vala) ~ medi, data = noMedTab2)
    } 
    else if (high4 & sample4 & logTrans) {
        solnModel <- lm(log(vala) ~ medi, data = noHighTab2)
    }  
    else if (med4 & high4){
        solnModel <- lm(vala ~ medi, data = initial)
    } 
    else if (med4 & logTrans){
        solnModel <- lm(log(vala) ~ medi, data = noMedTab2)
    } 
    else if (med4 & sample4){
        solnModel <- lm(vala ~ medi, data = largeSampleNoMed)
    } 
    else if (high4 & sample4) {
        solnModel <- lm(vala ~ medi, data = largeSampleNoHigh)
    }  
    else if(high4 & logTrans) {
        solnModel <- lm(log(vala) ~ medi, data = noHighTab2)
    }
    else if(sample4 & logTrans) {
        solnModel <- lm(log(vala) ~ medi, data = largeSample)
    }
    else if(med4) {
        solnModel <- lm(vala ~ medi, data = noMedTab2)
    }
    else if(high4) {
        solnModel <- lm(vala ~ medi, data = noHighTab2)
    }
    else if(sample4) {
        solnModel <- lm(vala ~ medi, data = largeSample)
    }
    else if(logTrans) {
        solnModel <- lm(log(vala) ~ medi, data = initialTab2)
    }
    
        summary(solnModel)$r.squared
})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
