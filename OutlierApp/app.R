library(rsconnect)
library(shiny)
library(tidyverse)
library(dplyr)
library(knitr)
library(broom)
library(RColorBrewer)
library(learnr)
library(shinythemes)
library(bslib)
library(shinyBS)
library(DT
        )

initial <- read_csv("data/airq-no-outliers.csv") %>%
  rename(bva = vala)
initial$outlier <- c(rep("no", 23))


randX1 <- runif(1, min = 5500, max=7000) # random x value for mid income outlier
randX2 <- runif(1, min = 11000, max= 13000) # x value for first high income outlier
randX3 <- runif(1, min = 12000, max= 13000) # x value for second high income outlier
randX4 <- runif(1, min = 12000, max = 13000) # x value for third high income outlier
randY1 <- 0
randY2 <- runif(1, min = 13000, max = 16000)
randY3 <- runif(1, min = 13000, max = 16500)
randY4 <- runif(1, min = 400, max = 1500)

# Flip coin to see if middle income outlier has unusually high or low bva
determiner <- runif(1, min = 0, max = 1)
ifelse (determiner<0.5, randY1 <- runif(1, min = 25, max = 75),
                                        randY1 <- runif(1, min = 10000, max = 11000))

df1 = data.frame(X = c(31:34), airq = c(rep(1, 4)), bva = c(randY1, randY2, randY3, randY4), 
                 rain = c(rep(1, 4)), coas = c(rep("yes", 4)), dens = c(rep(1, 4)),
                 medi = c(randX1, randX2, randX3, randX4), outlier = rep("yes", 4)) # Create data frame from random outliers

df2 = data.frame(X = 31, airq = 1, bva = randY1, 
                 rain = 1, coas = "yes", dens = 1, 
                 medi = randX1, outlier = "yes") # Create data frame for noHighTab2

df3 = data.frame(X = c(32:34), airq = c(rep(1, 3)), bva = c(randY2, randY3, randY4), 
                 rain = c(rep(1, 3)), coas = c(rep("yes", 3)), dens = c(rep(1, 3)), 
                 medi = c(randX2, randX3, randX4), outlier = rep("yes", 3)) # Create data frame for noMedTab2

initialTab2 <- rbind(initial, df1) # data frame with all outliers included

noHighTab2 <- rbind(initial, df2) # data frame with no high income outliers

noMedTab2 <- rbind(initial, df3) # data frame with no medium income outliers

set.seed(33)

# Shuffle row numbers
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

df4 = data.frame(X = c(35:53), airq = c(rep(1, 19)), bva = largeSampleY, 
                 rain = c(rep(1, 19)), coas = c(rep("yes", 19)), dens = c(rep(1, 19)), 
                 medi = largeSampleX, outlier = rep("no", 19)) # Create data frame for largeSample

largeSample <- rbind(initialTab2, df4)
largeSampleNoMed <- rbind(noMedTab2, df4)
largeSampleNoHigh <- rbind(noHighTab2, df4)
largeSampleNoOutliers <- rbind(initial, df4)

# Create augmented data frame with predictions
initialModel <- lm(bva ~ medi, data = initialTab2)

initial_aug <- augment(initialModel) %>%
    mutate(obs_num = row_number())

leverageThresh = 2*(2 + 1) / nrow(initial_aug) # define leverage threshold

outlierData <- initial_aug %>% #Filter to find outliers
    filter(.cooksd > 1 | abs(.std.resid) > 2 | .hat > leverageThresh)

trueOutliers <- outlierData$obs_num #Create vector for true outlier obs nums
outlierString <- toString(trueOutliers)

# sidebarPanel2 <- function (..., out = NULL, width = 12) 
# {
#     div(class = paste0("col-sm-", width), 
#         tags$form(class = "well", ...),
#         out
#     )
# }

# Style with BootstrapLib
bs_theme_new("4", bootswatch = "simplex")

bs_theme_add_variables(
  primary = "#003087",
  secondary = "#003087",
  #"well-bg" = "mix(#444444, #e4e4e4, 80%)"
  "card-border-color" = "#003087",
  "card-border-radius" = 0,
  "card-border-width" = "0.05rem",
  "font-size-base" = "1.0rem",
  
)

bs_theme_accent_colors(primary = "#003087", secondary = "#003087")
bs_theme_fonts(
  base = "Georgia",
  code = c("Courier", "monospace"),
  heading = "'Arial', sans-serif"
)

## About Panel -----------------------------------------------------------------

# Define UI for application
ui <- navbarPage(
  theme = shinytheme("lumen"),
  title = "Outliers",
            tabPanel("About",
                     style = "font-size:20px",
                     withMathJax(),
                     tags$script("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']],
    processEscapes: true}});"),
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
                        tags$br(),
                        fluidRow(
                          column(5,
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
                          column(5,
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
                          ), 
                          column(2,
                                 img(src = "DukeShinyEdLogo.png", height = 140, width = 117))
                        ), #end of second row
                        fluidRow(
                          column(10, offset = 0.1,
                                 "Click on 'Identify Outliers' in the top menu to get started!"),
                          
                        )
                      ),
                     br(),
                      
                      fluidRow(
                        column(width = 12,
                        p("The data on this site is a sample from an air quality data set of California metro areas.
                             It has been altered to include randomly generated outliers for educational purposes. See the original data",
                        tags$a(href="https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Airq.csv", " here.")),
                      )), # end of fourth row
                    br(), hr(),
                      fluidRow(
                        column(width = 12, 
                        h2("Acknowledgements"),
                        p("This app was originally developed by Glen Morgenstern and updated by Sean Li.")
                      )),#end of fifth row
                      ),
              #end of first tab
  ## Explore tab----------------------------------------------------------------

    tabPanel("Explore",
             style = "font-size:20px",
              tabsetPanel(
               tabPanel("Fit the Model",
              
                        # Sidebar with checkboxes
                        sidebarLayout(position = "left",
                            sidebarPanel(
                             #    h4("Fit the Model"),
                                "The scatterplot shows the relationship between the median household income (Medi) and business value added (BVA) for 27 Californian Metro Areas.",
                                tags$br(),
                                 "The original model based on these 27 observations is: ",
                                 withMathJax("$\\hat{BVA} = -1496.350 + 0.939 \\times Medi$"),
                                
                                tags$hr(),
                                
                             #    "Let’s explore how the model, in particular the coefficient of (Medi),  changes if new observations are added to the data set.",  
                                 br(), 
                                 tags$b("Click to add points to the scatterplot. As you add points, the model will be refit using the a data set that includes the original data and the newly added points."), 
                                tags$br(),
                                tags$br(),
                                "Notice how the model changes as new points are added to the data set. ",
                                tags$ul(
                                 tags$li("How does intercept change?"), 
                                 tags$li(" How does the estimated coefficient of (Medi) change?"),
                                 tags$li(" How does the confidence interval for the coefficient of (Medi) change?")
                                ),
          
                                tags$hr(),
                                tags$br(),
                                "Try adding the following points to the data and see how the model changes:",
                                tags$ul(
                                  tags$li("Point that is an outlier in (Medi) but not in (BVA)"), 
                                  tags$li("Point that is an outlier in (BVA) but not in (Medi)"),
                                  tags$li("An influential point")
                                )
                                 
                            ),
                            
                            
                            # Show a plot of the generated model
                            mainPanel(
                              fluidRow(
                                column(width = 12, 
                                       br(),
                                       p(tags$b("Click to add points to the scatterplot. As you add points, the model will be refit using the a data set that includes the original data and the newly added points."), style = "color: red"), 
                                )
                              ),
                              br(), 
                                plotOutput("outlierGraph", click = "plot_click"),
                                "Model using original observations",
                                htmlOutput("staticModel"),
                                tags$br(),
                                tags$br(),
                                "Model using original AND new observations",
                                htmlOutput("outlierModel"),
                                verbatimTextOutput("clickInfo"),
                                actionButton("reset", label = "Reset to Original"),
                                tags$br()
                            ), fluid = TRUE
                        ) 
               ), #end of second tab
               
               tabPanel("Model Diagnostics",
                        sidebarLayout(
                            sidebarPanel(
                                h4("Model Diagnostics"),
                                "Toggle the radio buttons to learn how to calculate 
                                  measures for outliers and what they mean.",
                                tags$br(),
                                tags$br(),
                                radioButtons("measure",
                                             "Choose your measure:",
                                             c("Standardized residuals" = "standardizedResiduals",
                                               "Leverage" = "leverage",
                                               "Cook's Distance" = "cooksDistance"
                                             ))
                                ,
                                htmlOutput("measureDefinition"),
                                tags$br(),
                                uiOutput("measureFormula"),
                                    tags$br(),
                                    tags$br(),
                                    tags$hr(),
                               tags$b("Common Thresholds for Outliers"),
                               tags$br(),
                               "An observation can be considered an outlier if this conditions is satisfied:",
                               tags$br(),
                               tags$br(),
                                htmlOutput("thresMeasure"),
                                    tags$hr(),
                                    tags$b("Goal: "), "Learn three methods to identify 
                                              outliers in a data set."
                            ),
                            mainPanel(
                                plotOutput("measureGraph"),
                                wellPanel(
                                # tags$b("Common Thresholds for Outliers"),
                                # tags$br(),
                                # "An observation can be considered an outlier if at least one of these conditions is satisfied:",
                                # tags$br(),
                                # tags$br(),
                                # tags$b("1. "), withMathJax("Leverage > $\\large \\frac{2(p+1)}{n}$, where (p)=num. of predictor variables + 1 and (n) is number of observations, OR"),
                                # tags$br(),
                                # tags$b("2. "), "|Std. Resid.| > 2, OR",
                                # tags$br(),
                                # tags$b("3. "), "Cook's Distance > 1",
                                # tags$br(),
                                # tags$br(),
                                # tags$b("Exercise: "), "Which observations are outliers? Labeled points are the answers.",
                               # checkboxInput('answerVisible', tags$i('Show true outliers'), FALSE),
                               # uiOutput('outlierAnswers')
                                  tags$b("Table of User Added Observations:"),
                                  tags$br(),
                                  DTOutput(outputId = "dynamicTable")
                                ),
                                tags$br()
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
                                "**this data set is not the one used in previous tabs with added points**",
                                tags$br(),
                                tags$br(),
                                checkboxGroupInput("solution",
                                                   "Click to perform the indicated action on the model:",
                                                   choices = c("Remove middle income outlier",
                                                     "Remove high income outliers",
                                                     "Increase sample size",
                                                     "Log transform y values")),
                                
                                  tags$br(),
                                  tags$br(),
                                  tags$b("Goal: "), "Learn acceptable methods to deal with outliers 
                                        in a data set."
                            ),
                            
                            
                            mainPanel(
                                    plotOutput("solutionsGraph"),
                                    "$R^{2}$ below:",
                                    verbatimTextOutput("solutionsR2"),
                                    tags$h4(
                                        tags$b("Did I Choose a Reasonable Solution?")),
                                    wellPanel(htmlOutput("solutionsDescription", 
                                                         style = "font-size: 17px")),
                                    tags$br()
                            )
                        )
               ) #end of fourth tab
               ), #end of tab panel
               ), # End of explore,

## Resources -------------------------------------------------------------------

tabPanel("Resources",
         column(width = 12,
         style = "font-size:20px",
         # fluidRow(
         #   h4("Further Resources")
         # ),
         
         fluidRow(
           "Check out the links below to learn more about outlier detection and how to 
                          deal with them."),
         tags$br(),
         
         fluidRow(
           wellPanel(
             tags$ul(
               tags$li(tags$a(href = "https://www.khanacademy.org/math/statistics-probability/summarizing-quantitative-data/box-whisker-plots/a/identifying-outliers-iqr-rule",
                              "Khan Academy - Simple Outlier Identification")),
               tags$li(tags$a(href = "https://towardsdatascience.com/5-ways-to-detect-outliers-that-every-data-scientist-should-know-python-code-70a54335a623",
                              "Towards Data Science - DBScan, Isolation Forests, Random Cut Forests")),
               tags$li(tags$a(href = "http://r-statistics.co/Outlier-Treatment-With-R.html",
                              "Advanced Outlier Tutorial in R")),
               tags$li(tags$a(href = "https://www.openintro.org/book/os/",
                              "Free Open Intro Statistics Textbook")),
             )
           ),
           tags$br()
           
         )) # End of fluidRow
   )# end of resources

## Feedback --------------------------------------------------------------------
tabPanel("Feedback",
         style = "font-size: 20px",
         
         p("Use the form below to submit feedback about the app."), 
         br(), br(),
         
         tags$iframe(src = "https://forms.office.com/Pages/ResponsePage.aspx?id=TsVyyzFKnk2xSh6jbfrJTBw0r2_bKCVMs9lST1_-2sxURDBLWjhPVzlUUTE4UVlUR1pVNzlZT0NZVi4u",
                     width = "900", height = "400",
                     frameBorder="0")
)


)
# tabPanel("Quiz",
#          fluidRow(
#            tags$iframe(src = "https://glenmorgenstern.shinyapps.io/OutlierQuiz/",
#                        width = "1000", height = "1500",
#                        frameBorder="0")
#          )
# )# end of quiz tab
               ) # End of UI
    


# Define server logic required to draw model
server <- function(input, output) {

#Start of tab 2 code
    # output$identifyText <- renderText({"On the right, you see a simple linear model 
    #     relating median household income and value added by businesses. Toggle the 
    #     checkboxes below to include or exclude outliers from the model."})
    
    # g <- ggplot(data=initialTab2, aes(x=medi, y=bva)) + geom_point() + 
    #     labs(title = "Business Value Added vs. Median Income",
    #          x = "Median Household Income", y = "Business Value Added") + 
    #     geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
    #     xlim(0, 13000) + ylim(0, 16500) + scale_color_brewer(palette = "Dark2")

    # Initial plot to graph when session starts
    
    # creating reactive dataframe
    
    values <- reactiveValues()
    values$DT <- data.frame(medi = numeric(),
                            bva = numeric()
                            )
    
    
    
    # Start of outlierGraph code
    output$outlierGraph <- renderPlot({
        #if(is.null(input$include)) { # When no checkboxes checked, change data frame to initialTab2
            static <- ggplot(data=initialTab2, aes(x=medi, y=bva)) + geom_point() + 
                labs(title = "Business Value Added vs. Median Income",
                     x = "Median Household Income (Medi)", y = "Business Value Added (BVA)") + 
              geom_smooth(data = initialTab2, method = "lm", se = FALSE, aes(x=medi, y=bva), colour = "gray") + theme_bw() + theme(text = element_text(size=20))
        #this next line adds the new values on
            initialTab2 <- rbind.match.columns(initialTab2,values$DT) #combines reactive DF to initialTab2
             replot <- static + geom_point(data=values$DT, size = 2, aes(x=medi, y=bva), color = "red") +
                geom_smooth(data = initialTab2, method = "lm", se = FALSE, aes(x=medi, y=bva), colour = "black") + 
                xlim(0, 15000) + ylim(0, 25000) + 
                scale_color_brewer(palette = "Dark2")
            replot
        #}
        # if(length(input$include) == 1) { # When one checkbox checked
        #     
        #     if (input$include == "Middle Income Outlier") { # When only include Middle Income Outlier checked
        #         g <- ggplot(data=noHighTab2, aes(x=medi, y=bva, color = outlier)) + geom_point() + 
        #             labs(title = "Business Value Added vs. Median Income",
        #                  x = "Median Household Income", y = "Business Value Added") + 
        #             geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
        #             xlim(0, 13000) + ylim(0, 16500) + 
        #             scale_color_brewer(palette = "Dark2")
        #     }
        #     if(input$include == "High Income Outliers") { #When only include High Income Outliers is checked
        #         g <- ggplot(data=noMedTab2, aes(x=medi, y=bva, color = outlier)) + geom_point() + 
        #             labs(title = "Business Value Added vs. Median Income",
        #                  x = "Median Household Income", y = "Business Value Added")+ 
        #             geom_smooth(method = "lm", se = FALSE, aes(group=1), colour = "black") + 
        #             xlim(0, 13000) + ylim(0, 16500) + 
        #             scale_color_brewer(palette = "Dark2")
        #     }
        # }
        # else if (length(input$include == 2)){ # When both checkboxes are checked, use initialTab2 dataframe
        #     g <- ggplot(data=initialTab2, aes(x=medi, y=bva, color = outlier)) + geom_point() + 
        #         labs(title = "Business Value Added vs. Median Income",
        #              x = "Median Household Income", y = "Business Value Added") + 
        #         geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
        #         xlim(0, 13000) + ylim(0, 16500) + 
        #         scale_color_brewer(palette = "Dark2")
        #     
        # }
        
        
        

    })
    #End of outlierGraph code
    
    #code to add new entry in dataframe upon clicking plot
    
    observeEvent(input$plot_click, {
      # each input is a factor so levels are consistent for plotting characteristics
      add_row <- data.frame(medi = input$plot_click$x,
                            bva = input$plot_click$y
                           )
      # add row to the data.frame
      values$DT <- rbind(values$DT, add_row)
    })
    #reset points to original
    observeEvent(input$reset, {
      values$DT <- values$DT[0,]
    })
    
    #function to combine data frames for this tab
    
    rbind.match.columns <- function(input1, input2) {
      n.input1 <- ncol(input1)
      n.input2 <- ncol(input2)
      if (n.input2 < n.input1) {
        TF.names <- which(names(input2) %in% names(input1))
        column.names <- names(input2[, TF.names])
      } else {
        TF.names <- which(names(input1) %in% names(input2))
        column.names <- names(input1[, TF.names])
      }
      return(rbind(input1[, column.names], input2[, column.names]))
    }
    
    #Start of outlierModel code
    output$outlierModel <- renderText({
    initialTab2 <- rbind.match.columns(initialTab2,values$DT) #combines reactive DF to initialTab2
    tab2Model <- lm(bva ~ medi, data = initialTab2) #model statistics on the bottom
    
    # if(is.null(input$include)) {
    #     tab2Model <- lm(bva ~ medi, data = initial)
    # }
    # else if(length(input$include) == 1) {
    #     if (input$include == "Middle Income Outlier") {
    #         tab2Model <- lm(bva ~ medi, data = noHighTab2)
    #     }
    #     else if(input$include == "High Income Outliers") {
    #         tab2Model <- lm(bva ~ medi, data = noMedTab2)
    #     }
    # }
    # else if (length(input$include == 2)){
    #     tab2Model <- lm(bva ~ medi, data = initialTab2)
    #     
    # }
    
    tab2Model %>%
        tidy(conf.int = TRUE) %>%
        kable(format = "html", digits = 3) %>%
        kableExtra::kable_styling("striped", full_width = F, font_size = 15)
    
 
   
     })
    output$staticModel <- renderText({
      tab2Modelstatic <- lm(bva ~ medi, data = initialTab2) #model statistics on the bottom
      
      tab2Modelstatic %>%
        tidy(conf.int = TRUE) %>%
        kable(format = "html", digits = 3) %>%
        kableExtra::kable_styling("striped", full_width = F, font_size = 15)
    })
    #End of outlierModel code
    
    #Start of identifyExerciseAnswer code
    output$identifyExerciseAnswer <- renderText({
      answer <- ""
      if (is.null(input$identifyExercise)) {
        answer <- "Select an option"
      }
      else if(input$identifyExercise == "slopeIncrease") {
        answer <- "No. Hint: Does the model coefficient 'medi' increase, decrease, or stay the same?"
      }
      else if(input$identifyExercise == "slopeFall") {
        answer <- "Correct!"
      }
      else {
        answer <- "No. Hint: Does the model coefficient 'medi' increase or decrease?"
      }
      
      answer
      
    })
    
    # End of identifyExerciseAnswer code
    
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
output$thresMeasure <- renderText({
  thres = ""
  headert = ""
  
  
  if(input$measure == "leverage") {
    thres = withMathJax("Leverage > $\\large \\frac{2(p+1)}{n}$, where (p)=num. of predictor variables + 1 and (n) is number of observations\n")
  }
  
  else if(input$measure == "standardizedResiduals") {
    thres = "|Std. Resid.| > 2\n"
  }
  
  else {
    thres = "Cook's Distance > 1\n"
  }
  paste0("<font-weight:bold><b>", headert, "</b></font>", thres)
  
})

output$dynamicTable <- renderDT(
  datatable(values$DT)
)

#Code for outlierAnswers below
# output$outlierAnswers <- renderUI({
#     if (!input$answerVisible) return()
#     paste0("True outliers in this data set are observations ", outlierString)
# })

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
        where (p) is the number of predictor variables in the model and (h) is leverage.'
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
      initialTab2 <- rbind.match.columns(initialTab2,values$DT)
      initialModel <- lm(bva ~ medi, data = initialTab2)

      initial_aug <- augment(initialModel) %>%
        mutate(obs_num = row_number())

      leverageThresh = 2*(2 + 1) / nrow(initial_aug) # define leverage threshold

      outlierData <- initial_aug %>% #Filter to find outliers
         filter(.cooksd > 1 | abs(.std.resid) > 2 | .hat > leverageThresh)
      
       trueOutliers <- outlierData$obs_num #Create vector for true outlier obs nums
       outlierString <- toString(trueOutliers)
      
        if(input$measure == "leverage") {
            measurePlot <- ggplot(data = initial_aug, aes(x = obs_num, y = .hat)) +
                geom_point() +
                geom_hline(yintercept = leverageThresh, color = "red") +
                geom_vline(xintercept = 27.5, color = "red")+
                labs(title = "Leverage by Observation",
                     x = "Observation Number",
                     y = "Leverage") + 
                geom_text(aes(label = ifelse(.hat > leverageThresh,as.character(obs_num),"")),
                          position = position_nudge(y=0.005)) + 
                scale_color_brewer(palette = "Dark2") + theme_bw() +
              annotate("text", x = 15, y = 0.2302, label = "Outliers ABOVE red line")+
              annotate("text", x=23, y = 0.13, label = "added points RIGHT of line")
        }
        if(input$measure == "standardizedResiduals") {
            measurePlot <- ggplot(data = initial_aug, aes(x = .fitted, y = .std.resid)) +
                geom_point() +
                geom_hline(yintercept = -2, color = "red") +
                geom_hline(yintercept = 2, color = "red") +
                labs(title = "Standard Residuals vs. Predicted Value",
                     x = "Predicted",
                     y = "Standard Residuals") + 
                geom_text(aes(label = ifelse(abs(.std.resid) > 2,paste0("Obs. ", as.character(obs_num)),"")),
                          position = position_nudge(y=0.2)) + 
                scale_color_brewer(palette = "Dark2") + theme_bw() +
              annotate("text", x = 1000, y = c(2.2, -2.2), label = c("Outliers ABOVE this line", "Outliers BELOW this line"))
        }
        else if(input$measure == "cooksDistance"){
            measurePlot <- ggplot(data = initial_aug, aes(x = obs_num, y = .cooksd)) +
                geom_point(alpha = 0.7) +
                geom_hline(yintercept=1,color = "red") +
                geom_vline(xintercept = 27.5, color = "red")+
                labs(x = "Observation Number", y = "Cook's Distance", title = "Cook's Distance") + 
                geom_text(aes(label = ifelse(.cooksd > 1,paste0("Obs. ",as.character(obs_num)),"")),
                          position = position_nudge(y=0.1)) + 
                scale_color_brewer(palette = "Dark2") + theme_bw() +
              annotate("text", x = 15, y = 1.05, label = "Outliers ABOVE red line")+
              annotate("text", x=23, y = 1.5, label = "added points RIGHT of line")
        }
        
        measurePlot
    })


# Code for tab 3 start
    
    # Code for originalGraph
    # output$originalGraph <- renderPlot({
    #     ggplot(data=initialTab2, aes(x=medi, y=bva, color=outlier)) + geom_point() + 
    #         labs(title = "(Unchanged) Value Added vs. Median Income",
    #              x = "Median Household Income", y = "Business Value Added") + 
    #         geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
    #         xlim(0, 13000) + ylim(0, 15000) + scale_color_brewer(palette = "Dark2")
    # })
    
    # Code for solutionsGraph
    output$solutionsGraph <- renderPlot({
        
        solnGraph = ggplot()
        if (is.null(input$solution)) {
            solnGraph <- ggplot(data = initialTab2, aes(x = medi, y = bva, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "(Unchanged) Value Added vs. Household Income", 
                     x = "Median Household Income", y = "Business Value Added") + 
                scale_color_brewer(palette = "Dark2") + xlim(0, 13000) + ylim(0, 16500)
        }
        
        med4     <- "Remove middle income outlier" %in% input$solution
        high4    <- "Remove high income outliers"  %in% input$solution
        sample4 <- "Increase sample size"  %in% input$solution
        logTrans <- "Log transform y values" %in% input$solution
        
        if(med4 & high4 & sample4 & logTrans) {
            solnGraph <- ggplot(data = largeSampleNoOutliers, aes(x = medi, y = log(bva), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000) + 
                scale_color_brewer(palette = "Dark2")
        }
        else if (med4 & high4 & sample4){
            solnGraph <- ggplot(data = largeSampleNoOutliers, aes(x = medi, y = bva, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 16500)+ 
                scale_color_brewer(palette = "Dark2")
        } 
        else if (med4 & high4 & logTrans){
            solnGraph <- ggplot(data = initial, aes(x = medi, y = log(bva), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000)+ 
                scale_color_brewer(palette = "Dark2")
        } 
        else if (med4 & sample4 & logTrans){
            solnGraph <- ggplot(data = largeSampleNoMed, aes(x = medi, y = log(bva), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000)+ 
                scale_color_brewer(palette = "Dark2")
        } 
        else if (high4 & sample4 & logTrans) {
            solnGraph <- ggplot(data = largeSampleNoHigh, aes(x = medi, y = log(bva), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000) + 
                scale_color_brewer(palette = "Dark2")
        }  
        else if (med4 & high4){
            solnGraph <- ggplot(data = initial, aes(x = medi, y = bva, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 16500) + 
                scale_color_brewer(palette = "Dark2")
        } 
        else if (med4 & logTrans){
            solnGraph <- ggplot(data = noMedTab2, aes(x = medi, y = log(bva), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000)+ 
                scale_color_brewer(palette = "Dark2")
        } 
        else if (med4 & sample4){
            solnGraph <- ggplot(data = largeSampleNoMed, aes(x = medi, y = bva, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 16500)+ 
                scale_color_brewer(palette = "Dark2")
        } 
        else if (high4 & sample4) {
            solnGraph <- ggplot(data = largeSampleNoHigh, aes(x = medi, y = bva, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 16500)+ 
                scale_color_brewer(palette = "Dark2")
        }  
        else if(high4 & logTrans) {
            solnGraph <- ggplot(data = noHighTab2, aes(x = medi, y = log(bva), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000)+ 
                scale_color_brewer(palette = "Dark2")
        }
        else if(sample4 & logTrans) {
            solnGraph <- ggplot(data = largeSample, aes(x = medi, y = log(bva), color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Log(Value Added) vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Log(Business Value Added)") + 
                xlim(0, 13000)+ 
                scale_color_brewer(palette = "Dark2")
        }
        else if(med4) {
            solnGraph <- ggplot(data = noMedTab2, aes(x = medi, y = bva, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 16500)+ 
                scale_color_brewer(palette = "Dark2")
        }
        else if(high4) {
            solnGraph <- ggplot(data = noHighTab2, aes(x = medi, y = bva, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 16500)+ 
                scale_color_brewer(palette = "Dark2")
        }
        else if(sample4) {
            solnGraph <- ggplot(data = largeSample, aes(x = medi, y = bva, color = outlier)) + 
                geom_point() + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") + 
                labs(title = "Value Added vs. Household Income with Larger Sample", 
                     x = "Median Household Income", y = "Business Value Added") + 
                xlim(0, 13000) + ylim(0, 16500)+ 
                scale_color_brewer(palette = "Dark2")
        }
        else if(logTrans) {
            solnGraph <- ggplot(data = initialTab2, aes(x = medi, y = log(bva), color = outlier)) + 
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
        logTrans <- "Log transform y values" %in% input$solution
        
        if(med4 & high4 & sample4 & logTrans) {
            correct <- "Not quite"
            description <- ". Increasing sample size and log transformation are considered 
            acceptable here, but removing an outlier because of an unusually high/low 
            response variable is not acceptable. The medium income outlier is a legitimate 
            observation. The log transformation corrects for the 'fan' that 
            came with more observations, though it does make the model more complicated 
            to interpret. Also note that with a larger sample size, the old high income outlier 
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
            NOT acceptable. The medium income outlier is a legitimate observation. Also 
            note that the log transformation makes the model more complicated to interpret."
        } 
        else if (med4 & sample4 & logTrans){
            correct <- "Not quite"
            description <- ". Increasing sample size and log transformation are considered 
            acceptable here, but removing an outlier because of an unusually high/low 
            response variable is NOT acceptable. The medium income outlier is a legitimate 
            observation. The log transformation corrects for the 'fan' that 
            came with more observations, though it does make the model more complicated 
            to interpret. Good job noting that with a larger sample size, the old high income outlier 
            is no longer an outlier. No need to exclude it!"
        } 
        else if (high4 & sample4 & logTrans) {
            correct <- "Almost"
            description <- ". Increasing sample size and log transformation are considered 
            acceptable here. Also, good job noting that the middle income outlier is a legitimate 
            observation that you should keep. The log transformation corrects for the 'fan' that 
            came with more observations, though it does make the model more complicated 
            to interpret. Note that with a larger sample size, the 
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
            will diminish. Also note that the log transformation makes the model 
            more complicated to interpret."
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
            you cannot increase sample size, try a log transformation. Note that the log 
            transformation makes the model more complicated to interpret. Removing 
            outliers with unusually high or low values in a predictor variable is 
            acceptable. Good job! Now, try to find the other acceptable solutions."
        }
        else if(sample4 & logTrans) {
            correct <- "Yes"
            description <- ", this is a reasonable solution. Good job seeing 
            that with a larger sample size, the old high income outlier is no 
            longer an outlier. The log transformation corrects for the 'fan' that 
            came with more observations, though it does make the model more complicated 
            to interpret. Try to find the other acceptable solutions."
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
            helps account for the outliers. However, the model does become more complicated 
            to interpret. There are even better solutions! Try to 
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
    solnModel <- lm(bva ~ medi, data = initialTab2) # default
    
    if (is.null(input$solution)) {
        solnModel <- lm(bva ~ medi, data = initialTab2) # default
    }
    
    med4     <- "Remove middle income outlier" %in% input$solution
    high4    <- "Remove high income outliers"  %in% input$solution
    sample4 <- "Increase sample size"  %in% input$solution
    logTrans <- "Log transform y values" %in% input$solution
    
    if(med4 & high4 & sample4 & logTrans) {
        solnModel <- lm(log(bva) ~ medi, data = largeSampleNoOutliers)
    }
    else if (med4 & high4 & sample4){
        solnModel <- lm(bva ~ medi, data = largeSampleNoOutliers)
    } 
    else if (med4 & high4 & logTrans){
        solnModel <- lm(log(bva) ~ medi, data = initial)
    } 
    else if (med4 & sample4 & logTrans){
        solnModel <- lm(log(bva) ~ medi, data = noMedTab2)
    } 
    else if (high4 & sample4 & logTrans) {
        solnModel <- lm(log(bva) ~ medi, data = noHighTab2)
    }  
    else if (med4 & high4){
        solnModel <- lm(bva ~ medi, data = initial)
    } 
    else if (med4 & logTrans){
        solnModel <- lm(log(bva) ~ medi, data = noMedTab2)
    } 
    else if (med4 & sample4){
        solnModel <- lm(bva ~ medi, data = largeSampleNoMed)
    } 
    else if (high4 & sample4) {
        solnModel <- lm(bva ~ medi, data = largeSampleNoHigh)
    }  
    else if(high4 & logTrans) {
        solnModel <- lm(log(bva) ~ medi, data = noHighTab2)
    }
    else if(sample4 & logTrans) {
        solnModel <- lm(log(bva) ~ medi, data = largeSample)
    }
    else if(med4) {
        solnModel <- lm(bva ~ medi, data = noMedTab2)
    }
    else if(high4) {
        solnModel <- lm(bva ~ medi, data = noHighTab2)
    }
    else if(sample4) {
        solnModel <- lm(bva ~ medi, data = largeSample)
    }
    else if(logTrans) {
        solnModel <- lm(log(bva) ~ medi, data = initialTab2)
    }
    
        summary(solnModel)$r.squared
})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
