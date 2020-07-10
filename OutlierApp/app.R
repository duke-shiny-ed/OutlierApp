
library(shiny)
library(tidyverse)
library(dplyr)
library(ggthemes)

initial <- read_csv("data/airq-no-outliers.csv")

randX1 <- runif(1, min = 5000, max=6000) # x value for mid income outlier
randX2 <- runif(1, min = 10000, max= 11500) # x value for first high income outlier
randX3 <- runif(1, min = 10000, max= 11500) # x value for second high income outlier
randY1 <- 0
randY2 <- runif(1, min = 12000, max = 14000)
randY3 <- runif(1, min = 11000, max = 13000)

determiner <- runif(1, min = 0, max = 1)
ifelse (determiner<0.5, randY1 <- runif(1, min = 100, max = 300),
                                        randY1 <- runif(1, min = 7000, max = 10000))

df1 = data.frame(X = c(2:4), airq = c(rep(0, 3)), vala = c(randY1, randY2, randY3), 
                 rain = c(rep(0, 3)), coas = c(rep("", 3)), dens = c(rep(0, 3)),
                 medi = c(randX1, randX2, randX3)) # Create data frame from random outliers

initial <- rbind(initial, df1)

# Define UI for application
ui <- fluidPage(
    titlePanel("How to Identify and Deal with Outliers"),
    navbarPage("Tabs",
               tabPanel("About",
                        fluidRow(
                            column(3,
                                   h2("About")
                            )
                        ), # end of title row
                        fluidRow(
                            "In this app, you will learn methods for recognizing and treating outliers in your data.
            These include:"
                        ),
                        fluidRow(
                            column(6,
                                   h5("Identifying Outliers \n"),
                                   
                            ),
                            column(6,
                                   h5("Dealing with Outliers"),
                                   
                            )
                        ) #end of second row
               ), #end of first tab
               
               tabPanel("Identify Outliers",
                        
                        # Sidebar with checkboxes
                        sidebarLayout(
                            sidebarPanel(
                                helpText("On the right, you see a simple linear model relating median household income and value added by businesses. 
                     Toggle the checkboxes below to remove some data points from the model."),
                                checkboxGroupInput("remove",
                                                   "Click to remove:",
                                                   c("Middle Income Outliers" = "mid2",
                                                     "High Income Outliers" = "high2")),
                            ),
                            # Show a plot of the generated model
                            mainPanel(
                                plotOutput("outlierGraph")
                            )
                        ) 
               ), #end of second tab
               
               tabPanel("Measuring Outliers",
                        sidebarLayout(
                            sidebarPanel(
                                helpText("Toggle the radio buttons to learn how to calculate measures for outliers and what they mean."),
                                radioButtons("measure",
                                             "Choose your measure:",
                                             c("Cook's Distance" = "cooksDistance",
                                               "Standardized residuals" = "standardizedResiduals",
                                               "Leverage" = "leverage"
                                             ))
                            ),
                            mainPanel(
                                plotOutput("measureGraph"),
                                textOutput("measureExplanation")
                            )
                        )
               ), #end of third tab
               
               tabPanel("Solutions for Outliers",
                        sidebarLayout(
                            sidebarPanel(
                                helpText("Help text here"),
                                checkboxGroupInput("solution",
                                                   "Click to perform the indicated action on the model:",
                                                   c("Remove Middle Income Outlier" = "mid4",
                                                     "Remove High Income Outliers" = "high4",
                                                     "Log Transform the Data" = "logTransform"))
                            ),
                            mainPanel(
                                fluidRow(
                                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("originalGraph"), plotOutput("solutionsGraph"))
                                )
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
    
    output$outlierGraph <- renderPlot({
        ggplot(data=initial, aes(x=medi, y=vala)) + geom_point() + 
            labs(title = "Business Value Added vs. Median Income",
                 x = "Median Household Income", y = "Business Value Added") + 
            geom_smooth(method = "lm", se = FALSE)

    })
    
    output$originalGraph <- renderPlot({
        # Code for original graph
    })
    
    output$solutionsGraph <- renderPlot({
        # Code for solutions graph
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
