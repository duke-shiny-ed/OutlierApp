
library(shiny)
library(tidyverse)
library(dplyr)
library(knitr)
library(broom)
library(ggthemes)
library(RColorBrewer)
library(learnr)
library(shinythemes)
library(bootstraplib)

initial <- read_csv("data/airq-no-outliers.csv")
initial$outlier <- c(rep("no", 23))

randX1 <- runif(1, min = 5000, max=6000) # x value for mid income outlier
randX2 <- runif(1, min = 11000, max= 12500) # x value for first high income outlier
randX3 <- runif(1, min = 11000, max= 12500) # x value for second high income outlier
randX4 <- runif(1, min = 12000, max = 13000)
randY1 <- 0
randY2 <- runif(1, min = 13000, max = 15000)
randY3 <- runif(1, min = 12000, max = 15000)
randY4 <- runif(1, min = 100, max = 1000)

determiner <- runif(1, min = 0, max = 1)
ifelse (determiner<0.5, randY1 <- runif(1, min = 50, max = 150),
                                        randY1 <- runif(1, min = 9000, max = 11000))

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

# Now, create random values for added sample size
randX5 <- runif(5, min = 400, max = 4000)
randX6 <- runif(5, min = 4000, max = 9000)
randY5 <- runif(5, min = 200, max = 1000)
randY6 <- runif(5, min = 800, max = 5000)
randX7 <- runif(4, min = 9000, max = 11000)
randX8 <- runif(5, min = 11000, max = 13200)
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

# Define UI for application
ui <- fluidPage(
theme = shinytheme("simplex"),
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
                        fluidRow(
                            "In this app, you will learn methods for recognizing and treating outliers in your data.
            These include:"
                        ),
                        fluidRow(
                            column(6,
                                   h5("Identifying Outliers"),
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
                                   h5("Dealing with Outliers"),
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
                           "Click on the next tab, 'Identify Outliers', to get started!",
                           tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br() 
                        ), #end of third row
                        
                        fluidRow(
                            "The data on this site is a sample from an air quality data set for California metro areas.
                             It has been altered to include outliers for educational purposes. See the original data ",
                            tags$a(href="https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Airq.csv", "here."),
                            tags$br(), tags$br()
                        ), # end of fourth row
                        fluidRow(
                            tags$em("This site was created by Glen Morgenstern and is hosted by the Duke Statistical Science Department.")
                       ) #end of fifth row
               ), #end of first tab
               
               tabPanel("Identify Outliers",
                        
                        # Sidebar with checkboxes
                        sidebarLayout(
                            sidebarPanel(
                                helpText("On the right, you see a simple linear model relating median household income and value added by businesses. 
                     Toggle the checkboxes below to remove some data points from the model."),
                                checkboxGroupInput("remove",
                                                   "Click to remove:",
                                                   c("Middle Income Outlier",
                                                     "High Income Outliers")),
                            ),
                            # Show a plot of the generated model
                            mainPanel(
                                plotOutput("outlierGraph"),
                                verbatimTextOutput("outlierModel")
                            )
                        ) 
               ), #end of second tab
               
               tabPanel("Measure Outliers",
                        sidebarLayout(
                            sidebarPanel(
                                helpText("Toggle the radio buttons to learn how to calculate measures for outliers and what they mean."),
                                radioButtons("measure",
                                             "Choose your measure:",
                                             c("Leverage" = "leverage",
                                               "Standardized residuals" = "standardizedResiduals",
                                               "Cook's Distance" = "cooksDistance"
                                             )),
                                textOutput("measureDefinition"),
                                tags$br(),
                                uiOutput("measureFormula")
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
                                "Which observations can you identify as outliers?"
                                )
                        )
               )
               ), #end of third tab
               
               tabPanel("Solutions for Outliers",
                        sidebarLayout(
                            sidebarPanel(
                                helpText("What should you do with your outliers? See whether the actions below are appropriate."),
                                checkboxGroupInput("solution",
                                                   "Click to perform the indicated action on the model:",
                                                   c("Remove middle income outlier" = "mid4",
                                                     "Remove high income outliers" = "high4",
                                                     "Increase sample size" = "increaseSample",
                                                     "Log transform the data" = "logTransform"))
                            ),
                            mainPanel(
                                    wellPanel(plotOutput("solutionsGraph"))

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
    g <- ggplot(data=initialTab2, aes(x=medi, y=vala)) + geom_point() + 
        labs(title = "Business Value Added vs. Median Income",
             x = "Median Household Income", y = "Business Value Added") + 
        geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
        xlim(0, 13000) + ylim(0, 15000) + scale_color_brewer(palette = "Dark2")

    # Initial plot to graph when session starts

    
    output$outlierGraph <- renderPlot({
        if(is.null(input$remove)) { # When no checkboxes checked, change data frame to initialTab2
            g <- ggplot(data=initialTab2, aes(x=medi, y=vala, color=outlier)) + geom_point() + 
                labs(title = "Business Value Added vs. Median Income",
                     x = "Median Household Income", y = "Business Value Added") + 
                geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
                xlim(0, 13000) + ylim(0, 15000) + 
                theme_economist() + scale_color_brewer(palette = "Dark2")
        }
        if(length(input$remove) == 1) { # When one checkbox checked
            
            if (input$remove == "Middle Income Outlier") { # When only remove Middle Income Outlier checked
                g <- ggplot(data=noMedTab2, aes(x=medi, y=vala, color = outlier)) + geom_point() + 
                    labs(title = "Business Value Added vs. Median Income",
                         x = "Median Household Income", y = "Business Value Added") + 
                    geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
                    xlim(0, 13000) + ylim(0, 15000) + 
                    theme_economist() + scale_color_brewer(palette = "Dark2")
            }
            if(input$remove == "High Income Outliers") { #When only remove High Income Outliers is checked
                g <- ggplot(data=noHighTab2, aes(x=medi, y=vala, color = outlier)) + geom_point() + 
                    labs(title = "Business Value Added vs. Median Income",
                         x = "Median Household Income", y = "Business Value Added")+ 
                    geom_smooth(method = "lm", se = FALSE, aes(group=1), colour = "black") + 
                    xlim(0, 13000) + ylim(0, 15000) + 
                    theme_economist() + scale_color_brewer(palette = "Dark2")
            }
        }
        else if (length(input$remove == 2)){ # When both checkboxes are checked, use initial dataframe
            g <- ggplot(data=initial, aes(x=medi, y=vala, color = outlier)) + geom_point() + 
                labs(title = "Business Value Added vs. Median Income",
                     x = "Median Household Income", y = "Business Value Added") + 
                geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
                xlim(0, 13000) + ylim(0, 15000) + 
                theme_economist() + scale_color_brewer(palette = "Dark2")
            
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

#Code for measureDefinition below
output$measureDefinition <- renderText({
    definition = ""
    if(input$measure == "leverage") {
        definition = "Leverage is the measure of the distance between 
        an observation's values of the predictor variables and the 
        average values of the predictor variables for the whole data set. 
        Formula below: \n \n"
    }
    
    else if(input$measure == "standardizedResiduals") {
        definition = "The standardized residual is the residual divided by 
        the standard error. The residual is an observation's actual value minus 
        the model's predicted value for that observation. Formula below: \n \n"
    }
    
    else {
        definition = "Cook's Distance is a measure of an observation's 
        overall impact. It's the effect that removing the observation has on 
        the estimated coefficients. Formula below: \n \n"
    }
    definition
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
         y = "Leverage") + theme_economist()

    output$measureGraph <- renderPlot({
        if(input$measure == "leverage") {
            measurePlot <- ggplot(data = initial_aug, aes(x = obs_num, y = .hat)) +
                geom_point() +
                geom_hline(yintercept = leverageThresh, color = "red") +
                labs(title = "Leverage by Observation",
                     x = "Observation Number",
                     y = "Leverage") + 
                geom_text(aes(label = ifelse(.hat > leverageThresh,as.character(obs_num),""))) + 
                theme_economist()
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
                theme_economist()
        }
        else if(input$measure == "cooksDistance"){
            measurePlot <- ggplot(data = initial_aug, aes(x = obs_num, y = .cooksd)) +
                geom_point(alpha = 0.7) +
                geom_hline(yintercept=1,color = "red") +
                labs(x = "Observation Number", y = "Cook's Distance", title = "Cook's Distance") + 
                geom_text(aes(label = ifelse(.cooksd > 1,paste0("Obs. ",as.character(obs_num)),""))) + 
                theme_economist()
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
        
# When no checkboxes checked, change data frame to initialTab2
            s <- ggplot(data=initialTab2, aes(x=medi, y=vala, color=outlier)) + geom_point() + 
                labs(title = "(Unchanged) Value Added vs. Median Income",
                     x = "Median Household Income", y = "Business Value Added") + 
                geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
                xlim(0, 13000) + ylim(0, 15000) + scale_color_brewer(palette = "Dark2")
        
            if(is.null(input$solution)) { # When no checkboxes checked, change data frame to initialTab2
                g <- ggplot(data=initialTab2, aes(x=medi, y=vala, color=outlier)) + geom_point() + 
                    labs(title = "(Unchanged) Value Added vs. Median Income",
                         x = "Median Household Income", y = "Business Value Added") + 
                    geom_smooth(method = "lm", se = FALSE, aes(group = 1), colour = "black") + 
                    xlim(0, 13000) + ylim(0, 15000) + scale_color_brewer(palette = "Dark2")
            }
 
        s # plot s
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
