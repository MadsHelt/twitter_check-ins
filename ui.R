library(shiny)
library(shinythemes)
library(d3heatmap)
library(leaflet.extras)
library(leaflet)
library(plotly)
library(shinyBS)
library(shinyjs)
library(dplyr)
library(quantmod)

shinyUI(
    navbarPage('Location Based Service', id = "panel", theme = shinytheme("cosmo"), 
               windowTitle = "Venue check-ins Tokyo", inverse = TRUE, 
             tabPanel(title = "Tokyo Map", 
                     
                      # Full screen mode for the map
                      tags$style(type = "text/css", "#scatterbox {
                                position: fixed; 
                                top: 41px;
                                 left: 0;
                                 right: 0;
                                 bottom: 0;
                                 overflow: hidden;
                                 padding: 0;}"),
                      
                      # Plotting the map 
                      plotlyOutput(outputId = "scatterbox", height = "100%", width = "100%"),

                      # Setting background for click menu
                      tags$head(tags$style(
                        HTML('#controls {background-color: rgba(250, 250, 250, 0.9);}'))),
                      
                      # Click here option to open absolutePanel
                      absolutePanel(id = "controls", fixed = TRUE,
                                    draggable = TRUE, top = 100, left = 10, right = "auto", bottom = "auto",
                                    width = 90, height = 40,
                                    
                                    checkboxInput(inputId = "somevalue", label = "Options", value = FALSE)),
                      br(),
                      
                      # Panel to change the map
                      absolutePanel(id = "popup", fixed = TRUE, draggable = TRUE, top = 150, left = 20,
                                    right = "auto", bottom = "auto", width = 250, height = "auto",
                                    
                                    conditionalPanel(condition = "input.somevalue == true",
                                                     selectizeInput(inputId = "social", width = 180, label = "Choose popularity", choices = c('Popular', 'Non-popular')),
                                                     selectizeInput(inputId = "gender", width = 180, label = "Choose gender", choices = c('male', 'female'), selected = 'male'),
                                                     selectizeInput(inputId = "weekd", width = 180, label = "Choose a weekday" ,choices = c('Monday', 'Tuesday', 'Wednesday', 
                                                                                                                                            "Thursday", "Friday", "Saturday",
                                                                                                                                            "Sunday")))),
                                                     #actionButton("reset_input", "Reset inputs"))),
                     
                      tags$head(tags$style(
                       HTML('#stat {background-color: rgba(0, 0, 0, 0.5);}'))),
             
                     # Descriptive stat: count_room, avgprice
                     absolutePanel(id = "stat", fixed = FALSE, draggable = TRUE, # class = "panel panel-default",
                                   top = 220, left = "auto", right = 20 , bottom = "auto",
                                   width = 200, height = "auto",
                                   # Not using this output yet
                                   plotlyOutput(outputId = "top5", height = 200),
                                   # Plotting bar chart of check_ins
                                   plotlyOutput(outputId = "checkin", height = 200))), # end of Tokyo panel

             # EDA tab
             navbarMenu("EDA",
                        # EDA sub-tab: Top-venues
                        tabPanel(title = "Top venues",
                          titlePanel("Top venues"),
                                 sidebarPanel(h4("Let's hang out at the train station!"),
                                              p("The Tokyo map didn't quite capture the best visited ventues. Likewise, it would also be interesting
                                     to explore how the check-ins are distributed throughout the day. By grouping by distinct venues 
                                                and stack the time of day for each venue, the following bar chart shows interesting findings."),
                                              br(),
                                              p("Surprisingly, venue check-ins at train stations and subways are a lot higher than check-ins at other venues.
                                                What is the reason for this? Do train stations and subways in Tokyo provide free Wi-Fi, which attract
                                                people to hang-out there, and is it a prerequisite to check-in before using the Wi-Fi? Or is cummuting just
                                                the time of the day where you use your phone?"),
                                              br(),
                                              p("In order to get a more sophisticated overview of the rest of the top venues, let's filter out train stations
                                                and subways (scroll down).")),
                                 mainPanel(
                                   plotlyOutput(outputId = "oneplot", height = 500),
                                   br(),
                                   br(),
                                   plotlyOutput(outputId = "twoplot", height = 500))),
                        
                        # EDA sub-tab: Time-series
                        tabPanel(title = "Check-ins by time",
                          titlePanel("Check-ins by time"),
                                 sidebarPanel(h4("Summer and winter seasonality"),
                                            p("Without conducting any statistical time-series analysis, it intuitively seems like there is a seasonal pattern
                                              between the sommer and winter months. Running from May and until the end of July, we observe a spike by 
                                              the number of check-ins. However, the number of check-ins drops drastically during the fall, but as we enter the
                                              winter months (especially December), the number of check-ins increases again. Logically, the seasonality could
                                              be due to holiday periods and thus increased activity on So-Me platforms.")),
                                 mainPanel(
                                   plotlyOutput(outputId = "threeplot"))),
                        
                        # EDA sub-tab: Demographic behaviour
                        tabPanel(title = "Demographic characteristics",
                          titlePanel("Demographic characteristics"),
                                 sidebarPanel(h4("Japaneese guys are active"),
                                              p("The first graph visualizes the difference in the number of check-ins by popularity. It does not seem like
                                              that there is a much difference between popular people and non-popular people according to the number of check-ins. 
                                              Thus, the two lines are very symmetrical during the whole observation period."),
                                              br(),
                                              p("However, looking at the other graph it is quite eye-cathing to see the difference between the number of 
                                                check-ins by gender. Males are much more active than females."),
                                              br(),
                                              p("The good reader will notice that the dataset is heavily imbalanced by a majority of males. 
                                                We there need to be careful about generalisation. But it is not excluded that Japaneese males use 
                                                the Twitter check-in function more often than Japaneese females and therefore appear more frequently is the dataset.")),
                                 mainPanel(
                                   plotlyOutput(outputId = "fiveplot", height = 240),
                                   br(),
                                   plotlyOutput(outputId = "fourplot", height = 240))
                        
                        
                        )), # end of EDA panel
             
             # Tab: Dataset
             tabPanel("Dataset",
                      hr(),
                      DT::dataTableOutput(outputId = "table"))
              )
      )



