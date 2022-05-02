library(shiny)
library(shinydashboard)
library(shinyBS)
dashboardPage(#skin="black",
              
              #Title
              dashboardHeader(title="Overfitting",titleWidth=235),
              
              #Sidebar
              dashboardSidebar(
                width = 235,
                sidebarMenu(
                  id = "tabs",
                  menuItem("Prerequisites",tabName = "pre",icon = icon("book")),
                  menuItem("Overview", tabName = "over", icon = icon("dashboard")),
                  menuItem("Exploration", tabName = "first", icon = icon("wpexplorer"))
                 
                )),
              
              #Content within the tabs
              dashboardBody(
                
                tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "sidebar.css"),
                    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #ffb6c1}")),
                    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #ffb6c1}")),
                    tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #ffb6c1}"))
                  ),
                
                tabItems(
                  tabItem(tabName = "pre",
                          HTML('<center><img src="overfit.gif" height = "400" width="500"></center>'),
                         # img(src = "overfit.gif", height = 400, width = 500),
                          h3(tags$b("Background: Overfitting")),br(),
                          h4(strong("Understanding the overfitting effect:")),
                          withMathJax(),
                          h4(tags$li("We say that a model is overfitted when it fits too closely to the training dataset. 
                          The model captures noise in the data and not just the underlying trends. The consequence of this is that the model may perform well on a training dataset but generalize poorly on a testing dataset. 
                                     ")),
                          
                          
                          h4(tags$li("Overfitting generally occurs because the model is too complicated or has too many features.
                                     The same can happen as you increase the degree in polynomial regression.")),
                          
                          br(),
                          
                          div(style = "text-align: center",bsButton("goover", "Go to the overview", icon("bolt"), size = "large"))
                  ),
                  tabItem(tabName = "over",
                          
                          br(),br(),br(),
                          h3(tags$b("About:")),
                          h4("This app explores how increasing the degree in the polynomial regression could result in overfitting "),
                          br(),
                          HTML('<center><img src="download.jpg" height = "200" width="300"></center>'),
                          h3(tags$b("Instructions:")),
                          
                          h4(tags$li("You need to ",
                                      
                                      tags$strong("first"), "click the ",
                                      tags$strong("Generate training and testing data button"),
                                      "and",
                                      tags$strong("then"), "after choosing the degree of th polynomial regression ",
                                      tags$strong("click the Fitting polynomials button "),"to visualize it.")),
                          
                          h4(tags$li("If you want to generate a new sample of data, just click the",
                                      tags$strong("Generate training and testing data button"),
                                      "again.")),
                          
                          div(style = "text-align: center",bsButton("explore", "Explore", icon("bolt"), size = "large")),
                          br(),
                          h3(tags$b("Acknowledgements:")),
                          h4("This app was developed by Dakota and Daisy. 
                             We have adapted and modified part of the skeleton code from an overfitting app made by Jinglin Feng and Alex Chen(BOAST). Special thanks to them  ")
                        
),

#Define the content contained within part 1 ie. tabname "first"
tabItem(tabName = "first",
        div(style="display: inline-block;vertical-align:top;",
            tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
        ),
        fluidRow(
          withMathJax(),
          column(4,
                 h3("Introduction:"),
                 box(width ="10.5%",background = "maroon",
                     "You may observe overfitting as you increase the polynomial degree fitted on the training data. Every time
                     you click the generate data button, a new dataset with size = 50(training data : testing data = 8:2) will be generated from  a linear relationship with some noise.
                     After fitting the polynomial on the training dataset, we then test our model's perfomance on the test dataset.
                     
                     For a better understanding, we also included the model performance on training and testing data measured by Mean Squared Error on the bottom"),
                 actionButton("plot", h5(tags$strong("Generate trainig and testing data"))), 
                 br(),
                 br(),
                 conditionalPanel("input.plot != 0",
                                  
                                                   sliderInput(
                                                     inputId = 'degree',
                                                     label = 'Degree of polynomial:',
                                                     value = 1,
                                                     min = 1,
                                                     max = 8,
                                                     step = 1
                                                   ),
                                  actionButton("validate", h5(tags$strong("Click to fit the chosen degree polynomial"))),
                                  br(),
                                  br()
                                  
                                  )
                ),
          mainPanel(
                    fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), plotOutput("train"), plotOutput("test"))),
                br(),
                br(),
               
                  fluidRow(
                  plotOutput("mse")
                )
                
              
                 
                 # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #000000}")),
                 # tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #000000}")),
                 # tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #000000}")),
                 

                                  ))
           
          
          )
        
          
          
          )
    


              
                  
))
