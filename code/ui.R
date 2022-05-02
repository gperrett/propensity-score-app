library(shiny)
library(shinydashboard)
library(shinyBS)

dashboardPage(
  skin="black",
  
  #Title
  dashboardHeader(title="Propensity Score Matching",titleWidth=235),
  
  #Sidebar
  dashboardSidebar(
    width = 235,
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction",tabName = "intro",icon = icon("book")),
      menuItem("Define Model", tabName = "model", icon = icon("dashboard")),
      menuItem("Result", tabName = "result", icon = icon("wpexplorer"))
      
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
      tabItem(tabName = "intro",
              h3(tags$b("What is Propensity Score? ")),
              h4(tags$li("text: propensity score introduction")),
              br(),
              h4(strong("Understanding propensity score matching:")),
              withMathJax(),
              h4(tags$li("text: what is propensity score matching?")),
              h4(tags$li("text: why propensity score matching could be counterintuitive? ")),
              br(),
              br(),
              h4(strong("What does this app do?")),
              withMathJax(),
              h4(tags$li("text: app function introduction")),
              h4(tags$li("1. Define Model: define your propensity score model ")),
              h4(tags$li("2. See Result: See the result ATE of your model and compare with the true ATE")),
              br(),
              
              div(style = "text-align: center",bsButton("goover", "Let's Have Some Fun!", icon("bolt"), size = "large")),
              br(),
              h3(tags$b("Acknowledgements:")),
              h4("This app was developed by George, Prianca, and Daisy. 
                             We have adapted and modified part of the skeleton code from an overfitting app made by Jinglin Feng and Alex Chen(BOAST). Special thanks to them  "),
              
      ),

      
      #Define the content contained within part 1 ie. tabname "result"
      tabItem(tabName = "model",
              # div(style="display: inline-block;vertical-align:top;",
              #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
              # ),
              fluidRow(
                withMathJax(),
                column(4,
                     #   h3("Introduction:"),
                     #   box(width ="10.5%",background = "maroon",
                     #       "You may observe overfitting as you increase the polynomial degree fitted on the training data. Every time
                     # you click the generate data button, a new dataset with size = 50(training data : testing data = 8:2) will be generated from  a linear relationship with some noise.
                     # After fitting the polynomial on the training dataset, we then test our model's perfomance on the test dataset.
                     # 
                     # For a better understanding, we also included the model performance on training and testing data measured by Mean Squared Error on the bottom"),
                     #   actionButton("plot", h5(tags$strong("Generate trainig and testing data"))), 
                       br(),
                       br(),
                       conditionalPanel("input.plot != 0",
                                        
                                          # choose dependent variable from dataset
                                          selectInput(inputId = 'models',
                                                      label = 'Choose a model for p-score',
                                                      choices = c('Logistic Regression', 'Random Forest', 'BART')),
                                          # If log reg is chosen, give option of profit or logit
                                          uiOutput('log_model_option'),
                                          selectInput(inputId = 'log_model_option',
                                                      label = 'choose a logistic regression model',
                                                      choices = NULL,),


                                        
                                        actionButton("fit_model", h5(tags$strong("Fit Model"))),
                                        br(),
                                        br()
                                        
                       )
                ),
                mainPanel(
                  # fluidRow(
                  #   splitLayout(cellWidths = c("50%", "50%"), plotOutput("train"), plotOutput("test"))),
                  # br(),
                  # br(),
                  # 
                  # fluidRow(
                  #   plotOutput("mse")
                  # )
                  
                  
                  
                  # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #000000}")),
                  # tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #000000}")),
                  # tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #000000}")),
                  
                  
                ))
              
              
      ),
      
      tabItem(tabName = "model",
              
              
      )
      
      
      
    )
    
    
    
    
    
  ))


# shinyUI(fluidPage(
# 
#   titlePanel("Propensity Score App"),
# 
#   # Load a local dataset
#   sidebarLayout(
#     sidebarPanel(
#   
#   # choose dependent variable from dataset
#   selectInput(inputId = 'models',
#               label = 'Choose a model for p-score',
#               choices = c('Logistic Regression', 'Random Forest', 'BART')),
#   # If log reg is chosen, give option of profit or logit
#   uiOutput('log_model_option'),
#   # selectInput(inputId = 'log_model_option',
#   #             label = 'choose a logistic regression model',
#   #             choices = NULL,)
#   # 
#   # 
#     ),
#   mainPanel(
#     "main panel"
#   )
# )))
# 
# 
