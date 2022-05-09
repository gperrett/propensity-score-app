library(shiny)
library(shinydashboard)
library(shinyBS)
library(sortable)
library(dplyr)
library(shinycssloaders)

dashboardPage(
  # skin="black",
  
  #Title
  dashboardHeader(title="Propensity Score Matching",titleWidth=235),
  
  #Sidebar
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction",tabName = "intro",icon = icon("book")),
      menuItem("Define Model", tabName = "model", icon = icon("dashboard")),
      menuItem("Result", tabName = "result", icon = icon("wpexplorer"))
      
    )),
  
  #Content within the tabs
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href ="dark_mode.css"),#"sidebar.css"),
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
              
              div(style = "text-align: center",bsButton("define_model", "Let's Have Some Fun!", icon("bolt"), size = "large")),
              br(),
              h4(tags$b("Acknowledgements:")),
              h5("This app was developed by George, Prianca, and Daisy. 
                 Special thanks to Jinglin Feng and Alex Chen, who 
                 designed the original skeleton of this app in their 
                 overfitting app at BOAST."),
              
      ),

      
      #Define the content contained within part 1 ie. tabname "result"
      tabItem(tabName = "model",
              fluidRow(
                withMathJax(),
                column(2,
                       br(),
                       br(),
                       br(),
                       br(),
                       conditionalPanel(#
                         condition="input.plot != 0",
                                        
                                        # choose dependent variable from dataset
                                          selectInput(inputId = 'models',
                                                      label = 'Choose a model',
                                                      choices = c('Logistic Regression', 'Random Forest', 'BART')),
                                          
                                        # If log reg is chosen, give option of profit or logit
                                        uiOutput('log_model_option'),
                                        
                                        
                                       # Click fit model button to produce results
                                        actionButton("fit_model", h5(tags$strong("Fit Model"))),
                                        br(),
                                        br(),
                                        
                      )
                ),
                 mainPanel(
                  # If log reg is chosen, give option of polynomial terms or interaction terms
                   div(style = "text-align: center",h3(tags$b("Please choose predictors"))),
                   uiOutput("bucket"),
                  width = 10,
                  ),
                  
                )
              
              
      ),
      
      tabItem(tabName = "result",
              fluidRow(
                column(
                  width = 12,
                  tags$b("Result"),
                  column(
                    width = 12,

                    h3(tags$p("Model Summary")),
                    htmlOutput("model_name") %>% withSpinner(color="#0dc5c1"),
                    tableOutput('model_summary')%>% withSpinner(color="#0dc5c1"),
                    
                    # Click clear button to go back to define model
                    actionButton("clear", h5(tags$strong("Back to Define Model"))),
                    br(),
                    br()



                  )
                )
              )
              
              
      )
      
      
      
    )
    
    
    
    
    
  ))

