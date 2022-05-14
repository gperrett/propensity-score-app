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
      menuItem("PS Result", tabName = "ps_result", icon = icon("wpexplorer")),
      menuItem("ATT Result", tabName = "att_result", icon = icon("wpexplorer"))
      
    )),
  
  #Content within the tabs
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href ="dark_mode.css"),#"sidebar.css"),
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #ffb6c1}")),
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #ffb6c1}")),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #ffb6c1}")),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #ffb6c1}"))
     ),
    
    tabItems(
      tabItem(tabName = "intro",
              h2(tags$b(strong("What is Propensity Score Matching? "))),
              h4(tags$li("In Casual Inference, Propensity Score Matching is 
              a statistical matching technique that attempts to estimate the 
              effect of a treatment by accounting for the covariates that 
                         predict the reception of the treatment. ")),
              h2(tags$b(strong('Problem in Propensity Score Matching'))),
              h4(tags$li('One important factor that determine the success of propensity
                         score matching is to use the correct propensity score matching
                         model. However, when each propensity score model produce 
                         a different result and we have not known the true effect of 
                         the drug yet, it is difficult to say which model is the correct one. ')),
              
              withMathJax(),
              br(),
              h2(strong("What does this app do?")),
              withMathJax(),
              h4(tags$li("This app is to show you how Propensity Score Matching could be misused and produce inaccurate result. ")),
              h4(tags$li("In this example, we are using stimulated data to understand how high quality child care could affect acadeic outcome.
                         In our example, the average treatment effect on the treated (ATT) is 0. However, based on the propensity score matching model you choose, 
                         you could get different result than the true ATT. ")),
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
                                          
                                        # If log reg is chosen, give link function options and interaction term option
                                        # Give matching option for all
                                        uiOutput('log_model_option'),
                                        uiOutput('matching_option'),
                                        uiOutput('interx_option'),
                                        
                                       # Click fit model button to produce results
                                        actionButton("fit_model", h5(tags$strong("Fit Model"))),
                                        br(),
                                        br()
                                        
                      )
                ),
                 mainPanel(
                  # If log reg is chosen, give option of polynomial terms or interaction terms
                   div(style = "text-align: center",h3(tags$b("Please choose predictors"))),
                   uiOutput("bucket"),

                  width = 10
                  ),
                  
                )
              
              
      ),
      
      tabItem(tabName = "ps_result",
              fluidRow(
                column(
                  width = 12,
                  h2(tags$b(strong("Propensity Score Matching Result"))),
                  column(
                    width = 12,
                    h3(tags$p("Propensity Score Model")),
                    htmlOutput("model_name"),
                    tags$head(tags$style("#model_name{
                                 font-size: 20px;
                                 }"
                      )
                    ),
                    
                    br(),
                    h3(tags$p("Balance Plot")),
                    plotOutput('balance_plot')%>% withSpinner(color="#0dc5c1")),
                  column(
                    width = 12,
                    h3(tags$p("Overlap Plot")),
                    plotOutput('overlap_plot')%>% withSpinner(color="#0dc5c1"),

                    br(),
                    br() ),
                  column(
                    width = 12,
                    
                    # Click clear button to go back to define model
                    actionButton("see_att", h5(tags$strong("See ATT Result"))),
                    br(),
                    br()
                  )
                )
              )
              
              
      ),
      tabItem(tabName = "att_result",
              fluidRow(
                column(
                  width = 12,
                  h2(tags$b(strong("ATT Result"))),
                  textOutput('att_info'),
                  tags$head(tags$style("#att_info{
                               font-size: 20px;
                               }"
                  )),

                  h3(tags$p("ATT Plot")),
                  plotOutput('att_plot')%>% withSpinner(color="#0dc5c1"),
                  
                  # Click clear button to go back to define model
                  actionButton("clear", h5(tags$strong("Back to Define Model"))),
                  br(),
                  br()
                )

                )
              )


      )
      
      
      
      
      
    )
    
    
    
    
    
  )


