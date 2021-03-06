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
      menuItem("Check Overlap + Balance", tabName = "ps_result", icon = icon("wpexplorer")),
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
              h2(tags$b(strong('The Problem With Propensity Score Matching'))),
              h4(tags$li('The main challenge with propensity score matching is due to researcher degrees of freedom.
                         The choice of model inputs allows for a lot of subjectivity in
                         how a propensity score model is defined and specified. This subjectivity has a direct influence on 
                         the results of propensity score matching. Each different model specification can lead to drastically
                         different treatment effect estimates. The question is: how bad can it be?')),
              
              withMathJax(),
              br(),
              h2(strong("What does this app do?")),
              withMathJax(),
              h4(tags$li("This app is to show you how Propensity Score Matching can be misused and produce inaccurate results.")),
              h4(tags$li("In this example, we use the IHDP dataset, which is data resulting from an observational study, to examine how high quality 
              child care could impact children's test scores in elementary school. We have simulated the outcome of test scores, such that the average treatment 
              effect on the treated (ATT) is 0. However, based on the propensity score matching model you choose, you could get many varying results that are not the true ATT.")),
              h4(tags$li("There are 3 main tabs:")),
              h4(tags$li("1. Define Model: define your propensity score model ")),
              h4(tags$li("2. Check the balance and overlap from your specified model and matching method.")),
              h4(tags$li("3. View Results: View the resulting estimated ATT along with the true ATT and past model estimations.")),
              br(),
              
              div(style = "text-align: center",bsButton("define_model", "Let's Have Some Fun!", icon("bolt"), size = "large")),
              br(),
              h4(tags$b("Acknowledgements:")),
              h5("This app was developed by Prianca and Daisy, with guidance from George. 
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
                  #overlap and balance tab
                  h2(tags$b(strong("Check Overlap and Balance from Propensity Score Matching"))),
                  column(
                    width = 12,
                    h3(tags$p("Propensity Score Model")),
                    htmlOutput("model_name"),
                    tags$head(tags$style("#model_name{
                                 font-size: 20px;
                                 }"
                      )
                    ),
                    #show the plots 
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
                    
                    # Click att button to go to att results page 
                    actionButton("see_att", h5(tags$strong("See ATT Result"))),
                    br(),
                    br()
                  )
                )
              )
              
              
      ),
      #att results tab
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
                  #show att plot as well as model table 
                  h3(tags$p("ATT Plot")),
                  plotOutput('att_plot')%>% withSpinner(color="#0dc5c1")),
                column(
                  width = 12,
                  h3(tags$p("Model Table")),
                  tableOutput('modelTable')%>% withSpinner(color="#0dc5c1"),
                  # Click back button to go back to model specification page
                  actionButton("back", h5(tags$strong("Define Another Model"))),
                  br(),
                  br(),
                  #button to clear history of models and start fresh 
                  actionButton("clear", h5(tags$strong("Clear All Models"))),
                  br(),
                  br()
                  
                )

                )
              )


      )
      
      
      
      
      
    )
    
    
    
    
    
  )


