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
              h2(tags$b(strong("What is Propensity Score Matching? "))),
              h4(tags$li("In Casual Inference, Propensity Score Matching is 
              a statistical matching technique that attempts to estimate the 
              effect of a treatment by accounting for the covariates that 
                         predict the reception of the treatment. ")),
              br(),
              h2(strong("Why does researchers use Propensity Score Matching? ")),
              h4(tags$li("Imagine you as a scientist who wants to understand
                         the effect of a new diabete drug. You conduct an experiment
                         in which you have patients in the Control group who take 
                         a placebo pill, and patients in the Treatment group who
                         take the new drug. You then measure the blood sugar of the 
                         partients to see if the new drug lower the blood sugar level
                         in the treatment group patients. ")),
              h3(strong("In an ideal world, Random Assignment is Gold...")),
              h4(tags$li("In an ideal world, you would be able to randomly assign 
                         the patients to the control group and the treatment group.
                         In this case, you could avoid selection bias. By randomly 
                         assigning patientsto the Control and Treatment group, 
                         you could also obtain an unbiased average treatment effect. 
                         ")),
              h3(strong("However, in reality...")),
              h4(tags$li("In reality, you are not so lucky as you have to 
                         comply with different research rules.  Therefore, you have to ask 
                         patients to volunteerly join either the treatment group
                         or the control group, but no one can join both at the same time. 
                         Now you have different numbers of patients with different
                         characteristics in the control group and the treatment group.")),
              h4(tags$li("In addition, you want to have a balance dataset in order to 
                          eliminate bias in your estimates. ")),
              h2(strong("Propensity Score Matching As a Tool")),
              h4(tags$li("One way to overcome the lack of random assignment is 
                          using Propensity Score Matching. Using Propensity Score Matching, we could 
                         create a model that could produce a score for each patients
                         ")),
              h4(tags$li('A propensity score is the probability of assigning treatment 
                         to a participant conditional on observed baseline characteristics. ')),
              h4(tags$li("Using Propensity Score Matching, we could create a 
                          model that could produce a propensity score for each patients. 
                          We then could match a patient in the Control group with a patient 
                          with a similar propensity score but in the Treatment group together. 
                          In this case, we could theoretically create a balanced dataset that 
                          is closed to a observation study with random assignment method. 
                         ")),
              h2(tags$b(strong('"Propensity Score Matching sounds cool. Why not use it for every study? "'))),
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
      
      tabItem(tabName = "result",
              fluidRow(
                column(
                  width = 12,
                  tags$b("Result"),
                  column(
                    width = 12,
                    h3(tags$p("Propensity Score Model")),
                    htmlOutput("model_name"),
                    
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
                    textOutput('att_info'),
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
    
    
    
    
    
  ))

