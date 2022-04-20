
library(shiny)


shinyUI(fluidPage(

  titlePanel("Propensity Score App"),

  # Load a local dataset
  sidebarLayout(
    sidebarPanel(
  
  # choose dependent variable from dataset
  selectInput(inputId = 'models',
              label = 'choose a model',
              choices = c('Logistic Regression', 'Random Forest', 'BART')),
  # If log reg is chosen, give option of profit or logit
  uiOutput('log_model_option'),
  # selectInput(inputId = 'log_model_option',
  #             label = 'choose a logistic regression model',
  #             choices = NULL,)
  # 
  # 
    ),
  mainPanel(
    "main panel"
  )
)))


