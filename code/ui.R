
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
  
  
 

    ),
  mainPanel(
    plotOutput("histogram")
  )
)))


