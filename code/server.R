library(shiny)



shinyServer(function(input, output) {
  
  # Load dataset
  data <- reactive({
    
    
  })
  
  ## Choose 
  output$log_model_option = renderUI({
    
    # get variable type
    # type = datatype()[input$variable]
    # colorvar_choices = names(which(datatype()=='discrete'))
    # if the type is continuous, add color variables for histogram
    if(input$models == 'Logistic Regression'){
      selectInput(inputId = 'log_model_option',
                  label = 'choose a type of logistics regression',
                  choices =  c('probit','logit'))
      
    }else{NULL}})
  
  
  # # Choose Model
  # observeEvent(input$models, {
  #   # req(input$data)
  #   if(input$models == 'Logistics Regression'){}
  #   selectInput(inputId = 'log_model_option', 
  #               label = 'choose a type of logistics regression',
  #               choices = c('probit','logit'))
  # })
  
  
})

