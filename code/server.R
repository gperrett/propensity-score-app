library(shiny)



shinyServer(function(input, output) {
  
  # Load dataset
  data <- reactive({
    
    
  })
  
  
  # Choose Model
  observeEvent(input$models, {
    # req(input$data)
    if(input$models == 'Logistics'){}
    updateSelectInput(inputId = 'log_model_option', 
                      choices = c('probit','logit'))
  })
  
  
})

