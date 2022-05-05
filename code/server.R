library(shiny)



shinyServer(function(input, output) {
  
  # Load dataset
  data <- reactive({
    
    
  })
  
  ## Logistic Regression
  output$log_model_option = renderUI({
    if(input$models == 'Logistic Regression'){
      selectInput(inputId = 'log_model_option',
                  label = 'choose a type of logistics regression',
                  choices =  c('probit','logit'))
      
    }else{NULL}})
  
  output$bucket = renderUI({
    if(input$models == 'Logistic Regression'){
      bucket_list(
        header = "Please choose which variable for polynomial terms and interaction terms",
        add_rank_list(
          text = "Variables",
          labels = c("treat","bw", "b.head","preterm","birth.o","nnhealth", "momage",
                     "sex","twin","b.marr","mom.lths","mom.hs","mom.scoll", "cig",
                     "first","booze","drugs","work.dur","prenatal","ark","ein",
                     "har","mia","pen","tex","was","momwhite","momblack","momhisp"),
          input_id = 'variables'
        ),
        add_rank_list(
          text = "Polynomial Term(s)",
          labels = NULL,
          input_id = 'polyterm'
        ),
        add_rank_list(
          text = "Interaction Term(s)",
          labels = NULL,
          input_id = 'interactterm'
        )
      )
      
    }else{NULL}})
  
  

  
  
})

