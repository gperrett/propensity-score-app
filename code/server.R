library(shiny)
library(randomForest)
library(randomForestExplainer)
library(dplyr)
library(shinycssloaders)
library(dbarts)
library(tidyverse)
library(arm)

## Helper functions

make_poly_terms = function(predictors){
  if (length(predictors>0)){return(paste0('I(', predictors,'^2)'))}
}

make_interx_terms = function(predictors){
  if (length(predictors > 0)){
    if ((length(predictors) < 2) ){
      stop('Error: need at least 2 predictors for interactive terms')
    }else(return(paste0(predictors,collapse = '*')))
  }
}

make_model_formula = function(predictors, outcome, interact_terms=NULL, poly_terms=NULL){

  if (!is.null(interact_terms)){
    interact = make_interx_terms(interact_terms)
    predictors = c(predictors, interact)
  }
  if (!is.null(poly_terms)){
    poly = make_poly_terms(poly_terms)
    predictors = c(predictors, poly)
  }
  return(paste0(outcome, '~', paste0(predictors, collapse = '+')))
}

fit_lr_model = function(model_formula, link, data){
  model <- glm(as.formula(model_formula), data = data, family = binomial(link = link))
  #return(summary(glm(as.formula(model_formula), data = data, family = binomial(link = link))))
  return(model)
 }

fit_rf_model = function(model_formula, data){
  
  return(measure_importance(randomForest(as.formula(model_formula), data = data)))
  
}

fit_bart_model = function(model_formula, data){
  return(summary(bart2(as.formula(model_formula), data = data)))
}


## Server
shinyServer(function(input, output,session) {

  ## Load dataset
  data <- read.csv('~/Documents/propensity-score-app/data/simdata.csv')
    # read.csv(paste0(getwd(),'/data/simdata.csv'))
    

  
  observeEvent(input$define_model, {
    updateTabItems(session, "tabs", "model")
  })
  
  ## Logistic Regression: choose link function
  output$log_model_option = renderUI({
    if(input$models == 'Logistic Regression'){
      selectInput(inputId = 'log_model_option',
                  label = 'Choose a link function',
                  choices =  c('logit','probit'))
      
    }else{NULL}})
  
  output$matching_option = renderUI({
    selectInput(inputId = 'matching_option',
                label = 'Choose a matching method: ',
                choices = c('Nearest Neighbor with Replacement', 
                            'Nearest Neighbor without Replacement',
                            'IPTW'))
  })
  
  ## If log reg: give option of using polynomial terms and interaction terms
  output$bucket = renderUI({
    if(input$models == 'Logistic Regression'){
      bucket_list(
        header = NULL,
        add_rank_list(
          text = "Variables",
          labels = c("bw", "b.head","preterm","birth.o","nnhealth", "momage",
                     "sex","twin","b.marr","mom.lths","mom.hs","mom.scoll", "cig",
                     "first","booze","drugs","work.dur","prenatal","ark","ein",
                     "har","mia","pen","tex","was","momwhite","momblack","momhisp"),
          input_id = 'variables'
        ),
        add_rank_list(
          text = "Predictor(s)",
          labels = NULL,
          input_id = 'predictors'
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
      
    }else{bucket_list(
      header = NULL,
      add_rank_list(
        text = "Variables",
        labels = c("bw", "b.head","preterm","birth.o","nnhealth", "momage",
                   "sex","twin","b.marr","mom.lths","mom.hs","mom.scoll", "cig",
                   "first","booze","drugs","work.dur","prenatal","ark","ein",
                   "har","mia","pen","tex","was","momwhite","momblack","momhisp"),
        input_id = 'variables'
      ),
      add_rank_list(
        text = "Predictor(s)",
        labels = NULL,
        input_id = 'predictors'
      ))}})
  
  
  ## Go to result page after fitting model
  observeEvent(input$fit_model, {
    updateTabItems(session, "tabs", "result")
  })
  
  
  ## Fit model
  model_formula = eventReactive(input$fit_model, {
    if(input$models == 'Logistic Regression'){
      make_model_formula(predictors=  input$predictors, 
                         interact_terms = input$interactterm, 
                         poly_terms = input$polyterm, outcome = 'factor(treat)')
    }else if(input$models == 'Random Forest'){
      make_model_formula(predictors=  input$predictors, 
                         interact_terms = NULL, 
                         poly_terms = NULL, outcome = 'treat')
    }else if(input$models == 'BART'){
      make_model_formula(predictors=  input$predictors, 
                         interact_terms = NULL, 
                         poly_terms = NULL, outcome = 'treat')
    }

  })

  
  ps <- reactive({
    if(input$models=='Logistic Regression'){
      ps_model <- fit_lr_model(model_formula(), data = data, 
                               link = input$log_model_option)}
    
    ps <- predict(ps_model, type = 'response')
    return(ps)
  })
  
  
  pscore_weights = eventReactive(input$fit_model, {
    
    if(input$matching_option == 'Nearest Neighbor with Replacement') {
      matches <- arm::matching(z = data$treat, score = ps(), replace = TRUE)
    }
    else {
      matches <- arm::matching(z = data$treat, score = ps(), replace = FALSE)
    }
    #save weights
    matched <- matches$cnts
    
    return(matched)
    
  }  )
  
  output$att <- reactive({
    lin_model <- lm(obsy_A ~ Z + X1 + X2 + X3 + X4 + X5, data = obsA, weights = matched_Aprob)
    out_psA <- summary(lm1)
    out_psA
    A_ps_est <- out_psA$coefficients[2, 1]
    A_ps_se <- out_psA$coefficients[2, 2]
  })
  
  ## Get model summary
  model_summary = eventReactive(input$fit_model, 
                                { 
                                  
                                  ## Fit models
                                  if(input$models=='Logistic Regression'){
                                     fit_lr_model(model_formula(), data = data(), 
                                                  link = input$log_model_option)}
                                  else if(input$models == 'Random Forest'){
                                    fit_rf_model(model_formula(), data = data)}
                                  else if(input$models == 'BART'){
                                    fit_bart_model(model_formula(), data = data)}
  })
  

  
  ## Print model summary
  output$model_name = renderUI({
    model = paste("Model: ", model_formula())

    if(input$models == 'Logistic Regression'){
      model_type = 'Logistic Regression'
        link_func = paste("Link Function: ",model_summary()$family[[2]])
        aic = paste("AIC:", model_summary()$aic)
        HTML(paste(model, model_type, link_func, aic,sep = '<br/>'))
    }
    else if(input$models == 'Random Forest'){
      model_type = 'Random Forest'
      HTML(paste(model, model_type,sep = '<br/>'))
    }
    else if(input$models == 'BART'){
      model_type = 'BART'
      HTML(paste(model, model_type,sep = '<br/>'))
    }

    
  })
  
  output$model_summary =
    renderTable({
      if(input$models=='Logistic Regression'){model_summary()$coefficients}
      else if(input$models=='Random Forest'){model_summary()}
      else if(input$models == 'BART'){model_summary()}}
      , rownames = TRUE)
  
  output$balance_plot <- renderPlot( {
    plot(balance(data %>% dplyr::select(input$predictors), data$treat, pscore_weights()))
  })
  output$overlap_plot <- renderPlot( {
    data$ps <- ps()
    ggplot(data) + geom_histogram(aes(x = ps, color = factor(treat)), fill = 'white',
                                  alpha = 0.3, bins = 20) + scale_color_manual(values=c("blue", "red"))
  })
  

  
  ## Go to define model page after result
  observeEvent(input$clear, {
    updateTabItems(session, "tabs", "model")
  })




  
  

  
  
})

