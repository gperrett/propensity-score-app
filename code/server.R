library(shiny)
library(randomForest)
library(randomForestExplainer)
library(dplyr)
library(shinycssloaders)
library(dbarts)
library(tidyverse)
library(arm)
library(stringr)
library(ggrepel)

## Helper functions

### Make a list of interaction terms to choose from
make_interx_terms = function(var1, var2){
  # return(paste0(c(var1, var2),collapse = '*'))
  return(paste0(var1, '*', var2))
  }

create_interx_list = function(data){
  varbs = c("bw", "b.head","preterm","birth.o","nnhealth", "momage",
            "sex","twin","b.marr","mom.lths","mom.hs","mom.scoll", "cig",
            "first","booze","drugs","work.dur","prenatal","ark","ein",
            "har","mia","pen","tex","was","momwhite","momblack","momhisp"
            )
  interx_list = crossing(varbs, varbs)
  interx_list = interx_list[which(interx_list[1]!=interx_list[2]),]
  interx_list = mapply(make_interx_terms, interx_list[1],interx_list[2])
  # interx_list = c('None', interx_list)
  return(interx_list)
}

### Make poly terms
make_poly_terms = function(predictors){
  if (length(predictors>0)){return(paste0('I(', predictors,'^2)'))}
}


### Make propensity score model formula
make_model_formula = function(predictors, outcome, interact_terms=NULL, poly_terms=NULL){


  if (!is.null(interact_terms)){
    predictors = c(predictors, interact_terms)
  }
  if (!is.null(poly_terms)){
    poly = make_poly_terms(poly_terms)
    predictors = c(predictors, poly)
  }
  
  
  return(paste0(outcome, '~', paste0(predictors, collapse = '+')))
}

## Fit logistic regression model as propensity score model
fit_lr_model = function(model_formula, link, data){
  model <- glm(as.formula(model_formula), data = data, family = binomial(link = link))
  return(model)
 }

## Fit random forest model as propensity score model
fit_rf_model = function(model_formula, data){
  return(randomForest(as.formula(model_formula), data = data))
}

## Fit BART model as propensity score model
fit_bart_model = function(model_formula, data){
  return(bart2(as.formula(model_formula), data = data, combineChains = TRUE))
}


## Server
shinyServer(function(input, output,session) {

  ## Load dataset
  data <- read.csv('simwoutcome.csv')
  #data = read.csv('~/Desktop/propensity-score-app/data/simwoutcome.csv')
    # read.csv(paste0(getwd(),'/data/simdata.csv'))
  
  #reactive values storage for dataframe with all models
  store <- reactiveValues()
  #placeholder dataframe
  store$df <- data.frame(Number = integer(), Model = character(), Variables = character(),
                         Matching = character(), ATT = numeric())
  #keep track of number of models a user builds
  store$number_of_models <- 0
  
  #update tabs if button is clicked
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
  
  ## Choose matching options
  output$matching_option = renderUI({
    selectInput(inputId = 'matching_option',
                label = 'Choose a matching method: ',
                choices = c('Nearest Neighbor with Replacement', 
                            'Nearest Neighbor without Replacement',
                            'IPTW'))
  })
  
  ## Bucket list: if log reg: give option of using polynomial terms and interaction terms
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
        text = "`Predictor(s)",
        labels = NULL,
        input_id = 'predictors'
      ))}})
  


  ## If log reg: give option of using interaction terms as a dropdown list
  interx_options = reactive({create_interx_list(data())})
  output$interx_option = renderUI({
    if(input$models == 'Logistic Regression'){
      selectInput(inputId = 'interactterm',
                  label = 'Choose interactions',
                  choices =  interx_options(), 
                  multiple = TRUE)

    }else{NULL}})

  # ## Gather all predictors
  predictors=reactive({
    predictors = input$predictors
    if(!is.null(input$interactterm) & input$models == 'Logistic Regression'){
      predictors = c(predictors, unique(unlist(str_split(input$interactterm, pattern  ="\\*"))))
    }
    if(!is.null(input$polyterm) & input$models == 'Logistic Regression'){
      polyterms = str_remove(input$polyterm, "[2]")
      polyterms = str_remove(polyterms, "[\\^]")
      polyterms = str_remove(polyterms, "[I]")
      polyterms = str_remove(polyterms, "[(]")
      polyterms = str_remove(polyterms, "[)]")
      predictors = c(predictors,polyterms)
    }
    predictors = predictors
  })
  
  #make sure at least 1 predictor is selected
  observeEvent(input$fit_model, {
    if(length(predictors()) < 1){
      show_alert(title = 'Predictors Error', 
                 text = 'Please select at least 1 predictor', 
                 type = 'error')
    }
  })
  
  ## Go to result page after fitting model if at least 1 predictor is selected
  observeEvent(input$fit_model, {
    req(length(predictors()) >= 1)
    store$number_of_models <- store$number_of_models+ 1
    updateTabItems(session, "tabs", "ps_result")
  })
  

    
  ## Fit model
  model_formula = eventReactive(input$fit_model, {
    req(length(predictors()) >= 1)
    if(input$models == 'Logistic Regression'){
      make_model_formula(predictors= input$predictors, 
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
  
  

  ## Propensity scores fit
  ps <- reactive({
    if(input$models=='Logistic Regression'){
      ps_model <- fit_lr_model(model_formula(), data = data, 
                               link = input$log_model_option)
      ps <- predict(ps_model, type = 'response')
      }
    else if (input$models == 'Random Forest') {
      ps_model <- fit_rf_model(model_formula(), data = data)
      ps <- predict(ps_model, type = 'response')
    }
    else {
      ##get propensity score from bart
      print(model_formula())
      ps_model <- fit_bart_model(model_formula(),  data = data)
      ps <- fitted(ps_model)
    }
  
    return(ps)
  })
  
  ## Propensity Score Weights from chosen matching method 
  pscore_weights <- eventReactive(input$fit_model, {
    req(length(predictors()) >= 1)
    if(input$matching_option == 'Nearest Neighbor with Replacement') {
      matches <- arm::matching(z = data$treat, score = ps(), replace = TRUE)
      weights <- matches$cnts
    }
    else if (input$matching_option == 'Nearest Neighbor without Replacement'){
      matches <- arm::matching(z = data$treat, score = ps(), replace = FALSE)
      weights <- matches$cnts
    }
    else{
      ##iptw
      weights <- if_else(data$treat == 1, 1, ps()/(1-ps()))
    }
    #save weights
    
    return(weights)
    
  }  )
  
  ## ATT calculation using reactive weights 
  att <- reactive({
    m.formula =  paste0('YC ~', paste0(names(data %>% dplyr::select(-YC)), collapse = '+'))
    lin_model <- lm(as.formula(m.formula), data = data, weights = pscore_weights())
    out_ps <- summary(lin_model)
    att_est <- out_ps$coefficients[2, 1]
    print(att_est)
    return(att_est)
  })
  
  
  ## Print model summary
  output$model_name = renderUI({
    model = paste("Model: ", model_formula())

    if(input$models == 'Logistic Regression'){
      model_type = 'Logistic Regression'
        link_func = paste("Link Function: ",input$log_model_option)
        HTML(paste(model_type,model,  link_func, sep = '<br/>'))
    }
    else if(input$models == 'Random Forest'){
      model_type = 'Random Forest'
      HTML(paste(model_type,model, sep = '<br/>'))
    }
    else if(input$models == 'BART'){
      model_type = 'BART'
      HTML(paste(model_type,model, sep = '<br/>'))
    }

    
  })
  
  #update reactive dataframe with model fit info whenever button is clicked 
  #if at least 1 predictor is selected
  newEntry <- eventReactive(input$fit_model, {
    req(length(predictors()) >= 1)
    model_type = c()
    if(input$models == 'Logistic Regression'){
      model_type = 'Logistic Regression'
      link_func = input$log_model_option
      model_type <- paste(model_type, link_func, sep = ' - ')
    }
    else if(input$models == 'Random Forest'){
      model_type = 'Random Forest'
    }
    else {
      model_type = 'BART'
    }
    #dont include treatment var in list of predictors 
    covars <- str_split(model_formula(), '~')[[1]][2]
    #save line of new model information as data frame and rbind with reactive df
    newLine <- data.frame(as.integer(store$number_of_models, digits = 0), model_type, covars, input$matching_option, format(round(att(),8),nsmall=8))
    names(newLine) <- c("Number", "Model", "Variables", "Matching", "ATT")
    store$df <- rbind(store$df, newLine)
    return(store$df)
  })
  
  #print reactive df 
  output$modelTable = renderTable({
    return(newEntry())
  })

  
  ## Balance Plot
  output$balance_plot <- renderPlot({
    req(predictors(), pscore_weights())
    plot(balance(data %>% dplyr::select(predictors()), data$treat, pscore_weights()))
    # plot(balance(data %>% dplyr::select(input$predictors), data$treat, pscore_weights()))
  })
  
  ## Overlap Plot
  output$overlap_plot <- renderPlot({
    req(ps())
    data$ps <- ps()
    ggplot(data) + geom_histogram(aes(x = ps, color = factor(treat)), fill = 'white',
                                  alpha = 0.3, bins = 20) + scale_color_manual(values=c("blue", "red")) + labs(color = 'Treatment Group')
  })
  
  ## ATT
  output$att_info <- renderText({
    # print(paste0('The ATT is: ', att()))
    if(!is.null(att())){paste0('The ATT is: ', att())}
    
  })
  
  #plot true att (value = 0), as well as all previous atts from user's models 
  output$att_plot <- renderPlot({
    # print(att())
    if (att() < 0) {
      x = seq(att()-5, 5, .5)
    }
    else {
      x = seq(-5, att() + 5, .5)
    }
    ggplot(x = x, y = seq(0, 1, .1)) + 
      geom_vline(aes(xintercept = 0, colour = 'True ATT')) + 
      #plot all saved ATT values including new one
      geom_vline(aes(xintercept = as.numeric(newEntry()$ATT), group = factor(newEntry()$Number), colour = factor(newEntry()$Number)), linetype = 'dashed') + 
      #scale_x_continuous(breaks=seq(0,1,.05), limits = c(-.01, 1.01), expand = expansion()) + 
      #scale_y_continuous(labels = NULL, breaks = NULL) + 
      labs(y = "", x = 'Average Treatment Effect on Treated (ATT)', colour = "Model Number") + 
      theme_classic() + 
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank())
  })
  
  ## Go to define model page after result
  observeEvent(input$see_att, {
    updateTabItems(session, "tabs", "att_result")
  })

  
  ## Go to define model page after result
  observeEvent(input$back, {
    updateTabItems(session, "tabs", "model")
  })

  ## Go to define model page after result and clear history of models from dataframe to start fresh
  observeEvent(input$clear, {
    store$df <- data.frame(Number = integer(), Model = character(), Variables = character(),
                           Matching = character(), ATT = numeric())
    store$number_of_models <- 0
    updateTabItems(session, "tabs", "model")
  })


  
  

  
  
})

