f_data <- reactive({
  
  # MAP-specific version
  if (MAP_ACTIVE) {
    
    ## UPDATED FOR MAP ##
    req(input$file_fdata)
    
    # Disable input widget
    disable(id = "file_fdata")
    
    # Return data 
    return(MapConnect$Project$Data$f_data)
    
  } else {
    
    # Error handling: Need file_emeta to be valid
    req(input$file_fdata$datapath)
    # Load file
    filename <- input$file_fdata$datapath
    read.csv(filename, stringsAsFactors = FALSE, check.names = F)
    
  }

})

main_effects <- reactive({
  main_effects <- c(input$gcol1, input$gcol2)[which(c(input$gcol1, input$gcol2) != "None")]
  if (length(main_effects) == 0) {
    return(NULL)
  } else {
    return(main_effects)
  }
})

covariates <- reactive({
  covariates <- c(input$cvcol1, input$cvcol2)[which(c(input$cvcol1, input$cvcol2) != "None")]
  if (length(covariates) == 0) {
    return(NULL)
  } else {
    return(covariates)
  }
})

#' @details Booleans to determine the state of pairing structure completion
#' 
#' @return List with the following elements:
#' 
#' * selected: boolean indicating whether or not the user has selected id columns for pairing
#' * valid:  boolean indicating if all pairing information has been filled out
#' * pass: combination of selected and valid, specifically, selected == F OR valid == T
pairs_complete <- reactive({
  indicators <- list()
  
  cond_selected <- all(
    isTRUE(input$pair_id_col %in% colnames(f_data())),
    isTruthy(input$pair_id_col != "None")
  )
  
  cond_pairs <- all(
    cond_selected,
    isTruthy(input$pair_group_col != "None"), 
    isTruthy(input$pair_denom_col != "None")
  )
  
  indicators[["selected"]] = cond_selected
  indicators[["valid"]] = cond_pairs
  indicators[["pass"]] = !cond_selected | cond_pairs
  
  return(indicators)
})
