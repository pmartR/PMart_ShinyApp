f_data <- reactive({
  
  # MAP-specific version
  if (MAP) {
    
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

f_data_2 <- reactive({
  # Error handling: Need file_emeta to be valid
  req(input$file_fdata_2$datapath)
  # Load file
  filename <- input$file_fdata_2$datapath
  read.csv(filename, stringsAsFactors = FALSE, check.names = F)
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

main_effects_2 <- reactive({
  main_effects_2 <- c(input$gcol1_2, input$gcol2_2)[which(c(input$gcol1_2, input$gcol2_2) != "None")]
  if (length(main_effects_2) == 0) {
    return(NULL)
  } else {
    return(main_effects_2)
  }
})

covariates_2 <- reactive({
  covariates_2 <- c(input$cvcol1_2, input$cvcol2_2)[which(c(input$cvcol1_2, input$cvcol2_2) != "None")]
  if (length(covariates_2) == 0) {
    return(NULL)
  } else {
    return(covariates_2)
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
  
  if (two_lipids()) {
    cond_selected1 <- all(
        isTRUE(input$pair_id_col %in% colnames(f_data())), 
        isTruthy(input$pair_id_col != "None") 
    )
    cond_selected2 <- all(
        isTRUE(input$pair_id_col_2 %in% colnames(f_data_2())),
        isTruthy(input$pair_id_col_2 != "None") 
    )
    
    cond_selected = any(cond_selected1, cond_selected2)
    
    cond_pairs <- all(
      cond_selected1,
      cond_selected2,
      isTruthy(input$pair_group_col != "None"),
      isTruthy(input$pair_denom_col != "None"),
      isTruthy(input$pair_group_col_2 != "None"),
      isTruthy(input$pair_denom_col_2 != "None")
    )
  }
  else {
    cond_selected <- all(
      isTRUE(input$pair_id_col %in% colnames(f_data())),
      isTruthy(input$pair_id_col != "None")
    )
    
    cond_pairs <- all(
      cond_selected,
      isTruthy(input$pair_group_col != "None"), 
      isTruthy(input$pair_denom_col != "None")
    )
  }
  
  indicators[["selected"]] = cond_selected
  indicators[["valid"]] = cond_pairs
  indicators[["pass"]] = !cond_selected | cond_pairs
  
  return(indicators)
})
