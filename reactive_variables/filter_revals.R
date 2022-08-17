#'@details gets which e_meta identifiers will be removed based on what the user
#'selected, possibly in a column besides e_meta_cname for the first object...
e_meta_remove_rv <- reactive({
  cond <- all(sapply(list(
    objects$uploaded_omicsData$e_meta,
    input$emeta_customfilt_which_col_1,
    input$emeta_customfilt_which_values_1
  ), isTruthy))
  
  if(cond){
    objects$uploaded_omicsData$e_meta %>%
      dplyr::filter(!!rlang::sym(input$emeta_customfilt_which_col_1) %in% input$emeta_customfilt_which_values_1) %>%
      purrr::pluck(get_emeta_cname(objects$uploaded_omicsData)) 
  } else NULL
})
#'...and the second object
e_meta_remove_rv_2 <- reactive({
  cond <- all(sapply(list(
    objects$uploaded_omicsData_2$e_meta,
    input$emeta_customfilt_which_col_2,
    input$emeta_customfilt_which_values_2
  ), isTruthy))
  
  if(cond) {
    objects$uploaded_omicsData_2$e_meta %>%
      dplyr::filter(!!rlang::sym(input$emeta_customfilt_which_col_2) %in% input$emeta_customfilt_which_values_2) %>%
      purrr::pluck(get_emeta_cname(objects$uploaded_omicsData_2)) 
  } else NULL

})

