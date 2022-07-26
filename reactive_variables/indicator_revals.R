#'@details Is the data paired?  Should return F for all states in which 
#'objects$omicsData is not an omicsData object with pairing attributes
omicsData_paired <- reactive({
  if (!isTruthy(objects$omicsData)) {
    return(F)
  } 
  
  pair_id <- objects$omicsData %>% 
    attributes() %>% 
    `[[`("group_DF") %>% 
    attributes() %>% 
    `[[`("pair_id")
  
  return(!is.null(pair_id))
})