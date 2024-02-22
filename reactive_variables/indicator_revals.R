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

#'@details flag to indicate at least one object has been filtered
obj_filtered <- reactive({
  req(objects$omicsData)

  return(length(attr(objects$omicsData, "filters")) != 0)
})

#'@details Number of groups in the data
num_groups <- reactive({
  req(objects$omicsData)

  obj <- objects$omicsData

  group_df <- obj %>% attr("group_DF")
  if (is.null(group_df)) {
    return(0)
  }
  group_df %>%
    purrr::pluck("Group") %>%
    unique() %>%
    length()
})