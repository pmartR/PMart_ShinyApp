#'@details Holds the display dataframe of possible group comparisons
comp_df_holder <- reactiveValues(comp_df = NULL)

#'@details store the 
get_swap_vals <- reactive({
  values <- names(input)[str_detect(names(input), "imd_comparison_button_")]
  
  res <- map_int(values, function(val) {
    input[[val]]
  })
  
  names(res) <- gsub("imd_comparison_button_", "", values)
  
  return(res)
})
