#'@details Store a table that lists the downloadable components
download_table <- reactive({
    # lot of stuff stored here, make it a dependency
    reactiveValuesToList(objects)
  
    if(is.null(objects$omicsData_pre_rollup)){
        resloc_use <- resources_locations
        table_use <- tables$tables_table
    } else {
        resloc_use <- resources_locations_peprollup
        table_use <- tables$revenge_of_tables_table
    }
    
    # catch all for null resources
    for (table_name in table_use$Table){
      resloc <- resloc_use[[table_name]]
      
      if (inherits(resloc, "list")) {
        # use pluck to extract the table from its location within the reactive variables
        resource <- do.call(purrr::pluck, c(list(get(resloc[[1]])), resloc[2:length(resloc)]))
        if (is.null(resource)) {
          table_use <- table_use[table_use$Table != table_name,]
        }
      }
    }
  
    if (is.null(objects$omicsData_2) ||
        isTRUE(attributes(objects$omicsData)[['data_info']][['is_combined']])) {
      table_use = table_use[table_use$Table != "Processed Expression Data (e_data) dataset 2", ]
    }

    # Reset index numbers
    rownames(table_use) = seq(length=nrow(table_use))
    
    table_use
})