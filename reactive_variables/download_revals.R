#'@details Store a table that lists the downloadable components
download_table <- reactive({
    if(is.null(objects$omicsData_pre_rollup)){
        table_use <- tables$tables_table
    } else {
        table_use <- tables$revenge_of_tables_table
    }
    
    if (is.null(objects$omicsData$e_meta)) {
        table_use = table_use[table_use$Table != "Biomolecule Information (e_meta)",]
    }
    
    if (is.null(objects$imdanova_res)) {
        table_use = table_use[table_use$Table != "Statistics",]
    }
    
    if (is.null(objects$seqstats_res)) {
        table_use = table_use[table_use$Table != "SeqData Statistics",]
    }
    
    if (is.null(objects$omicsData_2) ||
        isTRUE(attributes(objects$omicsData)[['data_info']][['is_combined']])) {
      table_use = table_use[table_use$Table != "Processed Expression Data (e_data) dataset 2", ]
    }

    # Reset index numbers
    rownames(table_use) = seq(length=nrow(table_use))
    
    table_use
})