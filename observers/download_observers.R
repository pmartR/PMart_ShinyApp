### Plots

# remove or add a plot from the download queue
observeEvent(input$mark_plot_download, {
  req(length(input$download_plot_table_rows_selected) > 0)
  cond <- plots$plot_table[input$download_plot_table_rows_selected, 2] == dt_minus

  if (cond) {
    plots$plot_table[input$download_plot_table_rows_selected, 2] <- dt_checkmark
  }
  else {
    plots$plot_table[input$download_plot_table_rows_selected, 2] <- dt_minus
  }
})

# remove the selected plot on button click
# need to remove the entry plots$plot_table and the corresponding plot in plots$allplots
observeEvent(input$remove_plot_download, {
  req(length(input$download_plot_table_rows_selected) > 0)
  plot_name <- plots$plot_table[input$download_plot_table_rows_selected, 1]

  plots$plot_table <- plots$plot_table %>% filter(`Select a plot` != plot_name)
  plots$allplots[[plot_name]] <- NULL
})
###

### TABLES

# remove or add a plot from the download queue
observeEvent(input$mark_table_download, {
  req(length(input$download_tables_table_rows_selected) > 0)
  
  if(is.null(objects$omicsData_pre_rollup)){
    table_use <- tables$tables_table
  } else {
    table_use <- tables$revenge_of_tables_table
  }
  
  cond <- table_use[input$download_tables_table_rows_selected, 2] == dt_minus

  if (cond) {
    table_use[input$download_tables_table_rows_selected, 2] <- dt_checkmark
  }
  else {
    table_use[input$download_tables_table_rows_selected, 2] <- dt_minus
  }
  
  if(is.null(objects$omicsData_pre_rollup)){
    tables$tables_table <- table_use
  } else {
    tables$revenge_of_tables_table <- table_use
  }
  
})
###

# observer which write files to temp directory in preparation for download
# this needs to be outside the downloadhandler to get around the server timing out on long downloads
observeEvent(input$makezipfile, {
  disable("makezipfile")
  on.exit({
    enable("makezipfile")
  })
  
  if(is.null(objects$omicsData_pre_rollup)){
    table_use <- tables$tables_table
    resloc_use <- resources_locations
  } else {
    table_use <- tables$revenge_of_tables_table
    resloc_use <- resources_locations_peprollup
  }

  print(tempdir())
  fs <- vector()

  plots_marked_for_death <- which(plots$plot_table[, 2] == dt_checkmark)
  tables_marked_for_death <- which(table_use[, 2] == dt_checkmark)
  
  total_files <- length(c(plots_marked_for_death, tables_marked_for_death))

  withProgress(message = "Writing files: ", {
    if (length(plots_marked_for_death) > 0) {
      for (i in plots_marked_for_death) {
        plot_name <- plots$plot_table[i, 1]
        
        if(inherits(plots$allplots[[i]], "plotly")){
          fname <- paste0(gsub(":", " ", plot_name), ".html") #create a plot name
          saveWidget(plots$allplots[[i]], file.path(tempdir(), fname),
                     selfcontained = T)
        } else {
          fname <- paste0(plot_name, ".png")  # create a plot name
          ggsave(file.path(tempdir(), fname), plot = plots$allplots[[i]])
        }
        fs <- c(fs, fname) 
        incProgress(1 / total_files, detail = sprintf("%s done..", plot_name))
      }
    }
    # do.call('pluck', c(list(get('omicsData_postmortem')),list('e_data')))
    if (length(tables_marked_for_death) > 0) {
      for (i in tables_marked_for_death) {
        table_name <- table_use[i, 1]
        resloc <- resloc_use[[table_name]]

        if (inherits(resloc, "list")) {
          # use pluck to extract the table from its location within the reactive variables
          resource <- do.call(pluck, c(list(get(resloc[[1]])), resloc[2:length(resloc)]))
          if (is.null(resource)) next()

          fname <- paste0(table_name, ".csv") # create a table name
          fs <- c(fs, fname) # append filename

          write_csv(resource, file.path(tempdir(), fname))
          incProgress(1 / total_files, detail = sprintf("%s done..", table_name))
        }
      }
    }
    print(fs)
    revals$fs <- fs
  })
})

# check that the files have been written to the location that zip() will try to read from
observeEvent(revals$fs,
  {
    if (length(revals$fs) > 0) {
      download_condition <- sum(file.exists(file.path(tempdir(), revals$fs))) > 0
    }
    else {
      download_condition <- FALSE
    }

    toggleState("download_processed_data", condition = download_condition)
  },
  ignoreNULL = FALSE
)

observeEvent(input$download_apply_style_plot_1, {
  if(length(input$download_plot_table_rows_selected) < 1){
    return(NULL)
  } 
  
  plot_name <- plots$plot_table[input$download_plot_table_rows_selected, 1]
  plots$allplots[[plot_name]] <- add_plot_styling(input, "download", plots$allplots[[plot_name]])
})

###
