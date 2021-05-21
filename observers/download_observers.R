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
  cond <- tables$tables_table[input$download_tables_table_rows_selected, 2] == dt_minus

  if (cond) {
    tables$tables_table[input$download_tables_table_rows_selected, 2] <- dt_checkmark
  }
  else {
    tables$tables_table[input$download_tables_table_rows_selected, 2] <- dt_minus
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

  print(tempdir())
  fs <- vector()

  plots_marked_for_death <- which(plots$plot_table[, 2] == dt_checkmark)
  tables_marked_for_death <- which(tables$tables_table[, 2] == dt_checkmark)

  total_files <- length(c(plots_marked_for_death, tables_marked_for_death))

  withProgress(message = "Writing files: ", {
    if (length(plots_marked_for_death) > 0) {
      for (i in plots_marked_for_death) {
        plot_name <- plots$plot_table[i, 1]
        fname <- paste0(plot_name, ".png") # create a plot name
        fs <- c(fs, fname) # create running list of names, we will navigate to the temp directory in the download handler
        ggsave(file.path(tempdir(), fname), plot = plots$allplots[[i]])
        incProgress(1 / total_files, detail = sprintf("%s done..", plot_name))
      }
    }
    # do.call('pluck', c(list(get('omicsData_postmortem')),list('e_data')))
    if (length(tables_marked_for_death) > 0) {
      for (i in tables_marked_for_death) {
        table_name <- tables$tables_table[i, 1]
        resloc <- resources_locations[[table_name]]

        if (inherits(resloc, "list")) {
          # use pluck to extract the table from its location within the reactive variables
          resource <- do.call(pluck, c(list(get(resloc[[1]])), resloc[2:length(resloc)]))
          if (is.null(resource)) next()

          fname <- paste0(table_name, ".csv") # create a table name
          fs <- c(fs, fname) # append filename

          write_csv(resource, file.path(tempdir(), fname))
          incProgress(1 / total_files, detail = sprintf("%s done..", table_name))
        }
        else if (table_name == "Trelliscope Displays" & file.exists(file.path("www", resloc))) {
          fs <- c(fs, resources_locations[[table_name]])
          file.copy(file.path("www", resloc), tempdir(), recursive = TRUE)

          # config_info in the html header is set for being rendered within shiny, need to edit it if we want the downloadable version to work...
          # ...specifically '/<trelliscope_name>/appfiles/config.jsonp' -> 'appfiles/config.jsonp'.  We are directly editing the html file.
          index_path <- file.path(tempdir(), paste0("Trelliscope_", session$token), "index.html")
          temp_index <- readLines(index_path)
          # leading slashes mess things up, so included [/|\\]* regex, please change if there is a better way to do this substitution
          temp_index <- gsub(file.path(paste0("[/|\\]*Trelliscope_", session$token), "appfiles"), "appfiles", temp_index)

          fileConn <- file(index_path)
          writeLines(temp_index, fileConn)
          close(fileConn)

          # rewrite index file here
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

###
