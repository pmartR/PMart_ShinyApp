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
    resloc_use <- resources_locations
  } else {
    resloc_use <- resources_locations_peprollup
  }

  print(tempdir())
  fs <- vector()

  plots_marked_for_death <- which(plots$plot_table[, 2] == dt_checkmark)
  tables_marked_for_death <- which(download_table()[, 2] == dt_checkmark)
  
  total_files <- length(c(plots_marked_for_death, tables_marked_for_death))

  withProgress(message = "Writing files: ", {
    # Kaleido bug workaround
    reticulate::py_run_string("import sys")
    
    scope <- kaleido()
    
    if (length(plots_marked_for_death) > 0) {
      for (i in plots_marked_for_death) {
        plot_name <- plots$plot_table[i, 1]
        
        if(inherits(plots$allplots[[i]], "plotly")){
          save_options <- plots$plot_save_options[[i]]
          switch(
            save_options$type,
            
            "HTML Widget" = {
              fname <- paste0(gsub(":", " ", plot_name), ".html") #create a plot name
              saveWidget(plots$allplots[[i]], file.path(tempdir(), fname),
                         selfcontained = T)
            },
            
            "PNG" = {
              fname <- paste0(gsub(":", " ", plot_name), ".png")
              scope$transform(plots$allplots[[i]], file = gsub("\\\\", "/", file.path(tempdir(), fname)), width = save_options$width, height = save_options$height, scale = save_options$scale)
            },
            
            "JPG" = {
              fname <- paste0(gsub(":", " ", plot_name), ".jpg")
              scope$transform(plots$allplots[[i]], file = gsub("\\\\", "/", file.path(tempdir(), fname)), width = save_options$width, height = save_options$height, scale = save_options$scale)
            },
            
            "SVG" = {
              fname <- paste0(gsub(":", " ", plot_name), ".svg")
              scope$transform(plots$allplots[[i]], file = gsub("\\\\", "/", file.path(tempdir(), fname)), width = save_options$width, height = save_options$height, scale = save_options$scale)
            }
          )
          
        } else {
          fname <- paste0(gsub(":", " ", plot_name), ".png")  # create a plot name
          ggsave(file.path(tempdir(), fname), plot = plots$allplots[[i]])
        }
        fs <- c(fs, fname) 
        incProgress(1 / total_files, detail = sprintf("%s done..", plot_name))
      }
    }
    rm(scope); gc()
    
    # do.call('pluck', c(list(get('omicsData_postmortem')),list('e_data')))
    if (length(tables_marked_for_death) > 0) {
      for (i in tables_marked_for_death) {
        table_name <- download_table()[i, 1]
        resloc <- resloc_use[[table_name]]

        if (inherits(resloc, "list")) {
          # use pluck to extract the table from its location within the reactive variables
          resource <- do.call(purrr::pluck, c(list(get(resloc[[1]])), resloc[2:length(resloc)]))
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
  
  # __SHINYTEST__
  exportTestValues(files_to_export = revals$fs)
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
  
  output$download_image_preview <- renderUI(NULL)
})

observeEvent(input$download_apply_save_options, {
  if(length(input$download_plot_table_rows_selected) < 1){
    return(NULL)
  } 
  
  plot_name <- plots$plot_table[input$download_plot_table_rows_selected, 1]
  plots$plot_save_options[[plot_name]] <- list(type = input$download_file_type, width = input$download_plot_width, height = input$download_plot_height, scale = input$download_plot_scale)

  output$download_image_preview <- renderUI(NULL)  
})

observeEvent(input$download_preview_image, {
  if(length(input$download_plot_table_rows_selected) < 1){
    return(NULL)
  } 
  
  plot_name <- plots$plot_table[input$download_plot_table_rows_selected, 1]
  
  output$download_image_preview <- renderUI("Generating preview...")
  
  # Prevent accidental double-clicking (makes it take a lot longer)
  shinyjs::disable("download_preview_image")
  
  image_format <- input$download_file_type
  image_width <- input$download_plot_width
  image_height <- input$download_plot_height
  image_scale <- input$download_plot_scale
  
  # HTML Widgets don't get previews (they have the default preview)
  if (image_format == "HTML Widget") {
    output$download_image_preview <- renderUI("See Widget preview above.")
    return(NULL)
  }
  
  if (image_format != "PNG" && image_format != "SVG" && image_format != "JPG") {
    output$download_image_preview <- renderUI(paste("Error: unknown output format", image_format))
    return(NULL)
  }
  
  # Kaleido bug workaround
  reticulate::py_run_string("import sys")
  
  scope <- kaleido()
  scope$transform(plots$allplots[[plot_name]], file = "www/preview.png", width = image_width, height = image_height, scale = image_scale)
  rm(scope); gc()
  
  # Attach the current time at the end of the query so the browser doesn't cache the preview image
  output$download_image_preview <- renderUI(img(src=paste0("preview.png?", Sys.time())))

  shinyjs::enable("download_preview_image")
})

#'@details Disable and add a tooltip to the button that updates the download
#'parameters for each plot if the currently selected parameters are the same
#'as the saved ones.
observe({
  req(input$download_plot_table_rows_selected)
  plot_name <- plots$plot_table[input$download_plot_table_rows_selected, 1]
  plot_file_type <- plots$plot_save_options[[plot_name]]$type
  plot_save_width <- plots$plot_save_options[[plot_name]]$width
  plot_save_height <- plots$plot_save_options[[plot_name]]$height
  plot_save_scale <- plots$plot_save_options[[plot_name]]$scale
  
  .cond1 <- isTRUE(plot_file_type == input$download_file_type)
  .cond2 <- isTRUE(plot_save_width == input$download_plot_width)
  .cond3 <- isTRUE(plot_save_height == input$download_plot_height)
  .cond4 <- isTRUE(plot_save_scale == input$download_plot_scale)
  
  params_unchanged <- all(.cond1, .cond2, .cond3, .cond4)
  
  togglestate_add_tooltip(session, "download_apply_save_options_tooltip", condition = !params_unchanged, tooltip_text = ttext_[["DOWNLOAD_OPTS_DISABLED"]])
  toggleCssClass(id = "download_apply_save_options", condition = !params_unchanged, class = "blueoutline")
  
})

observe({toggle("download_plot_options_UI", condition = length(input$download_plot_table_rows_selected) > 0)})

###
