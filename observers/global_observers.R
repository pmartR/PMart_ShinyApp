#
observeEvent(input$top_page,
  {
    toggleElement(
      "js_saveplot", 
      condition = input$top_page %in% c(
        "upload_data_tab", 
        "group_samples_tab", 
        "data_summary_tab", 
        "filter_tab", 
        "normalization_tab", 
        "peptide_statistics_tab",
        "protein_rollup_tab", 
        "statistics_tab")
      )
  },
  priority = 10,
  ignoreInit = FALSE
)

# store the current plot in a list of all plots
observeEvent(input$saveplot, {
  req(!is.null(plots$last_plot))

  # store the plot in a named element of the plots$allplots reactive list
  # store the name of that element in a table as well
  plot_name <- sprintf("Plot %s:%s", input$saveplot, input$top_page)
  plots$allplots[[plot_name]] <- plots$last_plot
  plots$plot_save_options[[plot_name]] <- list(type = "HTML Widget", width = 800, height = 400, scale = 1)
  plots$plot_table[nrow(plots$plot_table) + 1, ] <- c(plot_name, dt_checkmark)
  
  exportTestValues(
    cur_plot = plots$allplots[[plot_name]]
  )
  
  if (!is.null(plots$last_plot_2)) {
    plot_name_2 <- sprintf("Plot %s, object 2:%s", input$saveplot, input$top_page)
    plots$allplots[[plot_name_2]] <- plots$last_plot_2
    plots$plot_save_options[[plot_name_2]] <- list(type = "HTML Widget", width = 800, height = 400, scale = 1)
    plots$plot_table[nrow(plots$plot_table) + 1, ] <- c(plot_name_2, dt_checkmark)
    
    exportTestValues(
      cur_plot_2 = plots$allplots[[plot_name_2]]
    )
  }

  # prevent saving the same plot twice
  plots$last_plot <- plots$last_plot_2 <- NULL

  # wooooo css
  addCssClass("viewplots", "pulse_bow")
  Sys.sleep(0.6)
  removeCssClass("viewplots", "pulse_bow")
})

# disable saveplot button if there are no plots
observe({
  plot_exists <- !all(is.null(plots$last_plot), is.null(plots$last_plot_2))
  toggleState("saveplot", condition = plot_exists)
  toggleCssClass("saveplot", "fade_disabled", condition = !plot_exists)
})

# modal on button click.  contains plot table and display for selected plot
observeEvent(input$viewplots, {
  showModal(
    modalDialog(
      tagList(
        withSpinner(DTOutput("modal_plot_table")),
        uiOutput("modal_plot_UI")
      ),
      footer = tagList(
        # div(disabled(actionButton(inputId = "add_plot", width = '100%', label = "Save Current Plot for Later Download", icon = icon("save"))))
        div(
          style = "float:left",
          bsButton("mark_plot", "Select/de-select for download", icon = icon("minus")),
          bsButton("remove_plot", "Remove selected plot", icon = icon("remove"))
          # bsButton('download_plots', 'Download selected plots')
        ),
        modalButton("Dismiss")
      ),
      size = "l"
    )
  )
})

# update button text for adding/removing from download queue
observeEvent(c(input$modal_plot_table_rows_selected, input$download_plot_table_rows_selected), {
  cond <- plots$plot_table[input$modal_plot_table_rows_selected, 2] == dt_minus
  cond_download <- plots$plot_table[input$download_plot_table_rows_selected, 2] == dt_minus

  if (isTRUE(cond)) {
    updateButton(session, "mark_plot", icon = icon("plus"))
  }
  else {
    updateButton(session, "mark_plot", icon = icon("minus"))
  }

  if (isTRUE(cond_download)) {
    updateButton(session, "mark_plot_download", icon = icon("plus"))
  }
  else {
    updateButton(session, "mark_plot_download", icon = icon("minus"))
  }
})

# remove or add a plot from the download queue
observeEvent(input$mark_plot, {
  req(length(input$modal_plot_table_rows_selected) > 0)
  cond <- plots$plot_table[input$modal_plot_table_rows_selected, 2] == dt_minus

  if (cond) {
    plots$plot_table[input$modal_plot_table_rows_selected, 2] <- dt_checkmark
  }
  else {
    plots$plot_table[input$modal_plot_table_rows_selected, 2] <- dt_minus
  }
})

# remove the selected plot on button click
# need to remove the entry plots$plot_table and the corresponding plot in plots$allplots
observeEvent(input$remove_plot, {
  req(length(input$modal_plot_table_rows_selected) > 0)
  plot_name <- plots$plot_table[input$modal_plot_table_rows_selected, 1]

  plots$plot_table <- plots$plot_table %>% filter(`Select a plot` != plot_name)
  plots$allplots[[plot_name]] <- NULL
  plots$plot_save_options[[plot_name]] <- NULL
})

# make the logo go to the welcome page
observe({
  shinyjs::onclick("pmart-logo", updateTabsetPanel(inputId = "top_page", selected="intro_panel"))
})

#'@details Triggers a modal that displays a markdown with info
#'on how to use the current page.
observeEvent(input$how_use_page, {
  req(input$top_page)

  showModal(
    modalDialog(
      includeMarkdown(sprintf("www/help_modals/%s.md", input$top_page)),
      size = "l", 
      easyClose = TRUE
    )
  )
})