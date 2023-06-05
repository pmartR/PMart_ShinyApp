list(
  
  output$download_plot_UI <- renderUI({

    if (is.null(input$download_plot_table_rows_selected) ||
        input$download_plot_table_rows_selected <= 0) {
      return(HTML(messageBox("Select a plot to preview/edit")))
    }
    
    plot_name <- plots$plot_table[input$download_plot_table_rows_selected, 1]
    p <- plots$allplots[[plot_name]]
    
    output$download_image_preview <- renderUI(NULL)
    
    if(inherits(p, c("ggplot", "gtable"))){
      return(withSpinner(plotOutput("download_plot")))
    } else {
      return(withSpinner(plotlyOutput("download_plotly")))
    }
  }),
  
  # render a plot depending on which row in the plot table is selected
  output$download_plot <- renderPlot({
    req(input$download_plot_table_rows_selected)
    plot_name <- plots$plot_table[input$download_plot_table_rows_selected, 1]
    p <- plots$allplots[[plot_name]]

    if (inherits(p, "gtable")) {
      grid::grid.draw(p)
    }
    else {
      return(p)
    }
  }),
  
  output$download_plotly <- renderPlotly({
    req(input$download_plot_table_rows_selected)
    plot_name <- plots$plot_table[input$download_plot_table_rows_selected, 1]
    p <- plots$allplots[[plot_name]]
    
      return(p)
  }),

  # render the plot table reactive value
  output$download_plot_table <- renderDT(plots$plot_table, selection = "single", escape = FALSE),

  output$download_tables_table <- renderDT(
    {
      
      if(is.null(objects$omicsData_pre_rollup)){
        table_use <- tables$tables_table
      } else {
        table_use <- tables$revenge_of_tables_table
      }
      
      if (is.null(objects$omicsData$e_meta)) {
        table_use = table_use[table_use$Table != "Biomolecule Information (e_meta)",]
      }
      
      if (is.null(objects$imdanova_res)) {
        table_use = table_use[table_use$Table != "iMd-Anova Table",]
      }
      
      if (is.null(objects$seqstats_res)) {
        table_use = table_use[table_use$Table != "SeqData Stats Table",]
      }
      
      # Reset index numbers
      rownames(table_use) = seq(length=nrow(table_use))

      table_use
    },
    selection = list(selection = "single", selected = 1),
    escape = FALSE
  ),
  
  #'@details Plot type and dimension options for a plot selected in the downloads table
  output$plot_selected_save_options <- renderUI({
    req(input$download_plot_table_rows_selected)
    plot_name <- plots$plot_table[input$download_plot_table_rows_selected, 1]
    plot_file_type <- plots$plot_save_options[[plot_name]]$type
    plot_save_width <- plots$plot_save_options[[plot_name]]$width
    plot_save_height <- plots$plot_save_options[[plot_name]]$height
    plot_save_scale <- plots$plot_save_options[[plot_name]]$scale
    
    fluidRow(
      column(3, selectInput("download_file_type", "File Type", c("HTML Widget", "PNG", "JPG", "SVG"), c(plot_file_type))),
      conditionalPanel(
        "input.download_file_type!='HTML Widget'",
        column(3, numericInput("download_plot_width", "Width", plot_save_width, min = 0)),
        column(3, numericInput("download_plot_height", "Height", plot_save_height, min = 0)),
        column(3, numericInput("download_plot_scale", "Scale", plot_save_scale, min = 0, step = 0.25))
      )
    )
  }),
  
  #' Redraw style UI when another plot is selected.
  output$download_apply_style_UI <- renderUI({
    req(input$download_plot_table_rows_selected)
    return(style_UI("download"))
  }) 
)
