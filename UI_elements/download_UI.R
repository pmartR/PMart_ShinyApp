list(
  
  output$download_plot_UI <- renderUI({

    if(length(input$download_plot_table_rows_selected) < 1){
      return(NULL)
    } 
    
    plot_name <- plots$plot_table[input$download_plot_table_rows_selected, 1]
    p <- plots$allplots[[plot_name]]
    
    
    if(inherits(p, c("ggplot", "gtable"))){
      return(withSpinner(plotOutput("download_plot")))
    } else {
      return(withSpinner(plotlyOutput("download_plotly")))
    }
  }),
  
  # render a plot depending on which row in the plot table is selected
  output$download_plot <- renderPlot({
    # req(length(input$download_plot_table_rows_selected) > 0, cancelOutput = TRUE)
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
    # req(length(input$download_plot_table_rows_selected) > 0, cancelOutput = TRUE)
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

      table_use
    },
    selection = list(selection = "single", selected = 1),
    escape = FALSE
  )
)
