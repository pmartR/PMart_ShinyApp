list(
  # render a plot depending on which row in the plot table is selected
  output$download_plot <- renderPlot({
    req(length(input$download_plot_table_rows_selected) > 0, cancelOutput = TRUE)
    plot_name <- plots$plot_table[input$download_plot_table_rows_selected, 1]
    p <- plots$allplots[[plot_name]]

    if (inherits(p, "gtable")) {
      grid::grid.draw(p)
    }
    else {
      return(p)
    }
  }),

  # render the plot table reactive value
  output$download_plot_table <- renderDT(plots$plot_table, selection = "single", escape = FALSE),

  output$download_tables_table <- renderDT(
    {
      tables$tables_table
    },
    selection = "single",
    escape = FALSE
  )
)
