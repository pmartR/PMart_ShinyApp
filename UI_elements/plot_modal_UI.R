list(
  # render a plot depending on which row in the plot table is selected
  output$modal_plot <- renderPlot({
    req(length(input$modal_plot_table_rows_selected) > 0, cancelOutput = TRUE)
    plot_name = plots$plot_table[input$modal_plot_table_rows_selected,1]
    p <- plots$allplots[[plot_name]]
    
    if(inherits(p, 'gtable')){
      grid::grid.draw(p)
    }
    else return(p)
    
  }),
  
  # render the plot table reactive value
  output$modal_plot_table <- renderDT(plots$plot_table, selection = 'single', escape = FALSE),
  
  # display number of saved plots for saved plots button
  output$n_saved_plots <- renderUI({
    if(nrow(plots$plot_table) > 0){
      tags$span(paste0('View all saved plots: (', nrow(plots$plot_table), ')'))
    }
    else tags$span('View all saved plots (0)')
  })
)