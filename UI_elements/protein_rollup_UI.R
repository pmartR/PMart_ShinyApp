list(
  # plot of prodata after rollup
  output$rollup_plot <- renderPlot({
    req(!is.null(objects$omicsData), cancelOutput = TRUE)
    plots$last_plot <- plots$rollup_plot
    plots$rollup_plot
  }),
  
  # inputs for axes labels and sizes
  output$rollup_plot_options <- renderUI({
    style_UI('rollup')
  }),
  
  # apply filter plot style options
  output$rollup_apply_style <- renderUI({
    apply_style_UI('rollup', FALSE, FALSE)
  }),
  
  output$rollup_data_summary <- renderDT({
    req(!is.null(revals$rollup_summary), cancelOutput = TRUE)
    revals$rollup_summary[1:6,]
  }, rownames = T, options = list(dom = 't')),
  
  output$warnings_rollup <- renderUI({
    HTML(paste(revals$warnings_rollup, collapse = ""))
  })
)