# theme and axes labels that are added to the plot at draw time
qc_plot_style <- reactive({
  qc_xangle <- if (is_empty(input$qc_xangle) | is.na(input$qc_xangle)) 90 else input$qc_xangle
  qc_yangle <- if (is_empty(input$qc_yangle) | is.na(input$qc_yangle)) 90 else input$qc_yangle
  
  qc_x_fontsize <- if (is.logical(input$qc_x_fontsize)) 12 else input$qc_x_fontsize
  qc_y_fontsize <- if (is.logical(input$qc_y_fontsize)) 12 else input$qc_y_fontsize
  qc_title_fontsize <- if (is.logical(input$qc_title_fontsize)) 14 else input$qc_title_fontsize
  
  qc_x_ticksize <- if (is.logical(input$qc_x_ticksize)) 9 else input$qc_x_ticksize
  qc_y_ticksize <- if (is.logical(input$qc_y_ticksize)) 9 else input$qc_y_ticksize
  
  
  theme <- theme(
    axis.title.x = element_text(size = qc_x_fontsize),
    axis.title.y = element_text(size = qc_y_fontsize),
    axis.text.x = element_text(angle = qc_xangle, size = qc_x_ticksize, hjust = 1),
    axis.text.y = element_text(angle = qc_yangle, size = qc_y_ticksize, hjust = 1),
    plot.title = element_text(size = qc_title_fontsize)
  )
})

qc_xlab <- reactive({
  if (isTRUE(input$qc_xlab == "")) NULL else xlab(input$qc_xlab)
})

qc_ylab <- reactive({
  if (isTRUE(input$qc_ylab == "")) NULL else ylab(input$qc_ylab)
})

qc_title <- reactive({
  if (isTRUE(input$qc_title == "")) NULL else ggtitle(input$qc_title)
})

qc_flip <- reactive({
  if (isTRUE(input$qc_flip_axes == "TRUE")) coord_flip() else NULL
})
