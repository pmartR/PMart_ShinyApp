list(
  
  output$download_plot_UI <- renderUI({

    if(length(input$download_plot_table_rows_selected) < 1){
      return(NULL)
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
  ),
  
  output$download_plot_options_UI <- renderUI({
    if(length(input$download_plot_table_rows_selected) < 1){
      return(NULL)
    } 
    
    plot_name <- plots$plot_table[input$download_plot_table_rows_selected, 1]
    plot_file_type <- plots$plot_save_options[[plot_name]]$type
    plot_save_width <- plots$plot_save_options[[plot_name]]$width
    plot_save_height <- plots$plot_save_options[[plot_name]]$height
    plot_save_scale <- plots$plot_save_options[[plot_name]]$scale
    
    return (div(
      bsCollapsePanel("Axes Options",
                      value = "axes_options",
                      style_UI("download"),
                      apply_style_UI("download", FALSE, FALSE)
      ),
      bsCollapsePanel("Save Options",
                      value = "save_options",
                      div(
                        fluidRow(
                          column(3, selectInput("download_file_type", "File Type", c("HTML Widget", "PNG", "JPG", "SVG"), c(plot_file_type))),
                          conditionalPanel(
                            "input.download_file_type!='HTML Widget'",
                            column(3, numericInput("download_plot_width", "Width", plot_save_width)),
                            column(3, numericInput("download_plot_height", "Height", plot_save_height)),
                            column(3, numericInput("download_plot_scale", "Scale", plot_save_scale, min = 0, step = 0.25))
                          )
                        ),
                        fluidRow(
                          column(1, actionButton("download_apply_save_options", "Apply")),
                          column(1, conditionalPanel("input.download_file_type!='HTML Widget'", actionButton("download_preview_image", "Preview")))
                        ),
                        div(style="overflow:auto", uiOutput("download_image_preview"))
                      ))
    ))
  })
)
