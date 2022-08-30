list(
  #' #'@details Choose which plot type to view.  Disabled for some data types.
  #' output$which_qc_plot_UI <- renderUI({
  #'   req(!is.null(objects$omicsData))
  #'   radioGroupButtons("which_qc_plot", "Choose a Plot Type:",
  #'                     choices = c("Boxplots" = "boxplots", 
  #'                                 "Missing Values Barplots" = "bar", 
  #'                                 "Missing Values Scatterplots" = "scatter")
  #'   ),
  #'   
  #'   disable(selector = "#somevalue button:eq(1)")
  #' })
  
  # dropdowns specifying what variable to order boxplots by
  output$qc_order_by <- renderUI({
    req(!is.null(objects$omicsData))
    choices <- colnames(objects$omicsData$f_data %>% dplyr::select(-one_of(attributes(objects$omicsData)$cnames$fdata_cname)))
    pickerInput("qc_order_by", NULL,
      choices = c("Select one", choices, "Group Levels" = "Group"),
      selected = all_inputs()$qc_order_by,
      options = pickerOptions(dropupAuto = FALSE)
    )
  }),

  output$qc_order_by_2 <- renderUI({
    req(!is.null(objects$omicsData_2))
    choices <- colnames(objects$omicsData_2$f_data %>% dplyr::select(-one_of(attributes(objects$omicsData_2)$cnames$fdata_cname)))
    pickerInput("qc_order_by_2", NULL,
      choices = c("Select one(dataset 2)" = "Select one", choices, "Group Levels" = "Group"),
      selected = all_inputs()$qc_order_by_2
    )
  }),
  #

  # dropdowns specifying what color to order boxplots by
  output$qc_color_by <- renderUI({
    req(!is.null(objects$omicsData))
    choices <- colnames(objects$omicsData$f_data %>% dplyr::select(-one_of(attributes(objects$omicsData)$cnames$fdata_cname)))
    pickerInput("qc_color_by", NULL,
      choices = c("Select one", choices, "Group Levels" = "Group"),
      selected = all_inputs()$qc_color_by
    )
  }),

  output$qc_color_by_2 <- renderUI({
    req(!is.null(objects$omicsData_2))
    choices <- colnames(objects$omicsData_2$f_data %>% dplyr::select(-one_of(attributes(objects$omicsData_2)$cnames$fdata_cname)))
    pickerInput("qc_color_by_2", NULL,
      choices = c("Select one (dataset 2)" = "Select one", choices, "Group Levels" = "Group"),
      selected = all_inputs()$qc_color_by_2
    )
  }),
  #

  # qc tab plot panel
  output$qc_plots <- renderUI({
    if (two_lipids()) {
      tagList(
        div(id = "qc_plots_1", 
            style = "border-style:solid;border-width:1px;", 
            withSpinner(plotlyOutput("omicsData_plot"))
            ),
        div(id = "qc_plots_2", 
            style = "border-style:solid;border-width:1px;",
            withSpinner(plotlyOutput("omicsData_plot_2"))
            )
      )
    }
    else {
      div(id = "qc_plots_1", 
          style = "border-style:solid;border-width:1px;", 
          withSpinner(plotlyOutput("omicsData_plot"))
          )
    }
  }),

  # group summary tables
  output$qc_summary <- renderDT(revals$groups_summary, rownames = T, options = list(dom = "t")),
  output$qc_summary_2 <- renderDT(revals$groups_summary_2, rownames = T, options = list(dom = "t")),

  output$qc_data_summary <- renderUI({
    req(!is.null(revals$groups_summary), cancelOutput = TRUE)
    wellPanel(
      if (two_lipids()) {
        req(!is.null(revals$groups_summary_2), cancelOutput = TRUE)
        splitLayout(
          DTOutput("qc_summary"),
          DTOutput("qc_summary_2")
        )
      }
      else {
        DTOutput("qc_summary")
      }
    )
  }),

  #### MAIN PANEL, BOTH PLOTS ####

  output$omicsData_plot <- renderPlotly({
    req(!is.null(objects$omicsData))
    input$qc_apply_style_plot_1

    # commonly used params
    use_VizSampNames <- "VizSampNames" %in% colnames(objects$omicsData$f_data)
    
    order_by <- if (isTRUE(input$qc_order_by == "Select one")) NULL else input$qc_order_by
    color_by <- if (isTRUE(input$qc_color_by == "Select one")) NULL else input$qc_color_by
    
    transformation <- if(input$datatype == "seq") "lcpm" else NULL
    
    # ifelse chain for which type of plot
    if (input$which_qc_plot == "boxplots") {
      p <- plot(objects$omicsData,
        order_by = order_by, color_by = color_by,
        use_VizSampNames = use_VizSampNames,
        bw_theme = TRUE, transformation = transformation
      )
    }
    else {
      p <- plot(missingval_result(objects$omicsData),
        objects$omicsData,
        order_by = order_by, color_by = color_by,
        plot_type = input$which_qc_plot,
        use_VizSampNames = use_VizSampNames,
        palette = input$qc_colors,
        bw_theme = TRUE
      )
    }

    p <- p + isolate(qc_plot_style()) + 
      isolate(qc_xlab()) + isolate(qc_ylab()) + 
      isolate(qc_title()) + qc_flip()
    
    p <- p %>% ggplotly()
    plots$last_plot <- p
    return(p)
  }),

  output$omicsData_plot_2 <- renderPlotly({
    req(!is.null(objects$omicsData_2))
    input$qc_apply_style_plot_2

    # commonly used params
    use_VizSampNames <- "VizSampNames" %in% colnames(objects$omicsData_2$f_data)

    # ifelse chain for which type of plot
    if (input$which_qc_plot == "boxplots") {
      # specific boxplot options
      order_by <- if (isTRUE(input$qc_order_by_2 == "Select one")) NULL else input$qc_order_by_2
      color_by <- if (isTRUE(input$qc_color_by_2 == "Select one")) NULL else input$qc_color_by_2

      p <- plot(objects$omicsData_2,
        order_by = order_by, color_by = color_by,
        bw_theme = TRUE,
        use_VizSampNames = use_VizSampNames
      )
    }
    else {
      p <- plot(missingval_result(objects$omicsData_2),
                objects$omicsData_2, 
        plot_type = input$which_qc_plot,
        use_VizSampNames = use_VizSampNames,
        bw_theme = TRUE,
        palette = input$qc_colors
      )
    }
    
    p <- p + isolate(qc_plot_style()) + 
      isolate(qc_xlab()) + isolate(qc_ylab()) + 
      isolate(qc_title()) + qc_flip()
    p <- p  %>% ggplotly()
    plots$last_plot_2 <- p
    return(p)
  }),

  output$qc_plot_options <- renderUI({
    style_UI("qc")
  }),

  # apply filter plot style options
  output$qc_apply_style <- renderUI({
    apply_style_UI("qc", two_lipids(), two_lipids(), TRUE)
  }),

  output$warnings_transform <- renderUI({
    HTML(paste(revals$warnings_transform, collapse = ""))
  })
  #
)
