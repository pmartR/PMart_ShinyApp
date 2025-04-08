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
  output$qc_order_by_UI <- renderUI({
    req(!is.null(objects$omicsData))
    choices <- colnames(objects$omicsData$f_data %>% dplyr::select(-one_of(attributes(objects$omicsData)$cnames$fdata_cname)))
    
    name_append = if (two_lipids() || two_metab()) sprintf(" (%s)", omic_1_name()) else ""
    
    pickerInput("qc_order_by", NULL,
      choices = c(choices, "Group Levels" = "Group"),
      selected = all_inputs()$qc_order_by,
      multiple = TRUE,
      options = pickerOptions(
        dropupAuto = FALSE, maxOptions = 1,
        noneSelectedText = sprintf("Select one%s", name_append)
      )
    )
    
  }),

  output$qc_order_by_2_UI <- renderUI({
    req(!is.null(objects$omicsData_2))
    choices <- colnames(objects$omicsData_2$f_data %>% dplyr::select(-one_of(attributes(objects$omicsData_2)$cnames$fdata_cname)))

    name_append = if (two_lipids() || two_metab()) sprintf(" (%s)", omic_2_name()) else ""
    
    pickerInput("qc_order_by_2", NULL,
      choices = c(choices, "Group Levels" = "Group"),
      selected = all_inputs()$qc_order_by_2,
      multiple = TRUE,
      options = pickerOptions(
        dropupAuto = FALSE, maxOptions = 1,
        noneSelectedText = sprintf("Select one%s", name_append)  
      )
    )
    
  }),
  #

  # dropdowns specifying what color to order boxplots by
  output$qc_color_by_UI <- renderUI({
    req(!is.null(objects$omicsData))
    choices <- colnames(objects$omicsData$f_data %>% dplyr::select(-one_of(attributes(objects$omicsData)$cnames$fdata_cname)))

    name_append = if (two_lipids() || two_metab()) sprintf(" (%s)", omic_1_name()) else ""
    
    pickerInput("qc_color_by", NULL,
      choices = c(choices, "Group Levels" = "Group"),
      selected = all_inputs()$qc_color_by,
      multiple = TRUE,
      options = pickerOptions(
        dropupAuto = FALSE, maxOptions = 1,
        noneSelectedText = sprintf("Select one%s", name_append)
      )
    )
    
  }),

  output$qc_color_by_2_UI <- renderUI({
    req(!is.null(objects$omicsData_2))
    choices <- colnames(objects$omicsData_2$f_data %>% dplyr::select(-one_of(attributes(objects$omicsData_2)$cnames$fdata_cname)))

    name_append = if (two_lipids() || two_metab()) sprintf(" (%s)", omic_2_name()) else ""
    
    pickerInput("qc_color_by_2", NULL,
      choices = c(choices, "Group Levels" = "Group"),
      selected = all_inputs()$qc_color_by_2,
      multiple = TRUE,
      options = pickerOptions(
        dropupAuto = FALSE, maxOptions = 1, 
        noneSelectedText = sprintf("Select one%s", name_append)
      )
    )
    
  }),
  
  output$qc_shape_by_UI <- renderUI({
    req(!is.null(objects$omicsData))
    choices <- colnames(objects$omicsData$f_data %>% dplyr::select(-one_of(attributes(objects$omicsData)$cnames$fdata_cname)))
    
    noselect = if(two_lipids() || two_metab()) sprintf("Select one (%s)", omic_1_name()) else "Select one"
    
    pickerInput("qc_shape_by", NULL,
      choices = c(choices, "Group Levels" = "Group"),
      selected = all_inputs()$qc_shape_by,
      multiple = TRUE,
      options = pickerOptions(
        dropupAuto = FALSE, maxOptions = 1, noneSelectedText = noselect)
    )
    
  }),

  output$qc_shape_by_2_UI <- renderUI({
    req(!is.null(objects$omicsData_2))
    choices <- colnames(objects$omicsData_2$f_data %>% dplyr::select(-one_of(attributes(objects$omicsData_2)$cnames$fdata_cname)))

    noselect = sprintf("Select one (%s)", omic_2_name())
    
    pickerInput("qc_shape_by_2", NULL,
      choices = c(setNames("Select one", noselect), choices, "Group Levels" = "Group"),
      selected = all_inputs()$qc_shape_by_2,
      multiple = TRUE,
      options = pickerOptions(
        dropupAuto = FALSE, maxOptions = 1, noneSelectedText = noselect)
    )
    
  }),
  
  #

  # qc tab plot panel
  output$qc_plots <- renderUI({
    if (two_lipids() || two_metab()) {
      d1 = div(id = "qc_plots_1", 
            style = "border-style:solid;border-width:1px;", 
            withSpinner(plotlyOutput("omicsData_plot"))
            )
      d2 = div(id = "qc_plots_2", 
            style = "border-style:solid;border-width:1px;",
            withSpinner(plotlyOutput("omicsData_plot_2"))
            )
      lipid_tabset_plots(d1, d2, input$omic_1_name, input$omic_2_name)
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
      if (two_lipids() || two_metab()) {
        req(!is.null(revals$groups_summary_2), cancelOutput = TRUE)
        splitLayout(
          tagList(tags$h4(omic_1_name()), DTOutput("qc_summary")),
          tagList(tags$h4(omic_2_name()),  DTOutput("qc_summary_2"))
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
    input$qc_redraw_plot
    
    isolate({
      # commonly used params
      use_VizSampNames <- "VizSampNames" %in% colnames(objects$omicsData$f_data)
      
      # leftover from when there was more logic here, leaving in case we want to reintroduce
      order_by <- input$qc_order_by
      color_by <- input$qc_color_by
      shape_by <- input$qc_shape_by

      transformation <- if(input$datatype == "seq") "lcpm" else NULL
      
      # ifelse chain for which type of plot
      if (input$which_qc_plot == "boxplots") {
        pargs <- list(
          "x" = objects$omicsData,
          "order_by" = order_by,
          "color_by" = color_by,
          "use_VizSampNames" = use_VizSampNames,
          "bw_theme" = TRUE
        )
        
        if(!is.null(transformation)) pargs[['transformation']] <- transformation
        
        p <- do.call(plot, pargs)
      }
      else if(input$which_qc_plot == 'pca') {
        
        set.seed(input$set_seed_qc)
        
        .dimres <- dim_reduction(objects$omicsData)
        dimres_table <- .dimres
        attr(dimres_table, 'class') <- c(attr(dimres_table, 'class'), 'data.frame')
        objects$dimred_table_qc <- dimres_table
        
        showNotification(
          "Your dimension reduction results from this page have been saved and can be downloaded on the Download tab", 
          duration = 7,
        )
        
        p <- plot(.dimres, omicsData = objects$omicsData, color_by = color_by, shape_by = shape_by)
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
    })
  }),

  output$omicsData_plot_2 <- renderPlotly({
    req(!is.null(objects$omicsData_2))
    input$qc_apply_style_plot_2
    input$qc_redraw_plot

    isolate({
      # commonly used params
      use_VizSampNames <- "VizSampNames" %in% colnames(objects$omicsData_2$f_data)

      # leftover from when there was more logic here, leaving in case we want to reintroduce
      order_by <- input$qc_order_by_2
      color_by <- input$qc_color_by_2
      shape_by <- input$qc_shape_by_2

      # ifelse chain for which type of plot
      if (input$which_qc_plot == "boxplots") {
        p <- plot(objects$omicsData_2,
          order_by = order_by, color_by = color_by,
          bw_theme = TRUE,
          use_VizSampNames = use_VizSampNames
        )
      }
      else if(input$which_qc_plot == 'pca') {
        
        set.seed(input$set_seed_qc)
        
        .dimres <- dim_reduction(objects$omicsData_2)
        dimres_table <- .dimres
        attr(dimres_table, 'class') <- c(attr(dimres_table, 'class'), 'data.frame')
        objects$dimred_table_qc_2 <- dimres_table
        p <- plot(.dimres, omicsData = objects$omicsData, color_by = color_by, shape_by = shape_by)
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
    })
  }),

  output$qc_plot_options <- renderUI({
    style_UI("qc")
  }),

  # apply filter plot style options
  output$qc_apply_style <- renderUI({
    apply_style_UI("qc", 
                   two_lipids() || two_metab(), 
                   two_lipids() || two_metab(), 
                   TRUE, omic_1_name = omic_1_name(), omic_2_name = omic_2_name())
  }),

  output$warnings_transform <- renderUI({
    HTML(paste(revals$warnings_transform, collapse = ""))
  })
  #
)
