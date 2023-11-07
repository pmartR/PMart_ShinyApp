list(
  # conditionally disabled buttons for spans
  output$spans_conditionalbuttons <- renderUI({
    subset_fn_choices <- c(
      "Everything" = "all", "Top L order statistics (los)" = "los", "Percentage present (ppp)" = "ppp",
      "Complete" = "complete", "Rank invariant (rip)" = "rip", "Percentage present and rank invariant (ppp+rip)" = "ppp_rip"
    )

    norm_fn_choices <- c("Median" = "median", "Mean" = "mean", "Z-norm" = "zscore", "Median Absolute Distance" = "mad")

    spans_subset_fn_choices <- prettyCheckboxGroup("spans_which_subset_fn", "Test which subset functions?", choices = subset_fn_choices, selected = subset_fn_choices)
    spans_norm_fn_choices <- prettyCheckboxGroup("spans_which_norm_fn", "Test which normalization functions?", choices = norm_fn_choices, selected = norm_fn_choices, inline = TRUE)

    if (inherits(objects$omicsData, c("pepData", "proData"))) {
      btns <- div(
        # read the  name of the below button in an action movie villain voice for best performance ('EXCECUTE SPANS!!!!')
        bsButton("execute_spans", "Run SPANS procedure"),
        bsButton("spans_params", "Specify subset function parameters"),
        hidden(div("Running SPANS, please wait...",
          id = "spans_busy", class = "fadein-out",
          style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
        ))
      )
    }
    else {
      btns <- div(
        disabled(bsButton("execute_spans", "Run SPANS procedure")),
        disabled(bsButton("spans_params", "SPANS parameters"))
      )
    }

    tagList(spans_subset_fn_choices, spans_norm_fn_choices, btns)
  }),

  # table output of spansres object
  output$spans_table <- renderDT({
    df <- objects$spans_res
    df$percent_mols_used <- round(df$mols_used_in_norm/nrow(objects$omicsData$e_data)*100)
    
    df[c(3,1:2, 5, 7, 4)]
    
    },
    options = list(scrollX = TRUE, pageLength = 5),
    selection = "single"
  ),

  # spans score plot
  output$spans_plot <- renderPlotly({
    req(!is.null(objects$spans_res))
    p <- plot(objects$spans_res, interactive = T, Npep_bar = T)
    plots$last_plot <- p
    return(p)
  }),

  # plot.normres modal plot showing location parameters ### Requires pmartR update
  output$norm_modal_ba_plots <- renderPlotly({
    
    if (inherits(plots$norm_modal_ba_plots, "list") && !is.null(plots$norm_modal_ba_plots[[1]])) {
      # p <- gridExtra::arrangeGrob(plots$norm_modal_ba_plots[[1]], plots$norm_modal_ba_plots[[2]], ncol = 2)
      p <- subplot(plots$norm_modal_ba_plots, shareY = T, titleX = T, titleY = T)
      # plots$last_plot <- p
      grid::grid.draw(p)
    }
    else {
      plots$last_plot <- plots$norm_modal_ba_plots
      return(plots$norm_modal_ba_plots)
    }
  }),

  # plot.normres modal plot showing location parameters
  output$norm_modal_ba_plots_2 <- renderPlotly({
    if (inherits(plots$norm_modal_ba_plots_2, "list")) {
      # p <- gridExtra::arrangeGrob(plots$norm_modal_ba_plots_2[[1]], plots$norm_modal_ba_plots_2[[2]], ncol = 2)
      p <- subplot(plots$norm_modal_ba_plots_2, shareY = T, titleX = T, titleY = T)
      # plots$last_plot_2 <- p
      grid::grid.draw(p)
    }
    else {
      plots$last_plot_2 <- plots$norm_modal_ba_plots_2
      return(plots$norm_modal_ba_plots_2)
    }
  }),
  #

  # location parameter boxplot
  output$norm_modal_loc_boxplot <- renderPlotly({
    validate(need(!is.null(plots$loc_boxplot), "No location boxplot"))
    plots$loc_boxplot
  }),

  # scale parameter boxplot
  output$norm_modal_scale_boxplot <- renderPlotly({
    validate(need(!is.null(plots$scale_boxplot), "No scale boxplot"))
    plots$scale_boxplot
  }),

  # location parameter boxplot
  output$norm_modal_loc_boxplot_2 <- renderPlotly({
    validate(need(!is.null(plots$loc_boxplot_2), "No location boxplot"))
    plots$loc_boxplot_2
  }),

  # scale parameter boxplot
  output$norm_modal_scale_boxplot_2 <- renderPlotly({
    validate(need(!is.null(plots$scale_boxplot_2), "No scale boxplot"))
    plots$scale_boxplot_2
  }),

  # conditinally display before-after boxplots or boxplots of scale and location parameters.
  output$norm_modal_mainplot <- renderUI({
    if (input$norm_modal_plot_select == "ba") {
      
      # p <- gridExtra::arrangeGrob(plots$norm_modal_ba_plots[[1]], 
      #                             plots$norm_modal_ba_plots[[2]], ncol = 2)
      p <- subplot(plots$norm_modal_ba_plots, shareY = T, titleX = T, titleY = T)
      plots$last_plot <- p
      
      if (!is.null(objects$omicsData_2)) {
        # p <- gridExtra::arrangeGrob(plots$norm_modal_ba_plots_2[[1]], plots$norm_modal_ba_plots_2[[2]], ncol = 2)
        p <- subplot(plots$norm_modal_ba_plots_2, shareY = T, titleX = T, titleY = T)
        plots$last_plot_2 <- p
        
        ui1 <- withSpinner(plotlyOutput("norm_modal_ba_plots"))
        ui2 <- withSpinner(plotlyOutput("norm_modal_ba_plots_2"))

        lipid_tabset_plots(ui1, ui2, input$omic_1_name, input$omic_2_name)
      }
      else {
        withSpinner(plotlyOutput("norm_modal_ba_plots"))
      }
      
    } else if (input$norm_modal_plot_select == "fac") {
      plots_show <- list()
      plots_show[[1]] <- plots$loc_boxplot
      plots_show[[2]] <-  plots$scale_boxplot
      
      if (!is.null(objects$omicsData_2)) {
        plots_show[[3]] <- plots$loc_boxplot_2
        plots_show[[4]] <-  plots$scale_boxplot_2
        
        plots_show = Filter(Negate(is.null), plots_show)
        
        plots$last_plot <- subplot(plots_show, nrows = ifelse(length(plots_show) > 1, 2, 1))
        
        
        ui1 <- splitLayout(
            withSpinner(plotlyOutput("norm_modal_loc_boxplot")),
            withSpinner(plotlyOutput("norm_modal_scale_boxplot"))
        )
        
        ui2 <- splitLayout(
            withSpinner(plotlyOutput("norm_modal_loc_boxplot_2")),
            withSpinner(plotlyOutput("norm_modal_scale_boxplot_2"))
        )

        lipid_tabset_plots(ui1, ui2, input$omic_1_name, input$omic_2_name)
      }
      else {
        plots$last_plot <- subplot(plots_show, nrows = ifelse(length(plots_show) > 1, 2, 1))
        splitLayout(
          withSpinner(plotlyOutput("norm_modal_loc_boxplot")),
          withSpinner(plotlyOutput("norm_modal_scale_boxplot"))
        )
      }
    }
  }),

  # plot normalized data after modal dismiss
  output$normalized_boxplots <- renderPlotly({
    req(pluck(attributes(objects$omicsData), "data_info", "norm_info", "is_normalized") == TRUE)
    bc_addition <- ifelse(attributes(objects$omicsData)$data_info$batch_info$is_bc," (with Batch Correction)","")
    p_init <- plot(objects$omicsData, bw_theme = TRUE, color_by = "Group", order_by = "Group", use_VizSampNames = "VizSampNames" %in% colnames(objects$omicsData$f_data), interactive = F)
    title_text <- paste0(p_init$labels$title,bc_addition)
    p <- plot(objects$omicsData, bw_theme = TRUE, color_by = "Group", order_by = "Group", use_VizSampNames = "VizSampNames" %in% colnames(objects$omicsData$f_data), title_lab = title_text, interactive = T)
    plots$last_plot <- p
    p
  }),

  output$normalized_boxplots_2 <- renderPlotly({
    req(pluck(attributes(objects$omicsData_2), "data_info", "norm_info", "is_normalized") == TRUE)
    bc_addition <- ifelse(attributes(objects$omicsData_2)$data_info$batch_info$is_bc," (with Batch Correction)","")
    p_init <- plot(objects$omicsData_2, bw_theme = TRUE, use_VizSampNames = "VizSampNames" %in% colnames(objects$omicsData$f_data), interactive = F, color_by = "Group", order_by = "Group")
    title_text <- paste0(p_init$labels$title,bc_addition)
    p <- plot(objects$omicsData_2, bw_theme = TRUE, use_VizSampNames = "VizSampNames" %in% colnames(objects$omicsData$f_data), interactive = T, color_by = "Group", order_by = "Group")
    plots$last_plot_2 <- p
    p
  }),
  #

  output$normalized_boxplots_cond <- renderUI({
    req(pluck(attributes(objects$omicsData),"data_info","batch_info","is_bc") != TRUE)
    if (!is.null(objects$omicsData_2)) {
      ui1 <- withSpinner(plotlyOutput("normalized_boxplots"))
      ui2 <- withSpinner(plotlyOutput("normalized_boxplots_2"))
      lipid_tabset_plots(ui1, ui2, input$omic_1_name, input$omic_2_name)
    }
    else {
      withSpinner(plotlyOutput("normalized_boxplots"))
    }
  }),

  # go to rollup tab button only visible in peptide land
  output$goto_stats <- renderUI({
    if (!is.null(input$batch_correction_id) && input$batch_correction_id == "ComBat"){
      # empty we don't return anything
    }
    else if (inherits(objects$omicsData, "pepData")) {
      div(
        actionButton("goto_pepstats", "Continue to Peptide Statistics tab", style = "margin-top:5px;width:75%"),
        br(),
        actionButton("goto_rollup", "Continue to Protein Roll Up tab", style = "margin-top:5px;width:75%")
      )
    }
    else {
      actionButton("goto_statistics", "Continue to Statistics tab", style = "margin-top:5px;width:75%")
    }
  }),

  # group tab warnings
  output$warnings_normalize <- renderUI({
    HTML(paste(revals$warnings_normalize, collapse = ""))
  }),
  
  # batch correction pop up
  output$batch_review <- renderUI({
    divs = list()
    
    if(input$batch_fn == "none") return(
      div(
        br(),
        strong("No batch correction will be applied"),
        br(),
        br()
      )
    )
    
    divs[[i]] <- tagList(
      tags$b("Total Count Filter:"),
      tags$p(
        sprintf("Minimum Count: %s", input$batch_fn),
        ),
      hr()
    )
    
    filtered_disclaimer <- if(length(attr(objects$omicsData, "filters")) > 0) {
      h3(tags$b("Filters have already been applied, these filters will be applied in addition to the existing filters.", style = "color:deepskyblue"))
    } else NULL
    
    tagList(
      filtered_disclaimer,
      hr(),
      hr(),
      h3(tags$b("Batch correction to be applied:  ")),
      divs
    )
    
  }),
  
  # output$inspect_norm_disabled_UI <- renderUI({
  #   req(objects$omicsData)
  #   disable_logic = FALSE
  #   if(input$batch_correction_id == "EigenMS"){disable_logic = TRUE}
  #   bsButton("inspect_norm", "Diagnostics for normalization selection",style = "primary",disabled = disable_logic)
  # }),
  
  output$batch_plots <- renderPlotly({
    p <- subplot(plots$batch_plot_pre,plots$batch_plot_post, shareY = T, titleX = T, titleY = T)
    # plots$last_plot <- p
    p
  }),
  
  # display before and after batch correction plots
  output$batch_plots_mainplot <- renderUI({
      withSpinner(plotlyOutput("batch_plots"))
  }),
  
  # output$apply_bc_method_UI <- renderUI({
  #   req(objects$omicsData)
  #   # # assume that this will be disabled and people won't be using batch correction
  #   # disable_logic = TRUE
  #   # # however if we have batch correction that is not null and molecule filter was added we set button to be enabled
  #   # if(!is.null(input$batch_correction_id) && input$batch_correction_id != "None" && input$add_molfilt %% 2 != 0){disable_logic = FALSE}
  #   # # if batch correction has already been ran we disable it again
  #   # if(attributes(objects$omicsData)$data_info$batch_info$is_bc){disable_logic = TRUE}
  #   # # we also disable the button if combat has been selected but the data has not been normalized yet
  #   # if(attributes(objects$omicsData)$data_info$norm_info$is_norm == FALSE && input$batch_correction_id == "ComBat"){disable_logic = TRUE}
  #   div(
  #     id = "apply_bc_method_tooltip",
  #     class = "tooltip-wrapper",
  #     bsButton("apply_bc_method", "Apply batch correction", style = "primary")
  #   )
  # }),
  
  # output$inspect_norm_UI <- renderUI({
  #   req(objects$omicsData)
  #   div(
  #     id = "inspect_norm_tooltip",
  #     class = "tooltip-wrapper",
  #     bsButton("inspect_norm","Diagnostics for normalization selection",style = "primary"),
  #     style = "width: 100%;"
  #   )
  # }),
  
  # output$apply_normalization_UI <- renderUI({
  #   req(objects$omicsData)
  #   div(
  #     id = "apply_normalization_tooltip",
  #     class = "tooltip-wrapper",
  #     bsButton("apply_normalization", "Apply normalization", style = "primary")
  #   )
  # }),
  
  # plot normalized data after modal dismiss
  output$batch_boxplots <- renderPlotly({
    # req data not be batch corrected
    req(pluck(attributes(objects$omicsData), "data_info", "batch_info", "is_bc") == TRUE)
    # if they come in with batch corrected data, could lead to wonky behavior
    # if batch corrected disable normalization tab
    # need button for has been batch corrected and give pop up
    
    # messageBox() to keep things aesthetically pleasing
    bc_addition <- ifelse(attributes(objects$omicsData)$data_info$batch_info$is_bc," (with Batch Correction)","")
    p_init <- plot(objects$omicsData, bw_theme = TRUE, color_by = "Group", order_by = "Group", use_VizSampNames = "VizSampNames" %in% colnames(objects$omicsData$f_data), interactive = F)
    title_text <- paste0(p_init$labels$title,bc_addition)
    p <- plot(objects$omicsData, bw_theme = TRUE, color_by = "Group", order_by = "Group", use_VizSampNames = "VizSampNames" %in% colnames(objects$omicsData$f_data), title_lab = title_text, interactive = T)
    plots$last_plot <- p
    p
  }),
  
  output$batch_boxplots_2 <- renderPlotly({
    req(pluck(attributes(objects$omicsData_2), "data_info", "batch_info", "is_bc") == TRUE)
    bc_addition <- ifelse(attributes(objects$omicsData_2)$data_info$batch_info$is_bc," (with Batch Correction)","")
    p_init <- plot(objects$omicsData_2, bw_theme = TRUE, use_VizSampNames = "VizSampNames" %in% colnames(objects$omicsData$f_data), interactive = F, color_by = "Group", order_by = "Group")
    title_text <- paste0(p_init$labels$title,bc_addition)
    p <- plot(objects$omicsData_2, bw_theme = TRUE, use_VizSampNames = "VizSampNames" %in% colnames(objects$omicsData$f_data), interactive = T, color_by = "Group", order_by = "Group")
    plots$last_plot_2 <- p
    p
  }),
  #
  
  output$batch_boxplots_cond <- renderUI({
    if (!is.null(objects$omicsData_2)) {
      ui1 <- withSpinner(plotlyOutput("batch_boxplots"))
      ui2 <- withSpinner(plotlyOutput("batch_boxplots_2"))
      lipid_tabset_plots(ui1, ui2, input$lipid_1_name, input$lipid_2_name)
    }
    else {
      withSpinner(plotlyOutput("batch_boxplots"))
    }
  }),
  
  output$normalization_dismiss_UI <- renderUI({
    
    if(!is.null(input$batch_correction_id) && input$batch_correction_id %in% c("ComBat","EigenMS")){
      actionButton("normalization_dismiss", "Review results and apply batch correction", width = "75%")
    } else {
      actionButton("normalization_dismiss", "Review results", width = "75%")
    }
  })
  
  
)
