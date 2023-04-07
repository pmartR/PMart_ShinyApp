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
  output$spans_table <- renderDT(
    objects$spans_res,
    options = list(scrollX = TRUE),
    selection = "single"
  ),

  # spans score plot
  output$spans_plot <- renderPlotly({
    req(!is.null(objects$spans_res))
    p <- plot(objects$spans_res, interactive = T
              )
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
        
        tagList(
          withSpinner(plotlyOutput("norm_modal_ba_plots")),
          withSpinner(plotlyOutput("norm_modal_ba_plots_2"))
        )
      }
      else {
        withSpinner(plotlyOutput("norm_modal_ba_plots"))
      }
    }
    else if (input$norm_modal_plot_select == "fac") {
      plots_show <- list()
      plots_show[[1]] <- plots$loc_boxplot
      plots_show[[2]] <-  plots$scale_boxplot
      
      if (!is.null(objects$omicsData_2)) {
        plots_show[[3]] <- plots$loc_boxplot_2
        plots_show[[4]] <-  plots$scale_boxplot_2
        
        plots_show = Filter(Negate(is.null), plots_show)
        
        plots$last_plot <- subplot(plots_show, nrows = ifelse(length(plots_show) > 1, 2, 1))
        
        tagList(
          splitLayout(
            withSpinner(plotlyOutput("norm_modal_loc_boxplot")),
            withSpinner(plotlyOutput("norm_modal_scale_boxplot"))
          ),
          splitLayout(
            withSpinner(plotlyOutput("norm_modal_loc_boxplot_2")),
            withSpinner(plotlyOutput("norm_modal_scale_boxplot_2"))
          )
        )
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
    p <- plot(objects$omicsData, bw_theme = TRUE, color_by = "Group", order_by = "Group", interactive = T)
    plots$last_plot <- p
    p
  }),

  output$normalized_boxplots_2 <- renderPlotly({
    req(pluck(attributes(objects$omicsData_2), "data_info", "norm_info", "is_normalized") == TRUE)
    p <- plot(objects$omicsData_2, bw_theme = TRUE, interactive = T, color_by = "Group", order_by = "Group")
    plots$last_plot_2 <- p
    p
  }),
  #

  output$normalized_boxplots_cond <- renderUI({
    if (!is.null(objects$omicsData_2)) {
      tagList(
        withSpinner(plotlyOutput("normalized_boxplots")),
        withSpinner(plotlyOutput("normalized_boxplots_2"))
      )
    }
    else {
      withSpinner(plotlyOutput("normalized_boxplots"))
    }
  }),

  # go to rollup tab button only visible in peptide land
  output$goto_stats <- renderUI({
    if (inherits(objects$omicsData, "pepData")) {
      actionButton("goto_pepstats", "Continue to Peptide Statistics tab", style = "margin-top:5px;width:75%")
    }
    else {
      actionButton("goto_statistics", "Continue to Statistics tab", style = "margin-top:5px;width:75%")
    }
  }),

  # group tab warnings
  output$warnings_normalize <- renderUI({
    HTML(paste(revals$warnings_normalize, collapse = ""))
  })
)
