list(
  # conditionally disabled buttons for spans
  output$spans_conditionalbuttons <- renderUI({
    
    subset_fn_choices <- c('Everything' = 'all', 'Top L order statistics (los)' = 'los', 'Percentage present (ppp)' = 'ppp', 
                           'Complete' = 'complete', 'Rank invariant (rip)' = 'rip', 'Percentage present and rank invariant (ppp+rip)' = 'ppp_rip')
    
    norm_fn_choices <- c('Mean' = 'mean', 'Median' = 'median', 'Z-norm' = 'zscore', 'Median Absolute Distance' = 'mad')
    
    spans_subset_fn_choices <- prettyCheckboxGroup('spans_which_subset_fn', 'Test which subset functions?', choices = subset_fn_choices, selected = subset_fn_choices)
    spans_norm_fn_choices <- prettyCheckboxGroup('spans_which_norm_fn', 'Test which normalization functions?', choices = norm_fn_choices, selected = norm_fn_choices, inline = TRUE)
    
    if(inherits(objects$omicsData, c('pepData', 'proData'))){
       btns <- div(
                    # read the  name of the below button in an action movie villain voice for best performance ('EXCECUTE SPANS!!!!')
                    bsButton('execute_spans', 'Run SPANS procedure'),
                    bsButton('spans_params', 'Specify subset function parameters'),
                    hidden(div('Running SPANS, please wait...', id = 'spans_busy', class = 'fadein-out', 
                               style = 'color:deepskyblue;font-weight:bold;margin-bottom:5px'))
                  )
    }
    else{
      btns <- div(
                disabled(bsButton('execute_spans', 'Run SPANS procedure')),
                disabled(bsButton('spans_params', 'SPANS parameters'))
              )
    }
    
    tagList(spans_subset_fn_choices, spans_norm_fn_choices, btns)
  }),
  
  # table output of spansres object
  output$spans_table <- renderDT(
    objects$spans_res, options = list(scrollX = TRUE),
  selection = 'single'),
  
  # spans score plot
  output$spans_plot <- renderPlot({
    req(!is.null(objects$spans_res))
    p <- plot(objects$spans_res, plot_type = 'ggplot')
    plots$last_plot <- p
    return(p)
  }),
  
  # plot.normres modal plot showing location parameters
  output$norm_modal_ba_plots <- renderPlot({
    if(inherits(plots$norm_modal_ba_plots, 'list')){
      p <- gridExtra::arrangeGrob(plots$norm_modal_ba_plots[[1]], plots$norm_modal_ba_plots[[2]], ncol = 2) 
      plots$last_plot <- p
      grid::grid.draw(p)
    }
    else{
      plots$last_plot <- plots$norm_modal_ba_plots
      return(plots$norm_modal_ba_plots)
    }
  }),
  
      # plot.normres modal plot showing location parameters
      output$norm_modal_ba_plots_2 <- renderPlot({
        if(inherits(plots$norm_modal_ba_plots_2, 'list')){
          p <- gridExtra::arrangeGrob(plots$norm_modal_ba_plots_2[[1]], plots$norm_modal_ba_plots_2[[2]], ncol = 2) 
          plots$last_plot_2 <- p
          grid::grid.draw(p)
        }
        else{
          plots$last_plot_2 <- plots$norm_modal_ba_plots_2
          return(plots$norm_modal_ba_plots_2)
        }
      }),
  #
  
  # location parameter boxplot
  output$norm_modal_loc_boxplot <- renderPlot({
    validate(need(!is.null(plots$loc_boxplot), 'No location boxplot'))
    plots$loc_boxplot
  }),
  
  # scale parameter boxplot
  output$norm_modal_scale_boxplot <- renderPlot({
    validate(need(!is.null(plots$scale_boxplot), 'No scale boxplot'))
    plots$scale_boxplot
  }),
  
      # location parameter boxplot
      output$norm_modal_loc_boxplot_2 <- renderPlot({
        validate(need(!is.null(plots$loc_boxplot_2), 'No location boxplot'))
        plots$loc_boxplot_2
      }),
      
      # scale parameter boxplot
      output$norm_modal_scale_boxplot_2 <- renderPlot({
        validate(need(!is.null(plots$scale_boxplot_2), 'No scale boxplot'))
        plots$scale_boxplot_2
      }),
  
  # conditinally display before-after boxplots or boxplots of scale and location parameters.
  output$norm_modal_mainplot <- renderUI({
    if(input$norm_modal_plot_select == 'ba'){
      if(!is.null(objects$omicsData_2)){
        tagList(
          plotOutput('norm_modal_ba_plots'),
          plotOutput('norm_modal_ba_plots_2')
        )
      }
      else plotOutput('norm_modal_ba_plots')
    }
    else if(input$norm_modal_plot_select == 'fac'){
      if(!is.null(objects$omicsData_2)){
        tagList(
          splitLayout(
            plotOutput('norm_modal_loc_boxplot'),
            plotOutput('norm_modal_scale_boxplot')
          ),
          splitLayout(
            plotOutput('norm_modal_loc_boxplot_2'),
            plotOutput('norm_modal_scale_boxplot_2')
          )
        )
      }
      else{
        splitLayout(
          plotOutput('norm_modal_loc_boxplot'),
          plotOutput('norm_modal_scale_boxplot')
        )
      }
    }
  }),
  
  # plot normalized data after modal dismiss
  output$normalized_boxplots <- renderPlot({
    req(pluck(attributes(objects$omicsData), 'data_info', 'norm_info', 'is_normalized') == TRUE)
    plot(objects$omicsData, bw_theme = TRUE)
  }),
  
    output$normalized_boxplots_2 <- renderPlot({
      req(pluck(attributes(objects$omicsData_2), 'data_info', 'norm_info', 'is_normalized') == TRUE)
      plot(objects$omicsData_2, bw_theme = TRUE)
    }),
  #
  
  output$normalized_boxplots_cond <- renderUI({
    if(!is.null(objects$omicsData_2)){
      tagList(
        plotOutput('normalized_boxplots'),
        plotOutput('normalized_boxplots_2')
      )
    }
    else{
      plotOutput('normalized_boxplots')
    }
  }), 
  
  # go to rollup tab button only visible in peptide land
  output$goto_rollup <- renderUI({
    if(inherits(objects$omicsData, 'pepData')){
      actionButton('goto_rollup', 'Continue to protein rollup tab', style = 'margin-top:5px;width:75%')
    }
    else NULL
  }),
  
  # group tab warnings
  output$warnings_normalize <- renderUI({
    HTML(paste(revals$warnings_normalize, collapse = ""))
  })
  
)
