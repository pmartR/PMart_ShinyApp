# make statres object
observeEvent(input$apply_imdanova,{
  req(!is.null(objects$omicsData), input$top_page == 'Analysis')
  
  tryCatch({
    objects$imdanova_res <- imd_anova(objects$omicsData, test_method = input$test_method, pval_adjust = input$pval_adjust, pval_thresh = input$pval_thresh)
  }, error = function(e){
    msg <- paste0('Something went wrong running the analysis.  \n System error:  ', e)
    message(msg)
    revals$warnings_analysis$bad_imdanova <<- sprintf("<p style = 'color:red'>%s</p>", msg)
    objects$imdanova_res <- NULL
    plots$analysis_mainplot <- NULL
  })
})

# make plot object
observeEvent(c(objects$imdanova_res, input$imdanova_plot_type),{
  req(!is.null(objects$imdanova_res))
  tryCatch({
    plots$analysis_mainplot <- plot(objects$imdanova_res, plot_type = input$imdanova_plot_type, bw_theme = TRUE)
    updateCollapse(session, 'analysis_collapse_main', open = 'analysis_plots')
  }, error = function(e){
    msg <- paste0('Something went wrong plotting your imdanovaRes object.  \n System error:  ', e)
    message(msg)
    revals$warnings_analysis$bad_imdanova_plot <<- sprintf("<p style = 'color:red'>%s</p>", msg)
    plots$analysis_mainplot <- NULL
  })
})

# apply plot styling to...

#...first plot...
observeEvent(input$analysis_apply_style_plot_1,{
  analysis_xangle <- if(is_empty(input$analysis_xangle) | is.na(input$analysis_xangle)) 0 else input$analysis_xangle
  analysis_yangle <- if(is_empty(input$analysis_yangle) | is.na(input$analysis_yangle)) 0 else input$analysis_yangle
  
  theme <- theme(axis.title.x = element_text(size = input$analysis_x_fontsize), 
                 axis.title.y = element_text(size = input$analysis_y_fontsize),
                 axis.text.x = element_text(angle = analysis_xangle, size = input$analysis_x_ticksize),
                 axis.text.y = element_text(angle = analysis_yangle, size = input$analysis_y_ticksize),
                 plot.title = element_text(size = input$analysis_title_fontsize))
  
  if(inherits(plots$analysis_mainplot, 'list')){
    plots$analysis_mainplot[[1]] <- plots$analysis_mainplot[[1]] + xlab(input$analysis_xlab) + ylab(input$analysis_ylab) + ggtitle(input$analysis_title) + theme
  }
  else plots$analysis_mainplot <- plots$analysis_mainplot + xlab(input$analysis_xlab) + ylab(input$analysis_ylab) + ggtitle(input$analysis_title) + theme
})

#...second plot
observeEvent(input$analysis_apply_style_plot_2,{
  analysis_xangle <- if(is_empty(input$analysis_xangle) | is.na(input$analysis_xangle)) 0 else input$analysis_xangle
  analysis_yangle <- if(is_empty(input$analysis_yangle) | is.na(input$analysis_yangle)) 0 else input$analysis_yangle
  
  theme <- theme(axis.title.x = element_text(size = input$analysis_x_fontsize), 
                 axis.title.y = element_text(size = input$analysis_y_fontsize),
                 axis.text.x = element_text(angle = analysis_xangle, size = input$analysis_x_ticksize),
                 axis.text.y = element_text(angle = analysis_yangle, size = input$analysis_y_ticksize),
                 plot.title = element_text(size = input$analysis_title_fontsize))
  
  if(inherits(plots$analysis_mainplot, 'list')){
    plots$analysis_mainplot[[2]] <- plots$analysis_mainplot[[2]] + xlab(input$analysis_xlab) + ylab(input$analysis_ylab) + ggtitle(input$analysis_title) + theme
  }
  else plots$analysis_mainplot_2 <- plots$analysis_mainplot_2 + xlab(input$analysis_xlab) + ylab(input$analysis_ylab) + ggtitle(input$analysis_title) + theme
})
