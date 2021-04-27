# main trelliscope output
output$trelliscope_out <- renderTrelliscope({
  req(!is.null(objects$omicsData))
  # do not draw on button init
  if(input$make_trelliscope == 0){
    NULL
  }
  else{
    isolate({
      object = if(input$boxplots_yn) objects$omicsData else NULL
      stats = if(input$stats_yn) objects$imdanova_res else NULL

      withProgress(message = 'Making your display...', value = 1,
          revals$trelli <- trelliVis(omicsData = object, omicsStats = stats, panel_variable = get_edata_cname(objects$omicsData), 
                                     plot_type = input$trelli_plot_type, trelli_path_out = file.path('www/', paste0('Trelliscope_', session$token)), 
                                     y_limits = 'fixed', trelli_name = 'pmartR_displays')
      )
      
      invisible(print(revals$trelli[[1]]))
    })
  }
})

# dynamic input for plot_type argument in trelliVis
output$trelli_plot_type <- renderUI({
  req(!is.null(objects$omicsData))
  
  choices = c('Abundance Boxplot' = 'abundance_boxplot', 'Relative Abundance' = 'abundance_global', 
              'Abundance Heatmap' = 'abundance_heatmap', 'Missing Barplot' = 'missing_bar', 
              'Presence Heatmap' = 'presence_heatmap', 'Fold-change Barplot' = 'foldchange_bar',
              'Relative Fold-change' = 'foldchange_global', 'Fold-change Heatmap' = 'foldchange_heatmap')
  
  requires_stats = c('foldchange_bar', 'foldchange_global', 'foldchange_heatmap')
  
  # some plots require a statsRes object
  if(is.null(objects$imdanova_res)){
    choices <- choices[!(choices %in% requires_stats)]
  }
  
  pickerInput('trelli_plot_type', 'Plot Type:', choices = choices, multiple = F)
  
})

# panel variable options from edata_cname and all e_meta columns
output$trelli_panel_variable <- renderUI({
  req(!is.null(objects$omicsData))
  
  choices = union(get_edata_cname(objects$omicsData), colnames(objects$omicsData$e_meta))
  
  pickerInput('trelli_panel_variable', 'Choose a variable to divide the data:', choices = choices)
})