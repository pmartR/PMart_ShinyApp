# limit adjustment options depending on test method
output$pval_adjust <- renderUI({
  req(input$test_method)
  if(input$test_method == 'anova'){
    choices = c('Holm'='holm', 'Bonferroni'='bonferroni', 'Tukey'='tukey', 'Dunnet'='dunnett', 'None'='none')
  }
  else choices = c('Holm'='holm', 'Bonferroni'='bonferroni', 'None'='none')
  pickerInput('pval_adjust', 'Multiple comparisons adjustment', choices = choices)
})

# main plot display which takes in the plot object that is created immediately after imd_anova() is run
output$analysis_mainplot <- renderPlot({
  req(!is.null(plots$analysis_mainplot))
  p <- plots$analysis_mainplot
  if(inherits(p, 'list')){
    p <- gridExtra::arrangeGrob(p[[1]], p[[2]], ncol = 2)
    plots$last_plot <- p
    grid::grid.draw(p)
  }
  else{
    plots$last_plot <- p
    return(p)
  }
})

# display table output from imd_anova
output$analysis_summary_table <- renderDT({
  objects$imdanova_res[['Full_results']]
})

# theme UI 
output$analysis_plot_options <- renderUI({style_UI('analysis')})
output$analysis_apply_style <- renderUI({apply_style_UI('analysis', FALSE, inherits(plots$analysis_mainplot, 'list'))})