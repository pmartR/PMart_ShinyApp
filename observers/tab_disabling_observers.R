#'@details disable stats and rollup if e_meta does not exist or if we
#'are not working with pepData.  We will still leave the tabs enabled if we have
#'rolled up to pepData, but for review only.
observe({
  emeta_exists <- isTruthy(revals$e_meta) | isTruthy(objects$omicsData$e_meta)
  is_pepdata <- inherits(objects$omicsData, "pepData") | inherits(objects$omicsData_pre_rollup, 'pepData')
  
  #' First disable the peptide-specific tabs if we don't have e-meta or we are
  #' not analyzing pepdata
  cond = !emeta_exists | !is_pepdata
  
  toggleTooltip(
    session, 
    tooltip_text = ttext_[['TABDISABLE_NOT_PEP']], 
    condition = !is_pepdata,
    selector = ".nav li a[data-value=peptide_statistics_tab]"
  )
  
  toggleTooltip(
    session, 
    tooltip_text = ttext_[['TABDISABLE_NOT_PEP_NO_EMETA']], 
    condition = cond,
    selector = ".nav li a[data-value=protein_rollup_tab]"
  )
  
  
  toggleTab("peptide_statistics_tab", condition = is_pepdata)
  toggleTab("protein_rollup_tab", condition = !cond)
  

  #' Disable regular statistics if there is not e_meta, since we expect this tab
  #' to be run on rolled up protein data and we cant do that without e_meta.
  cond2 = !emeta_exists & is_pepdata
  toggleTooltip(
    session, 
    tooltip_text = ttext_[['TABDISABLE_PEP_NO_EMETA']], 
    condition = cond2,
    selector = ".nav li a[data-value=statistics_tab]"
  )
  
  toggleTab("statistics_tab", condition = !cond2)

})