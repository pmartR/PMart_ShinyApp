#'@details disable peptide stats and rollup if e_meta does not exist
observe({
  emeta_exists <- isTruthy(revals$e_meta) | isTruthy(objects$omicsData$e_meta)
  is_pepdata <- inherits(objects$omicsData, "pepData") | inherits(objects$omicsData_pre_rollup, 'pepData')
  
  cond = !emeta_exists | !is_pepdata
  
  toggleTooltip(
    session, 
    tooltip_text = ttext_[['NOT_PEP_NO_EMETA']], 
    condition = cond,
    selector = ".nav li a[data-value=peptide_statistics_tab]"
  )
  
  toggleTooltip(
    session, 
    tooltip_text = ttext_[['NOT_PEP_NO_EMETA']], 
    condition = cond,
    selector = ".nav li a[data-value=protein_rollup_tab]"
  )
  
  if(!cond) {
    js$enableTab(name = 'peptide_statistics_tab')
    js$enableTab(name = 'protein_rollup_tab')
  } else {
    js$disableTab(name = 'peptide_statistics_tab') 
    js$disableTab(name = 'protein_rollup_tab')
  }
})