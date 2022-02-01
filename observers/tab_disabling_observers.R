#'@details disable peptide stats and rollup if e_meta does not exist or if we
#'are not working with pepData.  We will still leave the tabs enabled if we have
#'rolled up to proData, but for review only.
observe({
  emeta_exists <- isTruthy(revals$e_meta) | isTruthy(objects$omicsData$e_meta)
  is_pepdata <- inherits(objects$omicsData, "pepData") | inherits(objects$omicsData_pre_rollup, 'pepData')
  
  #' First disable the peptide-specific tabs if we don't have e-meta or we are
  #' not analyzing pepdata
  cond = !emeta_exists | !is_pepdata
  
  cond_ref <- (is_pepdata && input$labeled_yn == "iso") || inherits(objects$omicsData, "nmrData")
  
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
  
  toggleTooltip(
    session, 
    tooltip_text = ttext_[['TABDISABLE_NOT_REF']], 
    condition = !cond_ref,
    selector = ".nav li a[data-value=reference_tab]"
  )
  
  toggleTab("peptide_statistics_tab", condition = is_pepdata)
  toggleTab("protein_rollup_tab", condition = !cond)
  toggleTab("reference_tab", condition = cond_ref)
  

  #' Disable regular statistics if there is not e_meta, since we expect this tab
  #' to be run on rolled up protein data and we cant do that without e_meta.
  #' ALso disable if simply have not yet rolled up (data is still peptide level).
  cond2 = !emeta_exists & is_pepdata
  cond3 = inherits(objects$omicsData, "pepData")
  
  tooltip_text = if(cond2) {
    ttext_[['TABDISABLE_PEP_NO_EMETA']]
  } else if(cond3) {
    ttext_[['TABDISABLE_PEP_NOT_ROLLED_UP']]
  }
  
  toggleTooltip(
    session, 
    tooltip_text = tooltip_text, 
    condition = cond2 | cond3,
    selector = ".nav li a[data-value=statistics_tab]"
  )
  
  toggleTab("statistics_tab", condition = !cond2 & !cond3)
})
