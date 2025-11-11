#'@details disable peptide stats, reference and rollup if e_meta does not exist or if we
#'are not working with pepData.  We will still leave the tabs enabled if we have
#'rolled up to proData, but for review only.
observe({
  #browser()
  emeta_exists <- isTruthy(revals$e_meta) | isTruthy(objects$omicsData$e_meta)
  is_pepdata <- inherits(objects$omicsData, "pepData") | inherits(objects$omicsData_pre_rollup, 'pepData')
  omicsdata_exists <- !is.null(objects$omicsData)
  
  #' First disable the peptide-specific tabs if we don't have e-meta or we are
  #' not analyzing pepdata or we 
  #' have not normalized/batch corrected the data
  cond = !emeta_exists | !is_pepdata
  
  cond_ref <- (is_pepdata && input$labeled_yn == "iso") || inherits(objects$omicsData, "nmrData")
  
  cond_seq <- inherits(objects$omicsData, "seqData") || is.null(objects$omicsData)
  
  cond_norm <- FALSE
  if(!is.null(attributes(objects$omicsData)$data_info$norm_info$is_normalized)){
    cond_norm <- attributes(objects$omicsData)$data_info$norm_info$is_normalized
    # set this to be FALSE again if we are working with ComBat 
    if(!is.null(input$batch_correction_id) && input$batch_correction_id == "ComBat"){
      cond_norm <- FALSE
    }
  }
  cond_bc <- FALSE
  if(!is.null(attributes(objects$omicsData)$data_info$batch_info$is_bc)){
    cond_norm <- attributes(objects$omicsData)$data_info$batch_info$is_bc | cond_norm
  }
  
  toggleTooltip(
    session, 
    tooltip_text = ttext_[['TABDISABLE_NOT_PEP']], 
    condition = (!is_pepdata & cond_norm),
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
  
  toggleTooltip(
    session, 
    tooltip_text = ttext_[['TABDISABLE_SEQDATA']], 
    condition = cond_seq,
    selector = ".nav li a[data-value=normalization_tab]"
  )
  
  toggleTab("peptide_statistics_tab", condition = is_pepdata & cond_norm)
  toggleTab("protein_rollup_tab", condition = !cond)
  toggleTab("reference_tab", condition = cond_ref)
  toggleTab("normalization_tab", condition = !cond_seq)
  

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
    condition = ((cond2 | cond3) & cond_norm),
    selector = ".nav li a[data-value=statistics_tab]"
  )
  
  toggleTab("statistics_tab", condition = !cond2 & !cond3 & cond_norm & omicsdata_exists)
})

#'@details Disable all tabs until omicsData object exists
observe({
  # Midpoints will have enabled necessary tabs
  if(MAP_ACTIVE){
    req(exists("MapConnect"))
    req(is.null(MapConnect$Midpoint)) 
  }
  
  to_disable_ids <- setdiff(
    TAB_IDS,
    c(
      "upload_and_datareqs",
      "reference_tab",
      "protein_rollup_tab",
      "peptide_statistics_tab"
    )
  )
  
  for (dtab in to_disable_ids) {
    toggleTooltip(
      session, 
      tooltip_text = "Upload your omicsdata", 
      condition = is.null(objects$omicsData),
      selector = sprintf(".nav li a[data-value=%s]", dtab)
    )
    
    toggleTab(dtab, condition = !is.null(objects$omicsData))
  } 
  
}, priority = 10)