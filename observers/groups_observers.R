#'@details Applies pmartR::group_designation inplace to omicsData objects.
makegroup <- function(){
  
  req(!is.null(objects$omicsData) && !is.null(f_data()))
  
  ## Check two lipids requirements and process first
  if (two_lipids()) {
    req(!is.null(objects$omicsData_2) && !is.null(f_data_2()))
    
    if (input$usevizsampnames == "Yes") {
      if (input$customsampnames_opts == "first_n") {
        args_2 <- list(firstn = input$first_n_2)
      }
      else if (input$customsampnames_opts == "range") {
        args_2 <- list(from = input$range_low_2, to = input$range_high_2)
      }
      else if (input$customsampnames_opts == "split") {
        args_2 <- list(delim = input$delimiter_2, components = as.numeric(input$split_el_2))
      }
    }
    
    objects$omicsData_2 <- objects$uploaded_omicsData_2 <- tryCatch(
      {
        tmp <- objects$omicsData_2
        tmp$f_data <- f_data_2()
        attr(tmp, "cnames")$fdata_cname <- input$fdata_id_col_2
        
        cov_type = c(input$cv_type_1_2, input$cv_type_2_2)
        
        pair_id = if(isTRUE(input$pair_id_col_2 == "None") | !isTruthy(input$pair_id_col_2)) NULL else input$pair_id_col_2
        pair_group = if(isTRUE(input$pair_group_col_2 == "None") | !isTruthy(input$pair_group_col_2)) NULL else input$pair_group_col_2
        pair_denom = if(isTRUE(input$pair_denom_col_2 == "None") | !isTruthy(input$pair_denom_col_2)) NULL else input$pair_denom_col_2
        
        tmp <- group_designation(tmp,
                                 main_effects = main_effects_2(),
                                 covariates = covariates_2(),
                                 cov_type = cov_type,
                                 pair_id = pair_id,
                                 pair_group = pair_group,
                                 pair_denom = pair_denom
        )
        
        if (input$usevizsampnames == "Yes") {
          tmp <- do.call(custom_sampnames, args = c(omicsData = list(tmp), args_2))
        }
        
        tmp
      },
      error = function(e) {
        msg <- paste0("Something went wrong grouping your second omicsData object \n System error:  ", e)
        message(msg)
        revals$warnings_groups$obj_2 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        tmp <- objects$omicsData_2
        attr(tmp, "group_DF") <- NULL
        tmp
      }
    )
  }
  
  ## Process omicsData normally
  if (input$usevizsampnames == "Yes") {
    if (input$customsampnames_opts == "first_n") {
      args <- list(firstn = input$first_n)
    }
    else if (input$customsampnames_opts == "range") {
      args <- list(from = input$range_low, to = input$range_high)
    }
    else if (input$customsampnames_opts == "split") {
      args <- list(delim = input$delimiter, components = as.numeric(input$split_el))
    }
  }
  
  # replace fdata and fdata cname, then check data integrity
  objects$omicsData <- objects$uploaded_omicsData <- tryCatch(
    {
      
      tmp <- objects$omicsData
      ## as it turns out, having an f_data with samples not in e_data is problematic
      ref_catch <- f_data()[[input$fdata_id_col]] %in% colnames(objects$omicsData$e_data)
      tmp$f_data <- f_data()[ref_catch,]
      attr(tmp, "cnames")$fdata_cname <- input$fdata_id_col
      
      cov_type = c(input$cv_type_1, input$cv_type_2)
      
      pair_id = if(isTRUE(input$pair_id_col == "None") | !isTruthy(input$pair_id_col)) NULL else input$pair_id_col
      pair_group = if(isTRUE(input$pair_group_col == "None") | !isTruthy(input$pair_group_col)) NULL else input$pair_group_col
      pair_denom = if(isTRUE(input$pair_denom_col == "None") | !isTruthy(input$pair_denom_col)) NULL else input$pair_denom_col
      
      tmp <- group_designation(tmp,
                               main_effects = main_effects(),
                               covariates = covariates(),
                               cov_type = cov_type,
                               pair_id = pair_id,
                               pair_group = pair_group,
                               pair_denom = pair_denom
      )
      
      if (input$usevizsampnames == "Yes") {
        tmp <- do.call(custom_sampnames, args = c(omicsData = list(tmp), args))
      }
      
      tmp
    },
    error = function(e) {
      msg <- paste0("Something went wrong grouping your omicsData object \n System error:  ", e)
      message(msg)
      revals$warnings_groups$obj_1 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      tmp <- objects$omicsData
      attr(tmp, "group_DF") <- NULL
      tmp
    }
  )
}

# toggle fdata id col select if f_data() exists
observe({
  req(f_data())
  toggleElement("js_fdata_id_col", condition = !is.null(f_data()))
})

observeEvent(c(input$file_fdata, input$file_fdata_2), {
  Sys.sleep(0.7)
  
  cond <- !is.null(input$file_fdata) && 
    (!two_lipids() || !is.null(input$file_fdata_2))

  if (cond) {
    updateCollapse(session, "groups_collapse_left", open = c("fdata_columns"))
    updateCollapse(session, "groups_collapse_right", open = c("fdata_preview"))
  }
  else {
    updateCollapse(session, "groups_collapse_left", open = c("fdata_upload"), close = c("fdata_columns"))
  }

  toggle("ok_fdata_upload", condition = cond)
})

# apply group designation
observeEvent(input$group_designation, {
  makegroup()

  cond1 <- is.null(attributes(objects$omicsData)$group_DF)
  cond2 <- !is.null(objects$omicsData_2) & is.null(attributes(objects$omicsData_2)$group_DF)


  if (!cond1 & !cond2) {
    updateCollapse(session, "groups_collapse_left", close = c("fdata_upload", "fdata_columns"))
    shinyjs::show("ok_fdata_idcols")
    shinyjs::show("grouped_data_summary")

    revals$warnings_groups$failed_groupdes <- NULL
    revals$warnings_groups$obj_1 <- NULL
    revals$warnings_groups$obj_2 <- NULL

    if(inherits(objects$omicsData, "nmrData") ||
       (inherits(objects$omicsData, "pepData") &&
        input$labeled_yn == "iso")){
      usebutton <- actionButton("goto_reference", "Continue to Reference Tab", style = "margin:5px;width:75%")
    } else {
      usebutton <- div(actionButton("goto_qc", "Continue to Data Summary Tab", style = "margin:5px;width:75%"),
                       actionButton("goto_filter", "Continue to Filter Tab", style = "margin:5px;width:75%"))
    }

    # if grouping structure is created, show success modal
    showModal(
      modalDialog(
        title = "Grouping Success",
        fluidRow(
          column(10,
            align = "center", offset = 1,
            HTML('<h4 style= "color:#1A5276">Your data has been successfully grouped.
                      Future comparisons will be made across these groups.</h4>'),
            hr(),
            actionButton("groups_dismiss", "Review results", width = "75%"),
            usebutton
            # actionButton("goto_qc", "Continue to Data Summary Tab", style = "margin:5px;width:75%")
          )
        ),
        footer = NULL
      )
    )

    revals$groups_summary <- summary(objects$omicsData)
    revals$groups_summary_2 <- if (two_lipids()) summary(objects$omicsData_2) else NULL
  }
  else {
    revals$warnings_groups$failed_groupdes <- "<p style = 'color:grey'>Something went wrong grouping your objects$omicsData object(s), please verify all fields are correct.</p>"
    revals$groups_summary <- NULL
    revals$groups_summary_2 <- NULL
  }
  
  enable("group_reset")
})

observeEvent(input$group_reset, {
  req(!is.null(objects$omicsData) && input$group_reset > 0)
  makeobject(use_iso = F)

  updateCollapse(session, "groups_collapse_left", close = c("fdata_upload"), open = "fdata_columns")
  updateCollapse(session, "groups_collapse_right", close = c("fdata_plots"), open = "fdata_preview")
  
  shinyjs::hide("ok_fdata_idcols")
  shinyjs::hide("grouped_data_summary")
  disable("group_reset")
})

# dismiss modal and open plot collapsepanel
observeEvent(input$groups_dismiss, {
  updateCollapse(session, "groups_collapse_right", open = "fdata_plots")
  removeModal()
})

# move tabs and dismiss modal
observeEvent(input$goto_qc, {
  updateTabsetPanel(session, "top_page", selected = "data_summary_tab")
  removeModal()
})

# dont allow download unless we have uploaded a data file with sample names
observeEvent(sample_names(), {
  toggleState("download_fdata", condition = !is.null(sample_names()))
})

# error checking for groups tab
observe({
  # 2 file lipid conditions:
  # two fdata files uploaded with correct id col
  # there is at 1 main effect, and all specified main effects and covariates exist in both datasets
  fdata_idcol1 <- if (length(input$fdata_id_col) == 0) "__NULLSELECT__" else input$fdata_id_col
  fdata_idcol2 <- if (length(input$fdata_id_col_2) == 0) "__NULLSELECT__" else input$fdata_id_col_2

  req(!is.null(f_data()))
  
  if (two_lipids()) {
    cond_files <- !is.null(input$file_fdata) & !is.null(input$file_fdata_2)
    cond_idcol_fdata <- all(isTRUE(input$fdata_id_col %in% colnames(f_data())), isTRUE(input$fdata_id_col_2 %in% colnames(f_data_2())))
    cond_sample_names <- all(f_data()[[fdata_idcol1]] %in% sample_names(), f_data_2()[[fdata_idcol2]] %in% sample_names_2())
    cond_pairs <- all(isTRUE(input$pair_col %in% colnames(f_data())), isTRUE(input$pair_col_2 %in% colnames(f_data_2())))
    
    cond_main_effects <- all(c(length(main_effects()), length(main_effects_2())) != 0, main_effects() %in% colnames(f_data()), main_effects_2() %in% colnames(f_data_2()))
    cond_main_effects <- cond_main_effects | cond_pairs # main effect can be left blank if pairing present
    
    cond_covariates <- if (all(c(length(covariates()), length(covariates_2())) == 0)) TRUE else all(covariates() %in% colnames(f_data()), covariates_2() %in% colnames(f_data_2()))
    cond_NA_groups <- any(is.na(f_data()[main_effects()])) || any(is.na(f_data_2()[main_effects()]))
    
    cond_pairs1 <- if (isTruthy(input$pair_id_col != "None")) {
      isTruthy(input$pair_group_col != "None") & isTruthy(input$pair_denom_col != "None")
    } else {
      TRUE
    }
    
    cond_pairs2 <- if (isTruthy(input$pair_id_col_2 != "None")) {
      isTruthy(input$pair_group_col_2 != "None") & isTruthy(input$pair_denom_col_2 != "None")
    } else {
      TRUE
    }
    
    cond_pairs <- cond_pairs1 & cond_pairs2
    
    cond <- all(cond_files, cond_idcol_fdata, cond_main_effects, cond_covariates, cond_sample_names, cond_pairs)
  }
  # otherwise check:
  # input column exists in fdata
  # there is at 1 main effect, and all specified main effects and covariates exist in f_data
  else {
    cond_files <- !is.null(input$file_fdata)
    cond_idcol_fdata <- isTRUE(input$fdata_id_col %in% colnames(f_data()))
    cond_sample_names <- all(f_data()[[fdata_idcol1]] %in% sample_names())
    cond_pairs <- isTRUE(input$pair_col %in% colnames(f_data()))
    
    cond_main_effects <- (length(main_effects()) != 0) & all(main_effects() %in% colnames(f_data()))
    cond_main_effects <- cond_main_effects | cond_pairs # main effect can be left blank if pairing present
    
    cond_covariates <- if (length(covariates() == 0)) TRUE else all(covariates() %in% colnames(f_data()))
    cond_NA_groups <- any(is.na(f_data()[main_effects()]))
    cond_iso_nrm <- inherits(objects$omicsData, "pepData") && 
                        input$labeled_yn == "iso"
    
    cond_pairs <- if (isTruthy(input$pair_id_col != "None")) {
      isTruthy(input$pair_group_col != "None") & isTruthy(input$pair_denom_col != "None")
    } else {
      TRUE
    }

    cond <- all(cond_files, cond_idcol_fdata, cond_main_effects, cond_covariates, cond_sample_names, cond_pairs)
  }

  revals$warnings_groups$files <- if (!cond_files) "<p style = 'color:grey'>No f_data uploaded or one file missing.</p>" else NULL
  revals$warnings_groups$sample_names <- if (!cond_sample_names) "<p style = 'color:grey'>The chosen sample ID columns do not contain the sample names for one or more files</p>" else NULL
  revals$warnings_groups$idcol_fdata <- if (!cond_idcol_fdata) "<p style = 'color:grey'>Selected ID columns were not found in one or more grouping files.</p>" else NULL
  revals$warnings_groups$main_effects <- if (!cond_main_effects) "<p style = 'color:grey'>No main effect specified or not found in one or more grouping files.</p>" else NULL
  revals$warnings_groups$covariates <- if (!cond_covariates) "<p style = 'color:grey'>Specified covariates not found in one or more grouping files.</p>" else NULL
  revals$warnings_groups$NA_groups <- if(cond_NA_groups) "<p style = 'color:grey'>Specified main effect(s) are not assigned for all samples; samples with missing main effect(s) will be removed. </p>" else NULL
  revals$warnings_groups$reference <- if(cond_NA_groups && cond_iso_nrm) "<p style = 'color:grey'>Note: Reference samples without assigned main effect(s) will still be available for downstream reference normalization.</p>" else NULL
  revals$warnings_groups$pairs <- if(!cond_pairs) "<p style = 'color:grey'>Please enter all pairing information.</p>" else NULL
  
  groups_not_applied <- is.null(attributes(objects$omicsData)$group_DF)
  
  toggleState("group_designation", condition = cond && groups_not_applied)
})
