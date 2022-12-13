#'@details Applies pmartR::group_designation inplace to omicsData objects.
makegroup <- function(){
  
  req(!is.null(objects$omicsData) && !is.null(f_data()))
  
  ## arguments for custom_sampnames
  if (input$usevizsampnames == "Yes") {
    if (input$customsampnames_opts == "first_n") {
      sampname_args <- list(firstn = input$first_n)
    }
    else if (input$customsampnames_opts == "range") {
      sampname_args <- list(from = input$range_low, to = input$range_high)
    }
    else if (input$customsampnames_opts == "split") {
      sampname_args <- list(delim = input$delimiter, components = as.numeric(input$split_el))
    }
  }
  
  revals$warnings_groups$obj_2 <- revals$warnings_groups$obj_1 <- NULL
  
  cov_type = c(input$cv_type_1, input$cv_type_2)
  
  pair_id = if(isTRUE(input$pair_id_col == "None") | !isTruthy(input$pair_id_col)) NULL else input$pair_id_col
  pair_group = if(isTRUE(input$pair_group_col == "None") | !isTruthy(input$pair_group_col)) NULL else input$pair_group_col
  pair_denom = if(isTRUE(input$pair_denom_col == "None") | !isTruthy(input$pair_denom_col)) NULL else input$pair_denom_col
  
  ## To process the data, we will replace fdata and fdata cname,
  ## call group_designation, and apply custom sampnames if specified.
  
  ## Check two lipids requirements and process second object first ...
  if (two_lipids()) {
    req(!is.null(objects$omicsData_2))
    
    .tmp_obj_2 <- tryCatch(
      {
        tmp <- objects$omicsData_2
        tmp$f_data <- f_data()
        attr(tmp, "cnames")$fdata_cname <- input$fdata_id_col_2
        
        tmp <- group_designation(tmp,
                                 main_effects = main_effects(),
                                 covariates = covariates(),
                                 cov_type = cov_type,
                                 pair_id = pair_id,
                                 pair_group = pair_group,
                                 pair_denom = pair_denom
        )

        if (input$usevizsampnames == "Yes") {
          tmp <- do.call(custom_sampnames, args = c(omicsData = list(tmp), sampname_args))
        }

        tmp
      },
      error = function(e) {
        msg <- paste0("Something went wrong grouping your second omicsData object \n System error:  ", e)
        message(msg)
        revals$warnings_groups$obj_2 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        NULL
      }
    )
    
    req(!is.null(.tmp_obj_2))
  }
  
  # ... then process the first object
  .tmp_obj <- tryCatch(
    {
      tmp <- objects$omicsData
      ## as it turns out, having an f_data with samples not in e_data is problematic
      ref_catch <- f_data()[[input$fdata_id_col]] %in% colnames(objects$omicsData$e_data)
      tmp$f_data <- f_data()[ref_catch,]
      attr(tmp, "cnames")$fdata_cname <- input$fdata_id_col
      
      cov_type = c(input$cv_type_1, input$cv_type_2)
      
      tmp <- group_designation(tmp,
                               main_effects = main_effects(),
                               covariates = covariates(),
                               cov_type = cov_type,
                               pair_id = pair_id,
                               pair_group = pair_group,
                               pair_denom = pair_denom
      )
      
      if (input$usevizsampnames == "Yes") {
        tmp <- do.call(custom_sampnames, args = c(omicsData = list(tmp), sampname_args))
      }
      
      tmp
    },
    error = function(e) {
      msg <- paste0("Something went wrong grouping your omicsData object \n System error:  ", e)
      message(msg)
      revals$warnings_groups$obj_1 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      NULL
    }
  )
  
  req(!is.null(.tmp_obj))
  
  objects$omicsData <- .tmp_obj
  
  if(two_lipids()){
    objects$omicsData_2 <- .tmp_obj_2 
  }
}

# toggle fdata id col select if f_data() exists
observe({
  req(f_data())
  toggleElement("js_fdata_id_col", condition = !is.null(f_data()))
})

observeEvent(input$file_fdata, {
  Sys.sleep(0.7)
  
  cond <- !is.null(input$file_fdata)

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

#'@details Toggle the 'complete' checkmark for pairing structure collapsepanel.
observeEvent(c(
  input$pair_id_col,
  input$pair_group_col,
  input$pair_denom_col
  ), {

  toggle("ok_fdata_pair_cols", condition = pairs_complete()[['valid']])
})

# error checking for groups tab
observe({
  fdata_idcol <- if (length(input$fdata_id_col) == 0) "__NULLSELECT__" else input$fdata_id_col

  req(!is.null(f_data()))
  
  # input column exists in fdata
  # there is at 1 main effect, and all specified main effects and covariates exist in f_data
  cond_files <- !is.null(input$file_fdata)
  cond_idcol_fdata <- isTRUE(input$fdata_id_col %in% colnames(f_data()))
  cond_sample_names <- all(f_data()[[fdata_idcol]] %in% sample_names())
  
  cond_main_effects <- (length(main_effects()) != 0) & all(main_effects() %in% colnames(f_data()))
  cond_main_effects <- cond_main_effects | pairs_complete()[["valid"]] # main effect can be left blank if pairing present
  
  cond_covariates <- if (length(covariates() == 0)) TRUE else all(covariates() %in% colnames(f_data()))
  cond_NA_groups <- any(is.na(f_data()[main_effects()]))
  cond_iso_nrm <- inherits(objects$omicsData, "pepData") && 
                      input$labeled_yn == "iso"
  
  cond <- all(cond_files, cond_idcol_fdata, cond_main_effects, cond_covariates, cond_sample_names, pairs_complete()[['pass']])

  revals$warnings_groups$files <- if (!cond_files) "<p style = 'color:grey'>No f_data uploaded or one file missing.</p>" else NULL
  revals$warnings_groups$sample_names <- if (!cond_sample_names) "<p style = 'color:grey'>The chosen sample ID columns do not contain the sample names for one or more files</p>" else NULL
  revals$warnings_groups$idcol_fdata <- if (!cond_idcol_fdata) "<p style = 'color:grey'>Selected ID columns were not found in one or more grouping files.</p>" else NULL
  revals$warnings_groups$main_effects <- if (!cond_main_effects) "<p style = 'color:grey'>No main effect or pairing structure specified or main effects not found in one or more grouping files.</p>" else NULL
  revals$warnings_groups$covariates <- if (!cond_covariates) "<p style = 'color:grey'>Specified covariates not found in one or more grouping files.</p>" else NULL
  revals$warnings_groups$NA_groups <- if(cond_NA_groups) "<p style = 'color:grey'>Specified main effect(s) are not assigned for all samples; samples with missing main effect(s) will be removed. </p>" else NULL
  revals$warnings_groups$reference <- if(cond_NA_groups && cond_iso_nrm) "<p style = 'color:grey'>Note: Reference samples without assigned main effect(s) will still be available for downstream reference normalization.</p>" else NULL
  revals$warnings_groups$pairs <- if(!pairs_complete()[['pass']]) "<p style = 'color:grey'>Please enter all pairing information.</p>" else NULL
  
  groups_not_applied <- is.null(attributes(objects$omicsData)$group_DF)
  
  toggleState("group_designation", condition = cond && groups_not_applied)
})
