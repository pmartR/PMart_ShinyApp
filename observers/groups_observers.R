# toggle fdata id col select if f_data() exists
observe({
  req(f_data())
  toggleElement("js_fdata_id_col", condition = !is.null(f_data()))
})

observeEvent(c(input$file_fdata, input$file_fdata_2), {
  Sys.sleep(0.7)
  if (two_lipids()) {
    cond <- !is.null(input$file_fdata) & !is.null(input$file_fdata_2)
  }
  else {
    cond <- !is.null(input$file_fdata)
  }

  if (cond) {
    updateCollapse(session, "groups_collapse_left", open = c("fdata_columns"))
    updateCollapse(session, "groups_collapse_right", open = c("fdata_preview"))
  }
  else {
    updateCollapse(session, "groups_collapse_left", open = c("fdata_upload"), close = c("fdata_columns"))
  }

  toggle("ok_fdata_upload", condition = cond)
})

# create 4 observers which maintain mutual exclusivity of main effects and covariates
lapply(list("gcol1", "gcol2", "cvcol1", "cvcol2"), function(el) {
  observeEvent(sapply(list("gcol1", "gcol2", "cvcol1", "cvcol2")[-which(list("gcol1", "gcol2", "cvcol1", "cvcol2") == el)], function(x) input[[x]]), {
    req(input$fdata_id_col, f_data())

    revals[[el]] <- input[[el]]
    updateSelectInput(session, el,
      choices = c("None", setdiff(
        colnames(f_data()),
        c(input$fdata_id_col, sapply(list("gcol1", "gcol2", "cvcol1", "cvcol2")[-which(list("gcol1", "gcol2", "cvcol1", "cvcol2") == el)], function(x) input[[x]]))
      )),
      selected = input[[el]]
    )
  })
})

# similarly for the case of two files
lapply(list("gcol1_2", "gcol2_2", "cvcol1_2", "cvcol2_2"), function(el) {
  observeEvent(sapply(list("gcol1_2", "gcol2_2", "cvcol1_2", "cvcol2_2")[-which(list("gcol1_2", "gcol2_2", "cvcol1_2", "cvcol2_2") == el)], function(x) input[[x]]), {
    req(input$fdata_id_col_2, f_data_2())

    revals[[el]] <- input[[el]]
    updateSelectInput(session, el,
      choices = c("None", setdiff(
        colnames(f_data_2()),
        c(input$fdata_id_col_2, sapply(list("gcol1_2", "gcol2_2", "cvcol1_2", "cvcol2_2")[-which(list("gcol1_2", "gcol2_2", "cvcol1_2", "cvcol2_2") == el)], function(x) input[[x]]))
      )),
      selected = input[[el]]
    )
  })
})

# apply group designation
observeEvent(input$group_designation, {
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

  if (two_lipids()) {
    req(objects$uploaded_omicsData, objects$uploaded_omicsData_2, f_data(), f_data_2())

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

    # replace fdata and fdata cname, then check data integrity
    objects$uploaded_omicsData$f_data <- f_data()
    objects$uploaded_omicsData_2$f_data <- f_data_2()
    attr(objects$uploaded_omicsData, "cnames")$fdata_cname <- input$fdata_id_col
    attr(objects$uploaded_omicsData_2, "cnames")$fdata_cname <- input$fdata_id_col_2
    req(pmartR:::verify_data_info(objects$uploaded_omicsData), pmartR:::verify_data_info(objects$uploaded_omicsData_2))

    objects$omicsData <- objects$uploaded_omicsData <- tryCatch(
      {
        tmp <- group_designation(objects$uploaded_omicsData,
          main_effects = main_effects(),
          covariates = covariates()
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

    objects$omicsData_2 <- objects$uploaded_omicsData_2 <- tryCatch(
      {
        tmp <- group_designation(objects$uploaded_omicsData_2,
          main_effects = main_effects_2(),
          covariates = covariates_2()
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
        tmp <- objects$uploaded_omicsData_2
        attr(tmp, "group_DF") <- NULL
        tmp
      }
    )
  }
  else {
    req(objects$uploaded_omicsData, f_data())

    objects$uploaded_omicsData$f_data <- f_data()
    attr(objects$uploaded_omicsData, "cnames")$fdata_cname <- input$fdata_id_col
    req(pmartR:::verify_data_info(objects$uploaded_omicsData))

    objects$omicsData <- objects$uploaded_omicsData <- tryCatch(
      {
        tmp <- group_designation(objects$uploaded_omicsData,
          main_effects = main_effects(),
          covariates = covariates()
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
        tmp <- objects$uploaded_omicsData
        attr(tmp, "group_DF") <- NULL
        tmp
      }
    )
  }

  cond1 <- is.null(attributes(objects$uploaded_omicsData)$group_DF)
  cond2 <- !is.null(objects$uploaded_omicsData_2) & is.null(attributes(objects$uploaded_omicsData_2)$group_DF)


  if (!cond1 & !cond2) {
    updateCollapse(session, "groups_collapse_left", close = c("fdata_upload", "fdata_columns"))
    shinyjs::show("ok_fdata_idcols")

    revals$warnings_groups$failed_groupdes <- NULL
    revals$warnings_groups$obj_1 <- NULL
    revals$warnings_groups$obj_2 <- NULL

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
            actionButton("goto_qc", "Continue to Data Summary Tab", style = "margin:5px;width:75%")
          )
        ),
        footer = NULL
      )
    )

    revals$groups_summary <- summary(objects$uploaded_omicsData)
    revals$groups_summary_2 <- if (two_lipids()) summary(objects$uploaded_omicsData_2) else NULL
  }
  else {
    revals$warnings_groups$failed_groupdes <- "<p style = 'color:grey'>Something went wrong grouping your objects$omicsData object(s), please verify all fields are correct.</p>"
    revals$groups_summary <- NULL
    revals$groups_summary_2 <- NULL
  }
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

  if (two_lipids()) {
    cond_files <- !is.null(input$file_fdata) & !is.null(input$file_fdata_2)
    cond_idcol_fdata <- all(isTRUE(input$fdata_id_col %in% colnames(f_data())), isTRUE(input$fdata_id_col_2 %in% colnames(f_data_2())))
    cond_sample_names <- all(f_data()[[fdata_idcol1]] %in% sample_names(), f_data_2()[[fdata_idcol2]] %in% sample_names_2())
    cond_main_effects <- all(c(length(main_effects()), length(main_effects_2())) != 0, main_effects() %in% colnames(f_data()), main_effects_2() %in% colnames(f_data_2()))
    cond_covariates <- if (all(c(length(covariates()), length(covariates_2())) == 0)) TRUE else all(covariates() %in% colnames(f_data()), covariates_2() %in% colnames(f_data_2()))

    cond <- all(cond_files, cond_idcol_fdata, cond_main_effects, cond_covariates, cond_sample_names)
  }
  # otherwise check:
  # input column exists in fdata
  # there is at 1 main effect, and all specified main effects and covariates exist in f_data
  else {
    cond_files <- !is.null(input$file_fdata)
    cond_idcol_fdata <- isTRUE(input$fdata_id_col %in% colnames(f_data()))
    cond_sample_names <- all(f_data()[[fdata_idcol1]] %in% sample_names())
    cond_main_effects <- (length(main_effects()) != 0) & all(main_effects() %in% colnames(f_data()))
    cond_covariates <- if (length(covariates() == 0)) TRUE else all(covariates() %in% colnames(f_data()))

    cond <- all(cond_files, cond_idcol_fdata, cond_main_effects, cond_covariates, cond_sample_names)
  }

  revals$warnings_groups$files <- if (!cond_files) "<p style = 'color:grey'>No f_data uploaded or one file missing.</p>" else NULL
  revals$warnings_groups$sample_names <- if (!cond_sample_names) "<p style = 'color:grey'>The chosen sample ID columns do not contain the sample names for one or more files</p>" else NULL
  revals$warnings_groups$idcol_fdata <- if (!cond_idcol_fdata) "<p style = 'color:grey'>Selected ID columns were not found in one or more grouping files.</p>" else NULL
  revals$warnings_groups$main_effects <- if (!cond_main_effects) "<p style = 'color:grey'>No main effect specified or not found in one or more grouping files.</p>" else NULL
  revals$warnings_groups$covariates <- if (!cond_covariates) "<p style = 'color:grey'>Specified covariates not found in one or more grouping files.</p>" else NULL

  toggleState("group_designation", condition = cond)
})
