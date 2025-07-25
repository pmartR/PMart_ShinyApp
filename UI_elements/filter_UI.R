#'@details Input for cv filter cv_threshold.  Inside a reactive UI so that the
#'maximum CV can be displayed depending on the data.
output$cv_threshold_UI <- renderUI({
  req(objects$omicsData)
  tmp_cvfilt <- cv_filter(objects$omicsData)
  max_cv = max(tmp_cvfilt$CV, na.rm=T)
  
  if(!is.null(objects$omicsData_2)) {
    tmp_cvfilt <- cv_filter(objects$omicsData_2)
    max_cv <- min(max_cv, max(tmp_cvfilt$CV, na.rm=T))
  }
  
  title = sprintf("Maximum CV (between 1 and %s)", round(max_cv, 2))
  
  numericInput("cv_threshold", title, min = 1, max = max_cv, value = round(max_cv*0.9, 2), step = 1)
})

# Summary of current filters and parameters
output$filter_review <- renderUI({
  # store text as a list of HTML elements
  divs <- list()

  fdata_cname <- attributes(objects$omicsData)$cnames$fdata_cname
  fdata_cname_2 <- attributes(objects$omicsData_2)$cnames$fdata_cname

  # indices of which filters/HTML elements belong to each object
  obj2_inds <- which(grepl("_2$", names(objects$filters)))
  obj1_inds <- setdiff(1:length(objects$filters), obj2_inds)
  
  if(length(objects$filters) == 0) return(
    div(
      br(),
      strong("No filters will be applied"),
      br(),
      br()
    )
  )
  
  #' instantiate this outside of the for loop, since it is used in multiple
  #' TODO: collect all samples and perform differences at the end?
  rmd_removed_samps <- if (any(grepl("rmdfilt", names(objects$filters)))) {
    tmp_idx = which(grepl("rmdfilt", names(objects$filters)))
    
    collect_samps_rmv <- NULL
    
    # loop for two lipids case
    for (idx in tmp_idx) {
      collect_samps_rmv <- c(collect_samps_rmv, attr(objects$filters[[idx]], "sample_names")[which(objects$filters[[idx]]$pvalue < input$pvalue_threshold)])
    }
    
    unique(collect_samps_rmv)
    
  } else NULL

  rmd_removed_samps <- if (!is.null(objects$filters[['rmdfilt']])) {
    summary(objects$filters[['rmdfilt']], pvalue_threshold = input$pvalue_threshold)$filtered_samples
  } else NULL

  rmd_removed_samps_2 <- if (!is.null(objects$filters[['rmdfilt_2']])) {
    summary(objects$filters[['rmdfilt_2']], pvalue_threshold = input$pvalue_threshold)$filtered_samples
  } else NULL
  
  rnafilt_libsize_removed_samps <- if(any(grepl("rnafilt_libsize", names(objects$filters)))) {
    size_library <- if(isTruthy(input$rnafilt_min_lib_size)) input$rnafilt_min_lib_size else NULL
    tmp_idx = which(grepl("rnafilt_libsize", names(objects$filters))) 
    tmp_summ <- summary(objects$filters[[tmp_idx]], size_library=size_library)$samples_filtered 
  } else NULL
  
  rnafilt_min_nonzero_removed_samps <- if(any(grepl("rnafilt_min_nonzero", names(objects$filters)))) {
    min_nonzero <- if(isTruthy(input$rnafilt_min_nonzero)) input$rnafilt_min_nonzero else NULL
    tmp_idx = which(grepl("rnafilt_min_nonzero", names(objects$filters))) 
    tmp_summ <- summary(objects$filters[[tmp_idx]], min_nonzero=min_nonzero)$samples_filtered 
  } else NULL
  
  # text for first filter object
  for (i in 1:length(objects$filters)) {
    # total count filter
    if (grepl("^tcfilt$", names(objects$filters)[i])) {
      tcfilt_summ <- summary(objects$filters[[i]], min_count = input$min_num_trans)
      divs[[i]] <- tagList(
        tags$b("Total Count Filter:"),
        tags$p(
          sprintf("Minimum Count: %s", input$min_num_trans),
          sprintf("Transcripts Removed: %s", tcfilt_summ$filtered_biomolecules)
        ),
        hr()
      )
    }
    # rna filter (library size) 
    else if (grepl("rnafilt_libsize", names(objects$filters)[i])) {
      divs[[i]] <- tagList(
        tags$b("Library Size Filter:"),
        tags$p(sprintf("Library Size Threshold: %s", input$rnafilt_min_libsize)),
        tags$p(sprintf("Removed Samples: %s", ifelse(length(rnafilt_libsize_removed_samps > 0), paste(rnafilt_libsize_removed_samps, collapse = " | "), "None"))),
        hr()
      )
    }
    
    # rna filter (min nonzero) 
    else if (grepl("rnafilt_min_nonzero", names(objects$filters)[i])) {
      divs[[i]] <- tagList(
        tags$b("Non-Zero Filter:"),
        tags$p(sprintf("Minimum Nonzero Count Threshold: %s", input$rnafilt_min_nonzero)),
        tags$p(sprintf("Removed Samples: %s", ifelse(length(rnafilt_min_nonzero_removed_samps > 0), paste(rnafilt_min_nonzero_removed_samps, collapse = " | "), "None"))),
        hr()
      )
    }
    
    # rmd filter
    else if (grepl("rmdfilt", names(objects$filters)[i])) {
      .to_rmv = summary(objects$filters[[i]], pvalue_threshold = input$pvalue_threshold)$filtered_samples
      .cond = is.null(.to_rmv) || .to_rmv == "NULL" # second condition is a bug in pmartR summary()
      divs[[i]] <- tagList(
        tags$b("rMd Filter:"),
        tags$p(sprintf("p-value threshold: %s", input$pvalue_threshold)),
        tags$p(sprintf("Removed Samples: %s", ifelse(!.cond, paste(.to_rmv, collapse = " | "), "None"))),
        hr()
      )
    }
    
    # custom filter
    else if (grepl("customfilt", names(objects$filters)[i])) {
      # get samples and check whether we are removing or keeping
      object_samples <- attr(objects$filters[[i]], "omicsData")$f_data[, get_fdata_cname(attr(objects$filters[[i]], "omicsData"))]
      
      if ("f_data_keep" %in% names(objects$filters[[i]])) {
        f_data_keep <- objects$filters[[i]][["f_data_keep"]]
        f_data_remove <- setdiff(object_samples, f_data_keep) %>% union(rmd_removed_samps) # ** include samples removed by rmd filter
      }
      else if ("f_data_remove" %in% names(objects$filters[[i]])) {
        f_data_remove <- objects$filters[[i]][["f_data_remove"]]
        f_data_keep <- setdiff(object_samples, union(f_data_remove, rmd_removed_samps)) # **
      }
      
      #
      e_data_remove = objects$filters[[i]]$e_data_remove
      e_meta_remove = objects$filters[[i]]$e_meta_remove
      
      #' construct divs containing info about what was removed by the custom filter
      e_data_remove_div <- if(length(e_data_remove) > 0) {
        tagList(tags$p(
          "Biomolecules Removed: ",
          div(style = "overflow-x:auto;max-height:150px", paste(e_data_remove, collapse = " | ")),
          div(sprintf("Total: %s", length(e_data_remove))),
          hr(),
        ))
      } else {
        NULL
      }
      
      f_data_remove_div <- if(length(f_data_remove) > 0) {
        tagList(
          tags$p("Samples Removed: ", div(style = "overflow-x:auto", paste(f_data_remove, collapse = " | "))),
          tags$p("Remaining Samples:", div(style = "overflow-x:auto", paste(f_data_keep, collapse = " | "))),
          hr()
        )
      } else {
        NULL
      }
      
      e_meta_remove_div <- if(length(e_meta_remove) > 0) {
        tagList(tags$p(
          "Biomolecules removed by association with extra biomolecule information:",
          div(style = "overflow-x:auto;max-height:150px", paste(e_meta_remove, collapse = " | ")),
          div(sprintf("Total: %s", length(e_meta_remove))),
          hr()
        ))
      } else {
        NULL
      }
      
      divs[[i]] <- tagList(
        tags$b("Custom Filter:"),
        f_data_remove_div,
        e_data_remove_div,
        e_meta_remove_div,
      )
      
    # cv filter
    } else if (grepl("cvfilt", names(objects$filters)[i])) {
      n_removed <- sum(objects$filters[[i]]$CV > input$cv_threshold, na.rm=T)
      divs[[i]] <- tagList(
        tags$b("Coefficient of Variation (CV) Filter:"),
        tags$p("CV threshold: ", input$cv_threshold, "| Biomolecules removed: ", n_removed),
        hr()
      )
      
    # imd filter
    } else if (grepl("imdanovafilt", names(objects$filters)[i])) {
      
      mng <- if(is.na(input$min_nonmiss_gtest)) NULL else input$min_nonmiss_gtest
      mna <- if(is.na(input$min_nonmiss_anova)) NULL else input$min_nonmiss_anova
      
      foo <- summary(objects$filters[[i]], 
                      min_nonmiss_anova = mna, 
                      min_nonmiss_gtest = mng
                      )
      divs[[i]] <- tagList(
        tags$b("iMd-ANOVA Filter:"),
        tags$p(
          "Minimum observed (G-test, ANOVA):", sprintf("(%s, %s)", input$min_nonmiss_anova, input$min_nonmiss_gtest),
          "| Biomolecules removed: ", foo$num_filtered
        ),
        hr()
      )
    }
    
    # molecule filter
    else if (grepl("molfilt", names(objects$filters)[i])) {
      foo <- summary(objects$filters[[i]], min_num = input$mol_min_num)
      divs[[i]] <- tagList(
        tags$b("Molecule Filter:"),
        tags$p(
          "Minimum observed:", input$mol_min_num,
          "| Biomolecules removed: ", foo$num_filtered
        ),
        hr()
      )
    }
    
    # proteomics filter
    else if (grepl("profilt", names(objects$filters)[i])) {
      foo <- summary(objects$filters[[i]], min_num_peps = input$min_num_peps, degen_peps = input$degen_peps)
      divs[[i]] <- tagList(
        tags$b("Proteomics Filter:"),
        tags$p("Minimum Peptides Mapping to a Protein:", input$min_num_peps, "| Remove degenerate peptides?: ", ifelse(input$degen_peps, "Yes", "No")),
        tags$p("Peptides removed: ", foo$num_pep_filtered, " | Proteins Removed: ", foo$num_pro_filtered),
        hr()
      )
    }
  }
  
  filtered_disclaimer <- if(length(attr(objects$omicsData, "filters")) > 0) {
    h3(tags$b("Filters have already been applied, these filters will be applied in addition to the existing filters.", style = "color:deepskyblue"))
  } else NULL
  
  # construct object size info divs
  n_edata_1 <- attributes(objects$omicsData)$data_info$num_edata
  n_fdata_1 <- attributes(objects$omicsData)$data_info$num_samps
  
  n_edata_2 <- attributes(objects$omicsData_2)$data_info$num_edata
  n_fdata_2 <- attributes(objects$omicsData_2)$data_info$num_samps
  
  n_edata_1_div <-
    h4(tags$b(
      sprintf(
        "Number of biomolecules in object 1 before filters:  %s",
        formatC(n_edata_1, format = "d", big.mark = ",")
      )
    ))
  n_fdata_1_div <-
    h4(tags$b(
      sprintf(
        "Number of samples in object 1 before filters:  %s",
        formatC(n_fdata_1, format = "d", big.mark = ",")
      )
    ))
  
  n_edata_2_div <- if (!is.null(n_edata_2))
    h4(tags$b(
      sprintf(
        "Number of biomolecules in object 2 before filters:  %s",
        formatC(n_edata_2, format = "d", big.mark = ",")
      )
    ))
  else
    NULL
  n_fdata_2_div <- if (!is.null(n_fdata_2))
    h4(tags$b(
      sprintf(
        "Number of samples in object 2 before filters:  %s",
        formatC(n_fdata_2, format = "d", big.mark = ",")
      )
    ))
  else
    NULL
  
  # Display two summary sections if there are two omicsData objects
  if (length(obj2_inds) > 0) {
    tagList(
      h3(tags$b("Samples removed in one object will be removed in the other as well.", style = "color:deepskyblue")),
      filtered_disclaimer,
      hr(),
      n_edata_1_div,
      n_fdata_1_div,
      n_edata_2_div,
      n_fdata_2_div,
      hr(),
      h3(tags$b(sprintf("Filters to be applied to %s:  ", omic_1_name()))),
      hr(),
      divs[obj1_inds],
      h3(tags$b(sprintf("Filters to be applied to %s:  ", omic_2_name()))),
      hr(),
      divs[obj2_inds]
    )
  }
  else {
    tagList(
      filtered_disclaimer,
      hr(),
      n_edata_1_div,
      n_fdata_1_div,
      hr(),
      h3(tags$b("Filters to be applied:  ")),
      divs
    )
  }
})

# Top filter plot
output$filter_mainplot <- renderPlotly({
  if (inherits(plots$filter_mainplot, "list")) {
    # p <- gridExtra::arrangeGrob(plots$filter_mainplot[[1]], plots$filter_mainplot[[2]], ncol = 2)
    p <- subplot(plots$filter_mainplot, shareY = T, titleX = T, titleY = T)
    plots$last_plot <- p
    grid::grid.draw(p)
  }
  else {
    plots$last_plot <- plots$filter_mainplot
    return(plots$filter_mainplot)
  }
})

# other filter plot for lipid data
output$filter_mainplot_2 <- renderPlotly({
  if (inherits(plots$filter_mainplot_2, "list")) {

    p <- subplot(plots$filter_mainplot_2, shareY = T, titleX = T, titleY = T)
    # p <- gridExtra::arrangeGrob(plots$filter_mainplot_2[[1]], plots$filter_mainplot_2[[2]], ncol = 2)
    plots$last_plot_2 <- p
    grid::grid.draw(p)
  }
  else {
    plots$last_plot_2 <- plots$filter_mainplot_2
    return(plots$filter_mainplot_2)
  }
})

# plot one or both UI elements
output$filter_dynamic_mainplot <- renderUI({
  if (!is.null(objects$omicsData) & is.null(objects$omicsData_2)) {
    withSpinner(plotlyOutput("filter_mainplot"))
  }
  else if (any(!is.null(objects$omicsData), !is.null(objects$omicsData_2))) {
    ui1 = withSpinner(plotlyOutput("filter_mainplot"))
    ui2 = withSpinner(plotlyOutput("filter_mainplot_2"))

    lipid_tabset_plots(ui1, ui2, input$omic_1_name, input$omic_2_name)
  }
})

# Sample (fdata) filter
output$fdata_customfilt <- renderUI({
  req(!is.null(objects$omicsData))
  fdata_IDS <- objects$omicsData$f_data %>% purrr::pluck(get_fdata_cname(objects$omicsData))
  if (!(isTRUE(input$fdata_customfilt_regex == "") | is.null(input$fdata_customfilt_regex))) {
    fdata_IDS <- fdata_IDS[grepl(input$fdata_customfilt_regex, fdata_IDS)]
  }
  if (two_lipids() || two_metab()) {
    req(!is.null(objects$omicsData_2))
    fdata_IDS_2 <- objects$omicsData_2$f_data %>% purrr::pluck(get_fdata_cname(objects$omicsData_2))
    if (!(isTRUE(input$fdata_customfilt_regex_2 == "") | is.null(input$fdata_customfilt_regex_2))) {
      fdata_IDS_2 <- fdata_IDS_2[grepl(input$fdata_customfilt_regex_2, fdata_IDS_2)]
    }

    fluidRow(
      column(
        6,
        div(
          pickerInput(
            "fdata_customfilt_choices",
            sprintf("Filter samples (%s)", omic_1_name()),
            choices = fdata_IDS,
            multiple = TRUE,
            options = list(`live-search` = TRUE, `actions-box` = TRUE)
          )
        )
      ),
      column(
        6,
        div(
          pickerInput(
            "fdata_customfilt_choices_2",
            sprintf("Filter samples (%s)", omic_2_name()),
            choices = fdata_IDS_2,
            multiple = TRUE,
            options = list(`live-search` = TRUE, `actions-box` = TRUE)
          )
        )
      )
    )
  }
  else {
    pickerInput(
      "fdata_customfilt_choices",
      "Filter samples",
      choices = fdata_IDS,
      multiple = TRUE,
      options = list(`live-search` = TRUE, `actions-box` = TRUE)
    )
  }
})

#'@details Condition UI for rmd metric selection.  We deselect Proportion_Missing
#' by default in the case of lipid data or metabolite data.
output$rmd_metrics_out <- renderUI({
  req(!is.null(objects$omicsData))
  
  ## Error throwing work around
  metric_set <- unlist(global_input_choices[['RMD_FILTER_CHOICES']])
  metric_set2 <- combn(metric_set, 2, simplify = F)
  
  res_check <- map(metric_set2, function(metric) {
    tryCatch({
      tmpfilt <- rmd_filter(objects$omicsData, 
                  metrics = metric)
      
      zero_var = list()
      
      for (var in attr(tmpfilt, "metrics")){
        zero_var[[var]] <- var(tmpfilt[[var]]) == 0
      }
      
      return (zero_var)
      
    }, error = function(e){
      print(e)
      return(NULL)
    })

  })
  
  error_pairs <- unlist(metric_set2[map_lgl(res_check, is.null)])
  disable_metric <- unique(error_pairs[duplicated(error_pairs)])
  
  # Here we are checking for variables that didn't error out, but that have zero variance.
  # Currently this isn't doing much, as the only one that is likely to have zero variance 
  # is Proportion_Missing, however it will also throw an error and be disabled previously
  # so kinda redundant.  Keeping this block since it doesn't seem to break things and will
  # still catch the unlikely scenarios of e.g. equal variance for correlation.
  non_null_idx <- !map_lgl(res_check, is.null)
  non_null_pairs <- res_check[non_null_idx]
  metrics_non_null <- metric_set2[non_null_idx] 
  compare_df <- data.frame(zero_var = unlist(non_null_pairs), metrics = unlist(metrics_non_null))
  zero_var_metrics <- compare_df %>% dplyr::filter(zero_var == TRUE) %>% dplyr::pull(metrics) %>% unique()
  
  disable_metric <- union(disable_metric, zero_var_metrics)
  ok_metrics <- metric_set[!(metric_set %in% disable_metric)]
  metric_filt <- !(metric_set %in% ok_metrics)

  if(inherits(objects$omicsData, c("lipidData", "metabData"))){
    selected <-  c("MAD", "Kurtosis", "Skewness", "Correlation")
    selected <- selected[selected %in% c("MAD", "Kurtosis", "Skewness", "Correlation", "Proportion_Missing")[!metric_filt]]
  }
  else {
    selected <-  c("MAD", "Kurtosis", "Skewness", "Correlation", "Proportion_Missing")
    selected <- selected[selected %in% c("MAD", "Kurtosis", "Skewness", "Correlation", "Proportion_Missing")[!metric_filt]]
  }

  if(all(!metric_filt)) metric_filt <- NULL
  
  pickerInput("rmd_metrics", 
              div(
                "Metrics to determine outliers",
                div(
                  style = "color:deepskyblue;display:inline-block",
                  tipify(
                    icon("question-sign", lib = "glyphicon"),
                    title = "Metrics with zero variance are disabled"
                  )
                )
              ),
              choices = global_input_choices[['RMD_FILTER_CHOICES']],
              selected = selected,
              choicesOpt = list(
                disabled = metric_filt
              ),
              multiple = TRUE
  )
})

#'@details warning icon that appears when we select proportion missing as
#'a metric for lipidomics or metabolomics data.
output$rmd_propmis_warn_icon <- renderUI({
  req(!is.null(objects$omicsData), input$rmd_metrics)
  if(
    inherits(objects$omicsData, c("lipidData", "metabData")) &
    "Proportion_Missing" %in% input$rmd_metrics
    ){
    return(tipify(
        icon("exclamation-sign", lib = "glyphicon", style="color:deepskyblue;display:inline-block"), 
        title = ttext_[["RMD_PROP_MISSING_WARNING"]]
      )
    )
  } else {
    return(NULL)
  }
})

# Conditional UI for rmd filter, depends on the sample names in the file(s)
output$rmdfilt_plot_type_UI <- renderUI({
  req(!is.null(objects$omicsData))
  if (input$rmdfilt_plot_type == "all") {
    NULL
  }
  else if (input$rmdfilt_plot_type == "subset") {
    if (two_lipids() || two_metab()) {
      req(!is.null(objects$omicsData_2))
      choices1 <- objects$omicsData$f_data[, get_fdata_cname(objects$omicsData)]
      choices2 <- objects$omicsData_2$f_data[, get_fdata_cname(objects$omicsData_2)]

      tagList(
        tags$b("Select samples to inspect:"),
        fluidRow(
          column(6, pickerInput("rmd_sample", NULL, choices = choices1, multiple = FALSE)),
          column(6, pickerInput("rmd_sample_2", NULL, choices = choices2, multiple = FALSE))
        )
      )
    }
    else {
      choices1 <- objects$omicsData$f_data[, get_fdata_cname(objects$omicsData)]
      pickerInput("rmd_sample", "Select samples to inspect:", choices = choices1, multiple = FALSE)
    }
  }
  else if (input$rmdfilt_plot_type == "outliers") {
    temp_rmd_filter1 <- rmd_filter(objects$omicsData, metrics = input$rmd_metrics)
    choices1 <- temp_rmd_filter1 %>% 
      dplyr::filter(pvalue < input$pvalue_threshold) %>% 
      dplyr::pull(get_fdata_cname(objects$omicsData))

    if (two_lipids() || two_metab()) {
      req(!is.null(objects$omicsData_2))
      temp_rmd_filter2 <- rmd_filter(objects$omicsData_2, metrics = input$rmd_metrics)
      choices2 <- temp_rmd_filter2 %>% 
        dplyr::filter(pvalue < input$pvalue_threshold) %>% 
        dplyr::pull(get_fdata_cname(objects$omicsData_2))

      tagList(
        tags$p("Select samples to inspect:"),
        fluidRow(
          column(6, pickerInput("rmd_sample", NULL, choices = choices1, multiple = FALSE)),
          column(6, pickerInput("rmd_sample_2", NULL, choices = choices2, multiple = FALSE))
        )
      )
    }
    else {
      pickerInput("rmd_sample", "Select samples to inspect", choices = choices1, multiple = FALSE)
    }
  }
})

#'@details Picker inputs for selecting which molecules to remove using e_data
#'custom filter.
output$edata_customfilt_pickers<- renderUI({
  req(!is.null(objects$omicsData))
  mols1 = objects$omicsData$e_data[,get_edata_cname(objects$omicsData)]
  
  if(two_lipids() || two_metab()){
    validate(need(!is.null(objects$omicsData_2), "No second object"))
    mols2 = objects$omicsData_2$e_data[,get_edata_cname(objects$omicsData_2)]
    
    tagList(
      tags$b("Filter by expression data identifiers:"),
      fluidRow(
        column(6,
            pickerInput(
              "edata_customfilt_remove_mols_1",
              sprintf("(%s)", omic_1_name()),
              choices = mols1,
              multiple = T,
              options = list(`live-search` = TRUE, `actions-box` = TRUE)
            )       
        ),
        column(6, 
            pickerInput(
              "edata_customfilt_remove_mols_2",
              sprintf("(%s)", omic_2_name()),
              choices = mols2,
              multiple = T,
              options = list(`live-search` = TRUE, `actions-box` = TRUE)
            ) 
        )
      )
    )
  } else {
    pickerInput(
      "edata_customfilt_remove_mols_1",
      "Filter by expression data identifiers:",
      choices = mols1,
      multiple = T,
      options = list(`live-search` = TRUE, `actions-box` = TRUE)
    ) 
  }
  
})

#'@details Pickers for which e-meta columns to use when filtering by e-meta
output$emeta_customfilt_which_col <- renderUI({
  req(objects$omicsData$e_meta)
  
  choices1 = colnames(objects$omicsData$e_meta)
  
  if(two_lipids() || two_metab()) {
    validate(need(!is.null(objects$omicsData_2), "No second object"))
    choices2 = colnames(objects$omicsData_2$e_meta)
    
    tagList(
      tags$p("Filter by which columns:"),
      fluidSplitLayout(
        pickerInput(
          "emeta_customfilt_which_col_1",
          sprintf("(%s)", omic_1_name()),
          choices = choices1
        ),
        pickerInput(
          "emeta_customfilt_which_col_2",
          sprintf("(%s)", omic_2_name()),
          choices = choices2
        )
      )
    )
  } else {
    pickerInput(
      "emeta_customfilt_which_col_1",
      "Filter by which column:",
      choices = choices1
    )
  }
  
})

#'@details Picker inputs for selecting which molecules to remove using e_data
#'custom filter.
output$emeta_customfilt_pickers<- renderUI({
  validate(
    need(objects$omicsData$e_meta, "No biomolecule information found."),
    need(
      input$emeta_customfilt_which_col_1,
      ifelse(
        isTruthy(objects$omicsData$e_meta), 
        "Select which column to use.",
        ""
      )
    )
  )
  
  choices1 <- objects$omicsData$e_meta %>% 
    purrr::pluck(input$emeta_customfilt_which_col_1) %>% unique() %>% sort()
  
  if(two_lipids() || two_metab()){
    validate(
      need(!is.null(objects$omicsData_2$e_meta), "No biomolecule information for second data object."),
      need(input$emeta_customfilt_which_col_2, "Select second identifier.")
    )
    choices2 <- objects$omicsData_2$e_meta %>% 
      purrr::pluck(input$emeta_customfilt_which_col_2) %>% unique() %>% sort()
    
    tagList(
      tags$p("Filter which biomolecule information values:"),
      fluidSplitLayout(
        pickerInput(
          "emeta_customfilt_which_values_1",
          sprintf("(%s)", omic_1_name()),
          choices = choices1,
          multiple = T,
          options = list(`live-search` = TRUE, `actions-box` = TRUE)
        ),
        pickerInput(
          "emeta_customfilt_which_values_2",
          sprintf("(%s)", omic_1_name()),
          choices = choices2,
          multiple = T,
          options = list(`live-search` = TRUE, `actions-box` = TRUE)
        )
      )
    )
    
  } else {
    pickerInput(
      "emeta_customfilt_which_values_1",
      "Filter which biomolecule information values:",
      choices = choices1,
      multiple = T,
      options = list(`live-search` = TRUE, `actions-box` = TRUE)
    )
  }
  
})

# inputs for axes labels and sizes
output$filter_plot_options <- renderUI({
  # Placeholder for conditional plot options based on filter type
  if (!is.null(revals$filter_vis) &&
      revals$filter_vis == "rmdfilt") {
    choices <- colnames(objects$omicsData$f_data %>%
                          dplyr::select(-one_of(
                            attributes(objects$omicsData)$cnames$fdata_cname
                          )))
    
    extra_UI <- tagList(
      h4("Main Plot Options"),
      pickerInput(
        "filter_rmd_order_by", 
        "Order By", 
        choices = c(
          "Select one" = NULLSELECT_,
          choices,
          "Group"
        ),
        selected = NULLSELECT_
      ),
      bsButton("plot_update_rmdfilt", "Update plot")
    )
  } else {
    extra_UI <- NULL
  }
  
  tagList(
    extra_UI,
    h4("Axes Options"),
    style_UI("filter")
  )
})

# apply filter plot style options
output$filter_apply_style <- renderUI({
  apply_style_UI("filter",
                  two_lipids() || two_metab(),
                  inherits(plots$filter_mainplot, "list"),
                  omic_1_name = omic_1_name(),
                  omic_2_name = omic_2_name())
})

# summary tables
output$filter_summary <- renderDT(revals$filter_summary, rownames = T, options = list(dom = "p", pageLength = 8))
output$filter_summary_2 <- renderDT(revals$filter_summary_2, rownames = T, options = list(dom = "p", pageLength = 8))

output$filter_data_summary <- renderUI({
  req(!is.null(revals$filter_summary), cancelOutput = TRUE)
  wellPanel(
    if (two_lipids() || two_metab()) {
      req(!is.null(revals$filter_summary_2), cancelOutput = TRUE)
      splitLayout(
        tagList(omic_1_name(), DTOutput("filter_summary")),
        tagList(omic_2_name(), DTOutput("filter_summary_2"))
      )
    }
    else {
      DTOutput("filter_summary")
    }
  )
})

#'@details Check whether we have done stats or normalized the data, warn users 
#' that they will have to re-apply normalization if their data was not already
#' normalized, and reapply stats.
output$execute_apply_filters_UI <- renderUI({
  upload_isnorm <- attr(objects$omicsData, "data_info")$norm_info$is_normalized
  cur_isnorm <- attr(objects$omicsData, "data_info")$norm_info$is_normalized
  is_rolled_up <- inherits(objects$omicsData, "proData") & inherits(objects$omicsData, "pepData")
  will_reset <- (!upload_isnorm & cur_isnorm) | !is.null(objects$imdanova_res) | is_rolled_up
  
  if (will_reset) {
    div(
      tags$span(style = "color:red", infotext_[['RESET_FILTERS_WARNING']]),
      div(
        bsButton("allow_reapply_filters", "OK, let me reset"),
        disabled(
          bsButton("apply_filters", "Reset and apply all filters", style = "primary")
        ),
        div(style = "float:right", modalButton("Update filter values (don't apply)"))
      )
    )
  } else {
    div(
      bsButton("apply_filters", "Apply all filters", style = "primary"),
      div(style = "float:right", modalButton("Update filter values (don't apply)"))
    )
  }
})

#'@details Output that is used to toggle a conditional panel containing the proteomics filter.
output$toggle_profilt <- eventReactive(objects$omicsData, {
  return(inherits(objects$omicsData, "pepData") && !is.null(objects$omicsData$e_meta))
})
outputOptions(output, "toggle_profilt", suspendWhenHidden = FALSE)

#
output$warnings_filter <- renderUI({
  HTML(paste(revals$warnings_filter, collapse = ""))
})

output$warnings_filter_modal <- renderUI({
  HTML(paste(revals$warnings_filter, collapse = ""))
})

