list(
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
    
    numericInput("cv_threshold", title, round(max_cv*0.9, 2), step = 1)
  }),

  # Summary of current filters and parameters
  output$filter_review <- renderUI({
    # store text as a list of HTML elements
    divs <- list()

    fdata_cname <- attributes(objects$omicsData)$cnames$fdata_cname
    fdata_cname_2 <- attributes(objects$omicsData_2)$cnames$fdata_cname

    # indices of which filters/HTML elements belong to each object
    obj2_inds <- which(grepl("_2", names(objects$filters)))
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
    #' nodes in the flow control
    #' TODO: collect all samples and perform differences at the end?
    rmd_removed_samps <- if (any(grepl("rmdfilt", names(objects$filters)))) {
      tmp_idx = which(grepl("rmdfilt", names(objects$filters)))
      attr(objects$filters[[tmp_idx]], "sample_names")[which(objects$filters[[tmp_idx]]$pvalue < input$pvalue_threshold)]
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
        tcfilt_summ <- summary(tcfilt, min_count = input$min_num_trans)
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
          tags$b("RNA Filter:"),
          tags$p(sprintf("Library Size Threshold: %s", input$rnafilt_min_libsize)),
          tags$p(sprintf("Removed Samples: %s", ifelse(length(rnafilt_libsize_removed_samps > 0), paste(rnafilt_libsize_removed_samps, collapse = " | "), "None"))),
          hr()
        )
      }
      
      # rna filter (min nonzero) 
      else if (grepl("rnafilt_min_nonzero", names(objects$filters)[i])) {
        divs[[i]] <- tagList(
          tags$b("RNA Filter:"),
          tags$p(sprintf("Minimum Nonzero Count Threshold: %s", input$rnafilt_min_nonzero)),
          tags$p(sprintf("Removed Samples: %s", ifelse(length(rnafilt_min_nonzero_removed_samps > 0), paste(rnafilt_min_nonzero_removed_samps, collapse = " | "), "None"))),
          hr()
        )
      }
      
      # rmd filter
      else if (grepl("rmdfilt", names(objects$filters)[i])) {
        divs[[i]] <- tagList(
          tags$b("rMd Filter:"),
          tags$p(sprintf("p-value threshold: %s", input$pvalue_threshold)),
          tags$p(sprintf("Removed Samples: %s", ifelse(length(rmd_removed_samps > 0), paste(rmd_removed_samps, collapse = " | "), "None"))),
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
        e_data_remove = objects$filters$customfilt$e_data_remove
        e_meta_remove = objects$filters$customfilt$e_meta_remove
        
        #' construct divs containing info about what was removed by the custom filter
        e_data_remove_div <- if(length(e_data_remove) > 0) {
          tagList(tags$p(
            "Biomolecules Removed: ",
            div(style = "overflow-x:auto;max-height:150px", paste(e_data_remove, collapse = " | ")),
            div(sprintf("Total: %s", length(e_data_remove)))
          ))
        } else {
          NULL
        }
        
        f_data_remove_div <- if(length(f_data_remove) > 0) {
          tagList(
            tags$p("Samples Removed: ", div(style = "overflow-x:auto", paste(f_data_remove, collapse = " | "))),
            tags$p("Remaining Samples:", div(style = "overflow-x:auto", paste(f_data_keep, collapse = " | ")))
          )
        } else {
          NULL
        }
        
        e_meta_remove_div <- if(length(e_meta_remove) > 0) {
          tagList(tags$p(
            "Biomolecules removed by association with extra biomolecule information:",
            div(style = "overflow-x:auto;max-height:150px", paste(e_meta_remove, collapse = " | ")),
            div(sprintf("Total: %s", length(e_meta_remove)))
          ))
        } else {
          NULL
        }
        
        divs[[i]] <- tagList(
          tags$b("Custom Filter:"),
          f_data_remove_div,
          hr(),
          e_data_remove_div,
          hr(),
          e_meta_remove_div,
          hr()
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

    # Display two summary sections if there are two omicsData objects
    if (length(obj2_inds) > 0) {
      tagList(
        h3(tags$b("Filters to be applied to Object 1:  ")),
        hr(),
        divs[obj1_inds],
        h3(tags$b("Filters to be applied to Object 2:  ")),
        hr(),
        divs[obj2_inds]
      )
    }
    else {
      tagList(
        h3(tags$b("Filters to be applied:  ")),
        divs
      )
    }
  }),

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
  }),

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
  }),

  # plot one or both UI elements
  output$filter_dynamic_mainplot <- renderUI({
    if (!is.null(objects$omicsData) & is.null(objects$omicsData_2)) {
      withSpinner(plotlyOutput("filter_mainplot"))
    }
    else if (any(!is.null(objects$omicsData), !is.null(objects$omicsData_2))) {
      tagList(
        withSpinner(plotlyOutput("filter_mainplot")),
        withSpinner(plotlyOutput("filter_mainplot_2"))
      )
    }
  }),

  # Sample (fdata) filter
  output$fdata_customfilt <- renderUI({
    req(!is.null(objects$omicsData))
    fdata_IDS <- objects$omicsData$f_data %>% purrr::pluck(get_fdata_cname(objects$omicsData))
    if (!(isTRUE(input$fdata_customfilt_regex == "") | is.null(input$fdata_customfilt_regex))) {
      fdata_IDS <- fdata_IDS[grepl(input$fdata_customfilt_regex, fdata_IDS)]
    }
    if (two_lipids()) {
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
              "Filter samples (dataset 1)",
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
              "Filter samples (dataset 2)",
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
  }),

  #'@details Condition UI for rmd metric selection.  We deselect Proportion_Missing
  #' by default in the case of lipid data or metabolite data.
  output$rmd_metrics_out <- renderUI({
    req(!is.null(objects$omicsData))
    
    ## Error throwing work around
    map_list <- c("MAD", "Kurtosis", "Skewness", "Corr", "Proportion_Missing")
    metric_set <- unlist(global_input_choices[['RMD_FILTER_CHOICES']])
    metric_set2 <- combn(metric_set, 2, simplify = F)
    
    res_check <- map(metric_set2, function(metric){
      tryCatch({
        rmd_filter(objects$omicsData, 
                   metrics = metric)
      }, error = function(e){
        print(e)
        return(NULL)
      })

    })
    
    pairs <- unlist(metric_set2[map_lgl(res_check, is.null)])
    disable_metric <- unique(pairs[duplicated(pairs)])
    
    ok_meterics <- metric_set[!(metric_set %in% disable_metric)]
    map_list <- map_list[!(metric_set %in% disable_metric)]
    
    filter <- rmd_filter(objects$omicsData, metrics = ok_meterics)
    ok_meterics <- ok_meterics[map_lgl(map_list, function(metric) var(filter[metric]) != 0)]
    metric_filt <- !(metric_set %in% ok_meterics)

    if(class(objects$omicsData) %in% c("lipidData", "metabData")){
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
  }),
  
  #'@details warning icon that appears when we select proportion missing as
  #'a metric for lipidomics or metabolomics data.
  output$rmd_propmis_warn_icon <- renderUI({
    req(!is.null(objects$omicsData), input$rmd_metrics)
    if(
      class(objects$omicsData) %in% c("lipidData", "metabData") &
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
  }),
  
  # Conditional UI for rmd filter, depends on the sample names in the file(s)
  output$rmdfilt_plot_type <- renderUI({
    req(!is.null(objects$omicsData))
    if (input$rmdfilt_plot_type == "all") {
      NULL
    }
    else if (input$rmdfilt_plot_type == "subset") {
      if (two_lipids()) {
        req(!is.null(objects$omicsData_2))
        choices1 <- objects$omicsData$f_data[, get_fdata_cname(objects$omicsData)]
        choices2 <- objects$omicsData_2$f_data[, get_fdata_cname(objects$omicsData_2)]

        tagList(
          tags$p("Select samples to inspect:"),
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
      if (two_lipids()) {
        req(!is.null(objects$omicsData_2))
        temp_rmd_filter2 <- rmd_filter(objects$omicsData_2, metrics = input$rmd_metrics)
        choices1 <- objects$omicsData$f_data[which(temp_rmd_filter1$pvalue < input$pvalue_threshold), get_fdata_cname(objects$omicsData)]
        choices2 <- objects$omicsData_2$f_data[which(temp_rmd_filter2$pvalue < input$pvalue_threshold), get_fdata_cname(objects$omicsData_2)]

        tagList(
          tags$p("Select samples to inspect:"),
          fluidRow(
            column(6, pickerInput("rmd_sample", NULL, choices = choices1, multiple = FALSE)),
            column(6, pickerInput("rmd_sample_2", NULL, choices = choices2, multiple = FALSE))
          )
        )
      }
      else {
        choices1 <- objects$omicsData$f_data[which(temp_rmd_filter1$pvalue < input$pvalue_threshold), get_fdata_cname(objects$omicsData)]
        pickerInput("rmd_sample", "Select samples to inspect", choices = choices1, multiple = FALSE)
      }
    }
  }),
  
  #'@details Picker inputs for selecting which molecules to remove using e_data
  #'custom filter.
  output$edata_customfilt_pickers<- renderUI({
    req(!is.null(objects$uploaded_omicsData))
    mols1 = objects$uploaded_omicsData$e_data[,get_edata_cname(objects$uploaded_omicsData)]
    
    if(two_lipids()){
      validate(need(!is.null(objects$uploaded_omicsData_2), "No second object"))
      mols2 = objects$uploaded_omicsData_2$e_data[,get_edata_cname(objects$uploaded_omicsData_2)]
      
      tagList(
        h5("Filter by data file identifiers:"),
        fluidRow(
          column(6,
             pickerInput(
               "edata_customfilt_remove_mols_1",
               choices = mols1,
               multiple = T,
               options = list(`live-search` = TRUE, `actions-box` = TRUE)
             )       
          ),
          column(6, 
             pickerInput(
               "edata_customfilt_remove_mols_2",
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
        "Filter by data file identifiers:",
        choices = mols1,
        multiple = T,
        options = list(`live-search` = TRUE, `actions-box` = TRUE)
      ) 
    }
    
  }),
  
  #'@details Pickers for which e-meta columns to use when filtering by e-meta
  output$emeta_customfilt_which_col <- renderUI({
    req(objects$uploaded_omicsData$e_meta)
    
    choices1 = colnames(objects$uploaded_omicsData$e_meta)
    
    if(two_lipids()) {
      validate(need(!is.null(objects$uploaded_omicsData_2), "No second object"))
      choices2 = colnames(objects$uploaded_omicsData_2)
      
      tagList(
        tags$p("Filter by which columns:"),
        div(
          class = 'inline-wrapper-1',
          pickerInput(
            "emeta_customfilt_which_col_1",
            choices = choices1
          ),
          pickerInput(
            "emta_customfilt_which_col_2",
            choices = mols2
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
    
  }),
  
  #'@details Picker inputs for selecting which molecules to remove using e_data
  #'custom filter.
  output$emeta_customfilt_pickers<- renderUI({
    validate(
      need(objects$uploaded_omicsData$e_meta, "No biomolecule information found."),
      need(input$emeta_customfilt_which_col_1, "Select which column to use.")
    )
    
    choices1 <- objects$uploaded_omicsData$e_meta %>% 
      purrr::pluck(input$emeta_customfilt_which_col_1) %>% unique() %>% sort()
    
    if(two_lipids()){
      validate(
        need(!is.null(objects$uploaded_omicsData_2$e_meta), "No biomolecule information for second data object."),
        need(input$emeta_customfilt_which_col_2, "Select second identifier.")
      )
      choices2 <- objects$uploaded_omicsData_2$e_meta %>% 
        purrr::pluck(input$emeta_customfilt_which_col_2) %>% unique() %>% sort()
      
      tagList(
        tags$p("Filter which biomolecule information values:"),
        div(
          class = 'inline-wrapper-1',
          pickerInput(
            "emeta_customfilt_which_values_1",
            choices = choices1,
            multiple = T,
            options = list(`live-search` = TRUE, `actions-box` = TRUE)
          ),
          pickerInput(
            "emta_customfilt_which_values_2",
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
    
  }),
  
  # Conditional UI for proteomics filter, invisible if the object is not pepData with protein column
  # output$profilt_UI <- renderUI({
  #   if (class(objects$omicsData) == "pepData") {
  #     tagList(
  #       fluidRow(
  #         column(
  #           6,
  #           actionButton("add_profilt",
  #             label = div("add/remove proteomics filter", hidden(div(id = "profilt_exists", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))),
  #             width = "100%"
  #           ),
  #           actionButton("plot_profilt", "Plot this filter", width = "100%"),
  #           actionButton("remove_profilt", "Remove proteomics filter", width = "100%")
  #         ),
  #         column(
  #           6,
  #           numericInput("min_num_peps", "Minimum number of peptides mapped to each protein:", 2, step = 1),
  #           checkboxInput("degen_peps", "Remove Degenerate Peptides?", TRUE)
  #         )
  #       ),
  #       hr()
  #     )
  #   }
  #   else {
  #     NULL
  #   }
  # }),

  # inputs for axes labels and sizes
  output$filter_plot_options <- renderUI({
    # Placeholder for conditional plot options based on filter type
    if(FALSE) {
      extra_UI <- NULL
    } else {
      extra_UI <- NULL
    }
    style_UI("filter", extra_UI)
  }),

  # apply filter plot style options
  output$filter_apply_style <- renderUI({
    apply_style_UI("filter", two_lipids(), inherits(plots$filter_mainplot, "list"))
  }),

  # summary tables
  output$filter_summary <- renderDT(revals$filter_summary, rownames = T, options = list(dom = "p", pageLength = 8)),
  output$filter_summary_2 <- renderDT(revals$filter_summary_2, rownames = T, options = list(dom = "p", pageLength = 8)),

  output$filter_data_summary <- renderUI({
    req(!is.null(revals$filter_summary), cancelOutput = TRUE)
    wellPanel(
      if (two_lipids()) {
        req(!is.null(revals$filter_summary_2), cancelOutput = TRUE)
        splitLayout(
          DTOutput("filter_summary"),
          DTOutput("filter_summary_2")
        )
      }
      else {
        DTOutput("filter_summary")
      }
    )
  }),
  
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
  }),
  
  #
  output$warnings_filter <- renderUI({
    HTML(paste(revals$warnings_filter, collapse = ""))
  }),

  output$warnings_filter_modal <- renderUI({
    HTML(paste(revals$warnings_filter, collapse = ""))
  })
)
