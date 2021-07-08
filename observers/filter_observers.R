#### The creation of all filter objects follows roughly the same pattern:
# 1) Clear any warning message for that filter
# 2) Attempt to store the filter object in a reactive value
# 3) If failed, display a warning message and return the old value of the filter (NULL if no filter was created yet)
# 4) Do (1-3) for the second object if present

# create molfilt objects
observeEvent(input$add_molfilt, {
  molfilt_exists <- !is.null(objects$filters$molfilt) & (!is.null(objects$filters$molfilt_2) | is.null(objects$uploaded_omicsData_2))

  if (!molfilt_exists) {
    objects$filters$molfilt <- tryCatch(
      {
        revals$warnings_filter$molecule_filter_1 <<- NULL
        molecule_filter(objects$uploaded_omicsData)
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your molecule filter object \n System error:  ", e)
        revals$warnings_filter$molecule_filter1 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        NULL
      }
    )
    if (!is.null(objects$uploaded_omicsData_2)) {
      objects$filters$molfilt_2 <- tryCatch(
        {
          revals$warnings_filter$molecule_filter2 <<- NULL
          molecule_filter(objects$uploaded_omicsData_2)
        },
        error = function(e) {
          msg <- paste0("Something went wrong updating your second molecule filter object \n System error:  ", e)
          revals$warnings_filter$molecule_filter2 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
          NULL
        }
      )
    }
  }
  else {
    revals$warnings_filter$molecule_filter_1 <- revals$warnings_filter$molecule_filter2 <- objects$filters$molfilt <- objects$filters$molfilt_2 <- NULL
  }

  # check if molfilt was created
  molfilt_exists <- !is.null(objects$filters$molfilt) & (!is.null(objects$filters$molfilt_2) | is.null(objects$uploaded_omicsData_2))

  # toggle('remove_molfilt', condition = !is.null(objects$filters$molfilt), anim = TRUE)
  toggle("molfilt_exists", condition = molfilt_exists, anim = TRUE)
  toggleState("mol_min_num", condition = !molfilt_exists)
  toggleCssClass("mol_min_num", "grey_text", condition = molfilt_exists)
})

# create cvfilt objects
observeEvent(input$add_cvfilt, {
  cvfilt_exists <- !is.null(objects$filters$cvfilt) & (!is.null(objects$filters$cvfilt_2) | is.null(objects$uploaded_omicsData_2))

  if (!cvfilt_exists) {
    objects$filters$cvfilt <- tryCatch(
      {
        revals$warnings_filter$cv_filter1 <<- NULL
        tmp_cvfilt <- cv_filter(objects$uploaded_omicsData)
        test_filt_conditions <- summary(
          tmp_cvfilt,
          cv_threshold = input$cv_threshold
        )
        tmp_cvfilt
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your CV filter object \n System error:  ", e)
        revals$warnings_filter$cv_filter1 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        NULL
      }
    )
    if (!is.null(objects$uploaded_omicsData_2)) {
      objects$filters$cvfilt_2 <- tryCatch(
        {
          revals$warnings_filter$cv_filter2 <<- NULL
          tmp_cvfilt <- cv_filter(objects$uploaded_omicsData_2)
          test_filt_conditions <- summary(
            tmp_cvfilt,
            cv_threshold = input$cv_threshold
          )
          cv_filter(objects$uploaded_omicsData_2)
        },
        error = function(e) {
          msg <- paste0("Something went wrong updating your second CV filter object \n System error:  ", e)
          revals$warnings_filter$cv_filter2 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
          NULL
        }
      )
    }
  }
  else {
    revals$warnings_filter$cv_filter1 <- revals$warnings_filter$cv_filter2 <- objects$filters$cvfilt <- objects$filters$cvfilt_2 <- NULL
  }

  cvfilt_exists <- !is.null(objects$filters$cvfilt) & (!is.null(objects$filters$cvfilt_2) | is.null(objects$uploaded_omicsData_2))

  # toggle('remove_cvfilt', condition = !is.null(objects$filters$cvfilt), anim = TRUE)
  toggle("cvfilt_exists", condition = !is.null(objects$filters$cvfilt) & (!is.null(objects$filters$cvfilt_2) | is.null(objects$uploaded_omicsData_2)), anim = TRUE)
  toggleState("cv_threshold", condition = !cvfilt_exists)
  toggleCssClass("cv_threshold", "grey_text", condition = cvfilt_exists)
})

# create profilt object
# this should never be an option if we're in lipid land
observeEvent(input$add_profilt, {
  if (is.null(objects$filters$profilt)) {
    objects$filters$profilt <- tryCatch(
      {
        revals$warnings_filter$profilt <<- NULL
        proteomics_filter(objects$uploaded_omicsData)
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your proteomics filter object \n System error:  ", e)
        revals$warnings_filter$profilt <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        objects$filters$profilt
      }
    )
  }
  else {
    revals$warnings_filter$profilt <- objects$filters$profilt <- NULL
  }

  # toggle('remove_profilt', condition = !is.null(objects$filters$profilt), anim = TRUE)
  toggle("profilt_exists", condition = !is.null(objects$filters$profilt), anim = TRUE)
  toggleState("min_num_peps", condition = is.null(objects$filters$profilt))
  toggleCssClass("min_num_peps", "grey_text", condition = !is.null(objects$filters$profilt))
  toggleState("degen_peps", condition = is.null(objects$filters$profilt))
  toggleCssClass("degen_peps", condition = !is.null(objects$filters$profilt))
})

# hide/show profilt if we are in pepdata land
observeEvent(objects$uploaded_omicsData, {
  toggle("profilt_UI", condition = inherits(objects$uploaded_omicsData, "pepData"))
})

# create imdanovafilt object
observeEvent(input$add_imdanovafilt, {
  imdanovafilt_exists <- !is.null(objects$filters$imdanovafilt) & (!is.null(objects$filters$imdanovafilt_2) | is.null(objects$uploaded_omicsData_2))

  if (!imdanovafilt_exists) {
    objects$filters$imdanovafilt <- tryCatch(
      {
        revals$warnings_filter$imdanovafilt1 <<- NULL
        imdanova_filter(objects$uploaded_omicsData)
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your imd-ANOVA filter object \n System error:  ", e)
        revals$warnings_filter$imdanovafilt1 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        NULL
      }
    )

    if (!is.null(objects$uploaded_omicsData_2)) {
      objects$filters$imdanovafilt_2 <- tryCatch(
        {
          revals$warnings_filter$imdanovafilt2 <<- NULL
          imdanova_filter(objects$uploaded_omicsData_2)
        },
        error = function(e) {
          msg <- paste0("Something went wrong updating your second imd-ANOVA filter object \n System error:  ", e)
          revals$warnings_filter$imdanovafilt2 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
          NULL
        }
      )
    }
  }
  else {
    revals$warnings_filter$imdanovafilt1 <- revals$warnings_filter$imdanovafilt2 <- objects$filters$imdanovafilt <- objects$filters$imdanovafilt_2 <- NULL
  }

  imdanovafilt_exists <- !is.null(objects$filters$imdanovafilt) & (!is.null(objects$filters$imdanovafilt_2) | is.null(objects$uploaded_omicsData_2))

  # toggle('remove_imdanovafilt', condition = !is.null(objects$filters$imdanovafilt), anim = TRUE)
  toggle("imdanovafilt_exists", condition = imdanovafilt_exists, anim = TRUE)
  toggleState("min_nonmiss_anova", condition = !imdanovafilt_exists)
  toggleCssClass("min_nonmiss_anova", "grey_text", condition = imdanovafilt_exists)
  toggleState("min_nonmiss_gtest", condition = !imdanovafilt_exists)
  toggleCssClass("min_nonmiss_gtest", "grey_text", condition = imdanovafilt_exists)
})

# create rmdfilt object
observeEvent(input$add_rmdfilt, {
  rmdfilt_exists <- !is.null(objects$filters$rmdfilt) & (!is.null(objects$filters$rmdfilt_2) | is.null(objects$uploaded_omicsData_2))

  if (!rmdfilt_exists) {
    objects$filters$rmdfilt <- tryCatch(
      {
        revals$warnings_filter$rmdfilt1 <<- NULL
        rmd_filter(objects$uploaded_omicsData, metrics = input$rmd_metrics)
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your rMd filter object \n System error:  ", e)
        revals$warnings_filter$rmdfilt1 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        NULL
      }
    )
    if (!is.null(objects$uploaded_omicsData_2)) {
      objects$filters$rmdfilt_2 <- tryCatch(
        {
          revals$warnings_filter$rmdfilt2 <<- NULL
          rmd_filter(objects$uploaded_omicsData_2, metrics = input$rmd_metrics)
        },
        error = function(e) {
          msg <- paste0("Something went wrong updating your second rMd filter object \n System error:  ", e)
          revals$warnings_filter$rmdfilt2 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
          NULL
        }
      )
    }
  }
  else {
    revals$warnings_filter$rmdfilt1 <- revals$warnings_filter$rmdfilt2 <- objects$filters$rmdfilt <- objects$filters$rmdfilt_2 <- NULL
  }

  rmdfilt_exists <- !is.null(objects$filters$rmdfilt) & (!is.null(objects$filters$rmdfilt_2) | is.null(objects$uploaded_omicsData_2))

  toggle("rmdfilt_exists", condition = rmdfilt_exists, anim = TRUE)
  toggleState("pvalue_threshold", condition = !rmdfilt_exists)
  toggleCssClass("pvalue_threshold", "grey_text", condition = rmdfilt_exists)
  toggleState("rmd_metrics", condition = !rmdfilt_exists)
  toggleCssClass(class = "grey_disabled", condition = rmdfilt_exists, selector = "button[data-id='rmd_metrics']")
})

# create customfilt object
observeEvent(input$add_customfilt, {
  customfilt_exists <- !is.null(objects$filters$customfilt) | !is.null(objects$filters$customfilt_2)

  if (!customfilt_exists) {
    objects$filters$customfilt <- tryCatch(
      {
        revals$warnings_filter$customfilt <<- NULL
        # removed samples either user specified samples, or in the case of 'Keep', the complement of user specified samples
        samples_rmv <- if (input$remove_or_keep == "Remove") input$fdata_customfilt_choices else setdiff(sample_names(), input$fdata_customfilt_choices)
        if (length(samples_rmv) > 0) custom_filter(objects$uploaded_omicsData, f_data_remove = samples_rmv) else NULL
      },
      error = function(e) {
        msg <- paste0("Something went wrong updating your custom sample filter object \n System error:  ", e)
        revals$warnings_filter$customfilt1 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        NULL
      }
    )
    if (!is.null(objects$uploaded_omicsData_2)) {
      objects$filters$customfilt_2 <- tryCatch(
        {
          revals$warnings_filter$customfilt_2 <<- NULL
          samples_rmv <- if (input$remove_or_keep == "Remove") input$fdata_customfilt_choices_2 else setdiff(sample_names_2(), input$fdata_customfilt_choices_2)
          if (length(samples_rmv) > 0) custom_filter(objects$uploaded_omicsData_2, f_data_remove = samples_rmv) else NULL
        },
        error = function(e) {
          msg <- paste0("Something went wrong updating your second custom sample filter object \n System error:  ", e)
          revals$warnings_filter$customfilt2 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
          NULL
        }
      )
    }
  }
  else {
    revals$warnings_filter$customfilt1 <- revals$warnings_filter$customfilt2 <- objects$filters$customfilt <- objects$filters$customfilt_2 <- NULL
  }

  customfilt_exists <- !is.null(objects$filters$customfilt) | !is.null(objects$filters$customfilt_2)

  toggle("customfilt_exists", condition = customfilt_exists, anim = TRUE)
  toggleState("remove_or_keep", condition = !customfilt_exists)
  toggleState("fdata_customfilt_choices", condition = !customfilt_exists)
  toggleState("fdata_customfilt_choices_2", condition = !customfilt_exists)
  toggleCssClass(class = "grey_disabled", condition = customfilt_exists, selector = "button[data-id='fdata_customfilt_choices']")
  toggleCssClass(class = "grey_disabled", condition = customfilt_exists, selector = "button[data-id='fdata_customfilt_choices_2']")
})


###############################
#### Filter plot observers ####
###############################

# rmdfilter plot
observeEvent(c(input$plot_rmdfilt, input$rmd_metrics, input$pvalue_threshold, input$rmd_sample, input$rmdfilt_plot_type),
  {
    # store selected sample ID's or NULL if we are plotting all samples
    sampleID1 <- if (length(input$rmd_sample) > 0 & (input$rmdfilt_plot_type %in% c("subset", "outliers"))) input$rmd_sample else NULL

    revals$warnings_filter$rmdfilt_plot <- revals$warnings_filter$rmdfilt_plot_2 <- NULL

    # store plot object in reactive variable
    plots$filter_mainplot <- tryCatch(
      {
        p <- plot(rmd_filter(objects$uploaded_omicsData, metrics = input$rmd_metrics), pvalue_threshold = input$pvalue_threshold, sampleID = sampleID1, bw_theme = TRUE)

        # block for displaying sample names if we are plotting all samples
        if (is.null(sampleID1)) {
          p <- p + theme(axis.text.x = element_text(angle = 45))
          if ("VizSampNames" %in% colnames(objects$uploaded_omicsData$f_data)) {
            p <- p + scale_x_discrete(labels = objects$uploaded_omicsData$f_data$VizSampNames)
          }
        }

        p
      },
      error = function(e) {
        msg <- paste0("Something went wrong making your rmd filter plot \n System error: ", e)
        revals$warnings_filter$rmdfilt_plot <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        NULL
      }
    )

    if (!is.null(objects$uploaded_omicsData_2)) {
      sampleID2 <- if (length(input$rmd_sample_2) > 0 & (input$rmdfilt_plot_type %in% c("subset", "outliers"))) input$rmd_sample_2 else NULL

      plots$filter_mainplot_2 <- tryCatch(
        {
          p <- plot(rmd_filter(objects$uploaded_omicsData_2, metrics = input$rmd_metrics),
            pvalue_threshold = input$pvalue_threshold, sampleID = sampleID2, bw_theme = TRUE
          )

          if (is.null(sampleID2)) {
            p <- p + theme(axis.text.x = element_text(angle = 45))
            if ("VizSampNames" %in% colnames(objects$uploaded_omicsData_2$f_data)) {
              p <- p + scale_x_discrete(labels = objects$uploaded_omicsData_2$f_data$VizSampNames)
            }
          }

          p
        },
        error = function(e) {
          msg <- paste0("Something went wrong making your second rmd filter plot \n System error: ", e)
          revals$warnings_filter$rmdfilt_plot_2 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
          NULL
        }
      )
    }
  },
  ignoreInit = T
)

# proteomics filter plot
observeEvent(c(input$plot_profilt, input$min_num_peps, input$degen_peps),
  {
    revals$warnings_filter$profilt_plot <- NULL

    plots$filter_mainplot <- tryCatch(
      {
        plot(proteomics_filter(objects$uploaded_omicsData), min_num_peps = as.numeric(input$min_num_peps), bw_theme = TRUE)
      },
      error = function(e) {
        msg <- paste0("Something went wrong making your proteomics filter plot \n System error: ", e)
        revals$warnings_filter$profilt_plot <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        NULL
      }
    )
  },
  ignoreInit = T
)

# molecule filter plot
observeEvent(c(input$plot_molfilt, input$mol_min_num),
  {
    revals$warnings_filter$molfilt_plot <- revals$warnings_filter$molfilt_plot_2 <- NULL

    plots$filter_mainplot <- tryCatch(
      {
        plot(molecule_filter(objects$uploaded_omicsData), min_num = input$mol_min_num, bw_theme = TRUE)
      },
      error = function(e) {
        msg <- paste0("Something went wrong making your molecule filter plot \n System error: ", e)
        revals$warnings_filter$molfilt_plot <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        NULL
      }
    )

    if (!is.null(objects$uploaded_omicsData_2)) {
      plots$filter_mainplot_2 <- tryCatch(
        {
          plot(molecule_filter(objects$uploaded_omicsData_2), min_num = input$mol_min_num, bw_theme = TRUE)
        },
        error = function(e) {
          msg <- paste0("Something went wrong making your second molecule filter plot \n System error: ", e)
          revals$warnings_filter$molfilt_plot_2 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
          NULL
        }
      )
    }
  },
  ignoreInit = TRUE
)

# cv filter plot
observeEvent(c(input$plot_cvfilt, input$cv_threshold, input$add_cvfilt),
  {
    revals$warnings_filter$cvfilt_plot <- revals$warnings_filter$cvfilt_plot_2 <- NULL
    
    plots$filter_mainplot <- tryCatch(
      {
        plot(cv_filter(objects$uploaded_omicsData), cv_threshold = input$cv_threshold, bw_theme = TRUE)
      },
      error = function(e) {
        msg <- paste0("Something went wrong making your CV filter plot \n System error: ", e)
        revals$warnings_filter$cvfilt_plot <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        NULL
      }
    )

    if (!is.null(objects$uploaded_omicsData_2)) {
      plots$filter_mainplot_2 <- tryCatch(
        {
          plot(cv_filter(objects$uploaded_omicsData_2), cv_threshold = input$cv_threshold, bw_theme = TRUE)
        },
        error = function(e) {
          msg <- paste0("Something went wrong making your second CV filter plot \n System error: ", e)
          revals$warnings_filter$cvfilt_plot_2 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
          NULL
        }
      )
    }
  },
  ignoreInit = TRUE
)

# imd anova filter plot
observeEvent(c(input$plot_imdanovafilt, input$min_nonmiss_anova, input$min_nonmiss_gtest),
  {
    revals$warnings_filter$imdanova_plot <- revals$warnings_filter$imdanova_plot_2 <- NULL

    plots$filter_mainplot <- tryCatch(
      {
        plot(imdanova_filter(objects$uploaded_omicsData), min_nonmiss_anova = input$min_nonmiss_anova, min_nonmiss_gtest = input$min_nonmiss_gtest, bw_theme = TRUE)
      },
      error = function(e) {
        msg <- paste0("Something went wrong making your iMd-ANOVA filter plot \n System error: ", e)
        revals$warnings_filter$imdanova_plot <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        NULL
      }
    )

    if (!is.null(objects$uploaded_omicsData_2)) {
      plots$filter_mainplot_2 <- tryCatch(
        {
          plot(imdanova_filter(objects$uploaded_omicsData_2), min_nonmiss_anova = input$min_nonmiss_anova, min_nonmiss_gtest = input$min_nonmiss_gtest, bw_theme = TRUE)
        },
        error = function(e) {
          msg <- paste0("Something went wrong making your second iMd-ANOVA filter plot \n System error: ", e)
          revals$warnings_filter$imdanova_plot_2 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
          NULL
        }
      )
    }
  },
  ignoreInit = TRUE
)

####

##########################
# apply plot styling to...
##########################

# ...first plot...
observeEvent(input$filter_apply_style_plot_1, {
  filter_xangle <- if (is_empty(input$filter_xangle) | is.na(input$filter_xangle)) 0 else input$filter_xangle
  filter_yangle <- if (is_empty(input$filter_yangle) | is.na(input$filter_yangle)) 0 else input$filter_yangle

  theme <- theme(
    axis.title.x = element_text(size = input$filter_x_fontsize),
    axis.title.y = element_text(size = input$filter_y_fontsize),
    axis.text.x = element_text(angle = filter_xangle, size = input$filter_x_ticksize),
    axis.text.y = element_text(angle = filter_yangle, size = input$filter_y_ticksize),
    plot.title = element_text(size = input$filter_title_fontsize)
  )

  if (inherits(plots$filter_mainplot, "list")) {
    plots$filter_mainplot[[1]] <- plots$filter_mainplot[[1]] + xlab(input$filter_xlab) + ylab(input$filter_ylab) + ggtitle(input$filter_title) + theme
  }
  else {
    plots$filter_mainplot <- plots$filter_mainplot + xlab(input$filter_xlab) + ylab(input$filter_ylab) + ggtitle(input$filter_title) + theme
  }
})

# ...second plot
observeEvent(input$filter_apply_style_plot_2, {
  filter_xangle <- if (is_empty(input$filter_xangle) | is.na(input$filter_xangle)) 0 else input$filter_xangle
  filter_yangle <- if (is_empty(input$filter_yangle) | is.na(input$filter_yangle)) 0 else input$filter_yangle

  theme <- theme(
    axis.title.x = element_text(size = input$filter_x_fontsize),
    axis.title.y = element_text(size = input$filter_y_fontsize),
    axis.text.x = element_text(angle = filter_xangle, size = input$filter_x_ticksize),
    axis.text.y = element_text(angle = filter_yangle, size = input$filter_y_ticksize),
    plot.title = element_text(size = input$filter_title_fontsize)
  )

  if (inherits(plots$filter_mainplot, "list")) {
    plots$filter_mainplot[[2]] <- plots$filter_mainplot[[2]] + xlab(input$filter_xlab) + ylab(input$filter_ylab) + ggtitle(input$filter_title) + theme
  }
  else {
    plots$filter_mainplot_2 <- plots$filter_mainplot_2 + xlab(input$filter_xlab) + ylab(input$filter_ylab) + ggtitle(input$filter_title) + theme
  }
})

#####

# filter review modal
observeEvent(input$review_filters, {
  showModal(
    modalDialog(
      tagList(
        uiOutput("filter_review"),
        uiOutput("execute_apply_filters_UI"),
        hr(),
        uiOutput("warnings_filter_modal")
      ),
      footer = NULL,
      size = "l"
    )
  )
})

#################
# Apply Filters #
#################
observeEvent(input$apply_filters, {
  req(!is.null(objects$uploaded_omicsData))
  
  # stats results are no longer valid, remove them
  objects$imdanova_res <- NULL
  
  tryCatch(
    {
      # make temp objects and clear summaries
      tmp <- objects$uploaded_omicsData
      tmp2 <- objects$uploaded_omicsData_2
      revals$filter_summary <- revals$filter_summary_2 <- NULL
      # molecule filter
      if (!is.null(objects$filters$molfilt) & is.null(attributes(tmp)$filters$moleculeFilt)) {
        tmp <- applyFilt(objects$filters$molfilt, tmp, min_num = input$mol_min_num)
        if (!is.null(objects$filters$molfilt_2) & is.null(attributes(tmp2)$filters$moleculeFilt)) {
          tmp2 <- applyFilt(objects$filters$molfilt_2, tmp2, min_num = input$mol_min_num)
        }
      }
      # proteomics filter
      if (class(tmp) == "pepData") {
        if (isTRUE(!is.null(objects$filters$profilt) & is.null(attributes(tmp)$filters$proteomicsFilt))) {
          tmp <- applyFilt(objects$filters$profilt, tmp, min_num_peps = input$min_num_peps, degen_peps = input$degen_peps)
        }
      }
      # cv filter
      if (!is.null(objects$filters$cvfilt) & is.null(attributes(tmp)$filters$cvFilt)) {
        tmp <- applyFilt(objects$filters$cvfilt, tmp, cv_threshold = input$cv_threshold)
        if (!is.null(objects$filters$cvfilt_2) & !is.null(tmp2) & is.null(attributes(tmp2)$filters$cvFilt)) {
          tmp2 <- applyFilt(objects$filters$cvfilt_2, tmp2, cv_threshold = input$cv_threshold)
        }
      }
      # imd filter
      if (!is.null(objects$filters$imdanovafilt) & is.null(attributes(tmp)$filters$imdanovaFilt)) {
        tmp <- applyFilt(objects$filters$imdanovafilt, tmp, min_nonmiss_anova = input$min_nonmiss_anova, min_nonmiss_gtest = input$min_nonmiss_gtest)
        if (!is.null(objects$filters$imdanovafilt_2) & !is.null(tmp2) & is.null(attributes(tmp2)$filters$imdanovaFilt)) {
          tmp2 <- applyFilt(objects$filters$imdanovafilt_2, tmp2, min_nonmiss_anova = input$min_nonmiss_anova, min_nonmiss_gtest = input$min_nonmiss_gtest)
        }
      }

      #### SAMPLE FILTERS ####

      # rMd filter
      if (!is.null(objects$filters$rmdfilt) & is.null(attributes(tmp)$filters$rmdFilt)) {
        tmp <- applyFilt(objects$filters$rmdfilt, tmp, pvalue_threshold = input$pvalue_threshold)
        if (!is.null(objects$filters$rmdfilt_2) & !is.null(tmp2)) {
          tmp2 <- applyFilt(objects$filters$rmdfilt_2, tmp2, pvalue_threshold = input$pvalue_threshold)
        }
      }

      # custom filter samples
      if (!is.null(objects$filters$customfilt)) {
        # get the intersect in case these samples were removed by previous sample filters e.g. rMd
        drop_samples1 <- intersect(objects$filters$customfilt$f_data_remove, tmp$f_data[, get_fdata_cname(tmp)])
        if (length(drop_samples1) > 0) tmp <- applyFilt(custom_filter(tmp, f_data_remove = drop_samples1), tmp)
      }
      if (!is.null(objects$filters$customfilt_2) & !is.null(tmp2)) {
        drop_samples2 <- intersect(objects$filters$customfilt_2$f_data_remove, tmp2$f_data[, get_fdata_cname(tmp)])
        if (length(drop_samples2) > 0) tmp2 <- applyFilt(custom_filter(tmp2, f_data_remove = drop_samples2), tmp2)
      }

      # store saved objects and remove temp objects, tmp2 will be null if there is only one dataset
      objects$omicsData <- tmp
      objects$omicsData_2 <- tmp2
      revals$filter_summary <- summary(tmp)
      if (!is.null(tmp2)) revals$filter_summary_2 <- summary(tmp2)
      rm(tmp, tmp2)
      res <- NULL
    },
    error = function(e) {
      res <<- paste0("Something went wrong applying your filters:  \n System error:  ", e)
    }
  )

  # two logicals that are TRUE is something went wrong, used to determine if success modal appears
  error_msg <- is.character(res)
  if (!is.null(objects$uploaded_omicsData_2)) {
    empty_object <- nrow(objects$omicsData$e_data) == 0 | nrow(objects$omicsData_2$e_data) == 0
  }
  else {
    empty_object <- nrow(objects$omicsData$e_data) == 0
  }

  # clear old error messages
  revals$warnings_filter$badfilter <- revals$warnings_filter$empty_object <- NULL

  # store new error messages if something went wrong, else show the success modal
  if (error_msg) {
    revals$warnings_filter$badfilter <- sprintf("<p style = 'color:red'>%s</p>", res)
  }
  else if (empty_object) {
    revals$warnings_filter$empty_object <- sprintf("<p style = 'color:red'>%s</p>", "These filters removed all molecules in one or more datasets, please revise")
  }
  else {
    revals$warnings_filter$badfilter <- NULL

    # collapse the filter panels for space to view tables
    updateCollapse(session, "filter_collapse", close = c("data_filters", "sample_filters"))

    # text summarizing the effect of filters...

    cond_text1 <- if (is.null(objects$omicsData_2)) "" else "Object1:  "
    cond_text2 <- if (is.null(objects$omicsData_2)) "" else "Object2:  "

    filters1 <- filter_names %>% # filter_names is a global variable defined in server.R
      filter(attribute %in% names(attributes(objects$omicsData)$filters)) %>%
      pluck("text") %>%
      paste(collapse = ", ")

    # ... second object
    if (!is.null(objects$omicsData_2)) {
      filters2 <- filter_names %>%
        filter(attribute %in% names(attributes(objects$omicsData_2)$filters)) %>%
        pluck("text") %>%
        paste(collapse = ", ")
      filters2_div <- tagList(HTML(paste0(cond_text2, filters2)), hr())
    } else {
      filters2 <- filters2_div <- NULL
    }

    showModal(
      modalDialog(
        title = "Filters Applied",
        fluidRow(
          column(10,
            align = "center", offset = 1,
            HTML(sprintf('<h4 style= "color:#1A5276">The following filters were applied to your data:</h4>')),
            HTML(paste0(cond_text1, filters1)),
            hr(),
            filters2_div,
            actionButton("filter_dismiss", "Stay on this tab", width = "75%"),
            actionButton("goto_norm", "Continue to normalization", style = "margin-top:5px;width:75%")
          )
        ),
        footer = NULL
      )
    )
  }

  #### CUSTOM FILTER UNDER (mental) CONSTRUCTION #####
})

#'@details enable the apply button if applying filters would cause a reset of
#'stats or normalization
observeEvent(input$allow_reapply_filters, {
  shinyjs::enable("apply_filters")
}, ignoreInit = T)

###

observeEvent(reactiveValuesToList(objects), {
  req(!is.null(objects$filters))
  cond_molfilts <- any(c("molfilt", "cvfilt", "imdanovafilt", "profilt") %in% names(objects$filters))
  cond_sampfilts <- any(c("rmdfilt", "customfilt") %in% names(objects$filters))

  toggleElement("ok_data_filters", condition = cond_molfilts, anim = TRUE)
  toggleElement("ok_sample_filters", condition = cond_sampfilts, anim = TRUE)
})

observeEvent(input$goto_norm, {
  updateTabsetPanel(session, "top_page", selected = "Normalization")
  removeModal()
})

observeEvent(input$filter_dismiss, {
  removeModal()
})
