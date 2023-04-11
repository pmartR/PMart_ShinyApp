#'@details depending on the type of comparison the user wants to do, edit the 
#' table that presents the group comparisons
observe({
  req(input$comparison_method, objects$omicsData)
  req(!is.null(attr(objects$omicsData,"group_DF")))
  
  if (input$comparison_method == "Control to test condition comparisons") {
    control <- input$control_group
    noncontrol <- input$imdanova_non_control_groups
    
    req(!is.null(control) && !is.null(noncontrol))
    
    combos <- t(combn(c(control, noncontrol), 2))
    if (nrow(combos) > 1) {
      combos <- as.matrix(combos[combos[, 1] == control, ], ncol = 2)
    }
  } else if (input$comparison_method == "Custom comparisons") {
    combos <- input$custom_comps
    
    req(!is.null(combos))
    
    combos <- str_split(combos, ", ", simplify = TRUE)
  } else {
    groups <- isolate(objects$omicsData) %>%
      pmartR:::get_group_table()
    groups <- groups[groups > 1] %>% names()
    combos <- t(combn(groups, 2))
  }
  
  isolate(comp_df_holder$comp_df <- data.frame(
    `Comparison` = as.character(combos[, 2]),
    `Control` = as.character(combos[, 1]),
    ` ` = buttonInput(
      FUN = actionButton,
      len = nrow(combos),
      id = "imd_comparison_button_",
      onclick = 'Shiny.onInputChange(\"lastClick\",  this.id)',
      label = "Swap",
      style = "padding:4px; font-size:80%"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  ))
  
  isolate(row.names(comp_df_holder$comp_df) <- NULL)
})

#'@details Swaps the comparison for a particular row in comp_df_holder.
#' The value of the clicked 'swap' button inside the datatable temporarily 
#' changes to 1 before resetting when the UI is redrawn.  The row where that
#' clicked button lives has its columns swapped.
observeEvent(get_swap_vals(), {
  res <- get_swap_vals()
  req(any(res > 0))
  
  row <- as.numeric(names(which(res > 0)))
  a <- comp_df_holder$comp_df[row, 1]
  b <- comp_df_holder$comp_df[row, 2]
  comp_df_holder$comp_df[row, 1] <- b
  comp_df_holder$comp_df[row, 2] <- a
})

# make statres object
observeEvent(input$apply_imdanova, {
  req(!is.null(objects$omicsData) && 
        input$top_page == "statistics_tab" && 
        !is.null(comp_df_holder$comp_df))
  
  shinyjs::show("analysis_busy")
  on.exit(hide("analysis_busy"))
  
  revals$warnings_statistics$bad_imdanova <- NULL
  
  tryCatch(
    {
      comps <- as.data.frame(comp_df_holder$comp_df)[1:2]
      colnames(comps) <- c("Test", "Control")
      
      pval_adjust_a_mc = if (!is.null(input$imdanova_pval_adjust_a_multcomp)) {
        input$imdanova_pval_adjust_a_multcomp
      } else "none"
      
      pval_adjust_g_mc = if (!is.null(input$imdanova_pval_adjust_g_multcomp)) {
        input$imdanova_pval_adjust_g_multcomp
      } else "none"
      
      pval_adjust_a_fdr = if (!is.null(input$imdanova_pval_adjust_a_fdr)) {
        input$imdanova_pval_adjust_a_fdr
      } else "none"
      
      pval_adjust_g_fdr = if (!is.null(input$imdanova_pval_adjust_g_fdr)) {
        input$imdanova_pval_adjust_g_fdr
      } else "none"
      
      objects$imdanova_res <- imd_anova(
        objects$omicsData,
        comparisons = comps,
        test_method = input$imdanova_test_method,
        pval_adjust_a_multcomp = pval_adjust_a_mc,
        pval_adjust_g_multcomp = pval_adjust_g_mc,
        pval_adjust_a_fdr = pval_adjust_a_fdr,
        pval_adjust_g_fdr = pval_adjust_g_fdr,
        pval_thresh = input$pval_thresh
      )
      
      pval_adjust_mc_modal_text <- switch(
        input$imdanova_test_method,
        "combined" = sprintf("ANOVA: %s, G-test: %s", str_to_title(pval_adjust_g_mc), str_to_title(pval_adjust_a_mc)),
        "anova" = str_to_title(pval_adjust_a_mc),
        "gtest" = str_to_title(pval_adjust_g_mc)
      )

      pval_adjust_fdr_modal_text <- switch(
        input$imdanova_test_method,
        "combined" = sprintf("ANOVA: %s, G-test: %s", str_to_title(pval_adjust_g_fdr), str_to_title(pval_adjust_a_fdr)),
        "anova" = str_to_title(pval_adjust_a_fdr),
        "gtest" = str_to_title(pval_adjust_g_fdr)
      )
      
      # success modal if all is well
      showModal(
        modalDialog(
          {
            fluidRow(
              column(10,
                      align = "center", offset = 1,
                      tags$h4(
                        paste0(
                          "Statistical analysis has been performed using ",
                          input$imdanova_test_method,
                          " test method from pmartR's iMd-ANOVA function"
                        )
                      ),
                      tags$h4(
                        sprintf(
                          "Multiple comparisons P-value correction peformed: %s",
                          pval_adjust_mc_modal_text
                        ),
                      ),
                      tags$h4(
                        sprintf(
                          "FDR p-value adjustment correction performed: %s",
                          pval_adjust_fdr_modal_text
                        )                       
                      ),
                      tags$h4(
                        sprintf(
                          "P-value threshold: %s",
                          input$pval_thresh
                        )
                      ),
                      hr(),
                      actionButton("stats_dismiss", "Review results", width = "75%"),
                      actionButton("goto_downloads", "Continue to Download tab", width = "75%")
              )
            )
          },
          footer = NULL
        )
      )
      
      show("stats-statistics-ok")
      show("imdanova_groups_ok")
      show("imdanova_settings_ok")
      
      updateCollapse(session, "statistics_collapse_left", close = c("stats-statistics-options"))
      updateCollapse(session, "imdanova-sidepanel-options", 
                     close = c("imdanova-specify-comparisons", "imdanova-select-settings"))
      
    },
    error = function(e) {
      msg <- paste0("Something went wrong running the statistics.  \n System error:  ", e)
      message(msg)
      revals$warnings_statistics$bad_imdanova <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      objects$imdanova_res <- NULL
      plots$statistics_mainplot <- NULL
    }
  )
})


# make statres object
observeEvent(input$apply_diagnostic, {
  req(!is.null(objects$omicsData) && 
        input$top_page == "statistics_tab")
  
  shinyjs::show("analysis_busy")
  on.exit({
    hide("analysis_busy")
    enable("apply_seqstats")
    })
  
  revals$warnings_statistics$bad_seqdiag <- NULL
  
  tryCatch(
    {
      
      #### make as object, plot separately?
      objects$diagnostic_res <- dispersion_est(
        method = input$stats_select_method,
        omicsData = objects$omicsData
      )
      
    },
    error = function(e) {
      msg <- paste0("Something went wrong running the diagnostics  \n System error:  ", e)
      message(msg)
      revals$warnings_statistics$bad_seqdiag <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      objects$diagnostic_res <- NULL
      plots$statistics_diagplot <- NULL
    }
  )
})

# make statres object
observeEvent(input$apply_seqstats, {
  req(!is.null(objects$omicsData) && 
        input$top_page == "statistics_tab" && 
        !is.null(comp_df_holder$comp_df))
  
  shinyjs::show("analysis_busy")
  on.exit({
    hide("analysis_busy")
    enable("apply_seqstats")
  })
  
  revals$warnings_statistics$bad_seqstats <- NULL
  
  tryCatch(
    {
      comps <- as.data.frame(comp_df_holder$comp_df)[1:2]
      colnames(comps) <- c("Test", "Control")
      
      pval_adjust <- if (!is.null(input$seqdata_pval_adjust)) {
        input$seqdata_pval_adjust
      } else "none"
      
      objects$seqstats_res <- diffexp_seq(
        method = input$stats_select_method,
        omicsData = objects$omicsData,
        comparisons = comps,
        test_method = input$seqdata_test_method, ### _test_method for options
        p_adjust = pval_adjust,
        p_cutoff = input$pval_thresh
      )
      
      
      pval_adjust_modal_text <- str_to_title(pval_adjust)
      
      text_adjustment <- if(input$stats_select_method %in% c("DESeq2", "imdanova")){
        paste0("Statistical analysis has been performed using ",
               ifelse(is.null(input$seqdata_test_method), 
                      input$imdanova_test_method, 
                      input$seqdata_test_method),
               " test method from pmartR's ",
               ifelse(is.null(input$seqdata_test_method), 
                      "iMd-ANOVA", 
                      "DESeq2 wrapper functions."),
               " function. ")
      } else {
        paste0("Statistical analysis has been performed using ",
               switch(input$stats_select_method, edgeR = "empirical Bayes quasi-likelihood F-",
                      voom = "empirical Bayes moderated t-statistics "),
               "test method from pmartR's", input$stats_select_method, "wrapper functions. ")
      }
      
      # success modal if all is well
      showModal(
        modalDialog(
          {
            fluidRow(
              column(10,
                     align = "center", offset = 1,
                     tags$h4(
                       paste0(
                         text_adjustment,
                         "Multiple comparisons P-value correction peformed: ",
                         pval_adjust_modal_text,
                         ". P-value threshold: ",
                         input$pval_thresh
                       )
                     ),
                     hr(),
                     actionButton("stats_dismiss", "Review results", width = "75%"),
                     actionButton("goto_downloads", "Continue to Download tab", width = "75%")
              )
            )
          },
          footer = NULL
        )
      )
      
      show("stats-statistics-ok")
      show("seqdata_groups_ok")
      show("seqdata_settings_ok")
      
      updateCollapse(session, "statistics_collapse_left", close = c("stats-statistics-options"))
      updateCollapse(session, "seqdata-sidepanel-options", 
                     close = c("seqdata-specify-comparisons", "seqdata-select-settings"))
      
    },
    error = function(e) {
      msg <- paste0("Something went wrong running the statistics.  \n System error:  ", e)
      message(msg)
      revals$warnings_statistics$bad_seqstats <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      objects$seqstats_res <- NULL
      plots$statistics_mainplot <- NULL
    }
  )
})

observeEvent(input$stats_dismiss, removeModal())
observeEvent(input$goto_downloads,{
  updateTabsetPanel(session, "top_page", selected = "download_tab")
  removeModal()
})

#'@details when imd_anova_res object is created or the style is updated, make a 
#'plot of the object and open the panel that displays it.
observeEvent(
  c(
    objects$imdanova_res,
    input$imdanova_plot_type,
    input$stats_update_plot_content
  ),
  {
    req(!is.null(objects$imdanova_res))
    
    interactive <- !is.null(input$stats_interactive_yn) && 
      as.logical(input$stats_interactive_yn)
    
    fc_colors = if (all(map_lgl(
      list(
        input$imd_down_cpicker,
        input$imd_nonsig_cpicker,
        input$imd_up_cpicker
      ),
      isTruthy
    ))) {
      c(
        input$imd_down_cpicker,
        input$imd_nonsig_cpicker,
        input$imd_up_cpicker
      )
    } else
      c("red", "black", "green")

    color_low = if(isTruthy(input$imd_low_cpicker)) input$imd_low_cpicker else "#132B43"
    color_high = if(isTruthy(input$imd_high_cpicker)) input$imd_high_cpicker else "#56B1F7"
    
    revals$warnings_statistics$bad_imdanova_plot <- NULL
    
    color_low = if(isTruthy(input$imd_low_cpicker)) input$imd_low_cpicker else "#132B43"
    color_high = if(isTruthy(input$imd_high_cpicker)) input$imd_high_cpicker else "#56B1F7"
    
    revals$warnings_statistics$bad_imdanova_plot <- NULL
    
    tryCatch({
      plots$statistics_mainplot <- plot(
        objects$imdanova_res,
        plot_type = input$imdanova_plot_type,
        bw_theme = TRUE,
        stacked = FALSE,
        fc_colors = fc_colors,
        fc_threshold = input$imd_plot_fc_thresh,
        interactive = interactive,
        color_low = color_low,
        color_high = color_high
      )
      updateCollapse(session, "statistics_collapse_main", open = "statistics_plots")
    },
    error = function(e) {
      msg <-
        paste0("Something went wrong plotting your imdanovaRes object.  \n System error:  ",
               e)
      message(msg)
      revals$warnings_statistics$bad_imdanova_plot <<-
        sprintf("<p style = 'color:red'>%s</p>", msg)
      plots$statistics_mainplot <- NULL
    })
  }
)

#'@details when seqstats_res object is created or the style is updated, make a 
#'plot of the object and open the panel that displays it.
observeEvent(
  c(
    objects$seqstats_res,
    input$seqdata_plot_type,
    input$stats_update_plot_content
  ),
  {
    req(!is.null(objects$seqstats_res), cancelOutput = TRUE)
    
    interactive <- !is.null(input$stats_interactive_yn) && 
      as.logical(input$stats_interactive_yn)
    
    fc_colors = if (all(map_lgl(
      list(
        input$imd_down_cpicker,
        input$imd_nonsig_cpicker,
        input$imd_up_cpicker
      ),
      isTruthy
    ))) {
      c(
        input$imd_down_cpicker,
        input$imd_nonsig_cpicker,
        input$imd_up_cpicker
      )
    } else
      c("red", "black", "green")
    
    color_low = if(isTruthy(input$imd_low_cpicker)) input$imd_low_cpicker else "#132B43"
    color_high = if(isTruthy(input$imd_high_cpicker)) input$imd_high_cpicker else "#56B1F7"
    
    revals$warnings_statistics$bad_imdanova_plot <- NULL
    
    color_low = if(isTruthy(input$imd_low_cpicker)) input$imd_low_cpicker else "#132B43"
    color_high = if(isTruthy(input$imd_high_cpicker)) input$imd_high_cpicker else "#56B1F7"
    
    revals$warnings_statistics$bad_imdanova_plot <- NULL
    
    tryCatch({
      plots$statistics_mainplot <- plot(
        objects$seqstats_res,
        plot_type = input$seqdata_plot_type,
        bw_theme = TRUE,
        stacked = FALSE,
        fc_colors = fc_colors,
        fc_threshold = input$imd_plot_fc_thresh,
        interactive = interactive,
        color_low = color_low,
        color_high = color_high
      )
      updateCollapse(session, "statistics_collapse_main", open = "statistics_plots")
    },
    error = function(e) {
      msg <-
        paste0("Something went wrong plotting your statRes object.  \n System error:  ",
               e)
      message(msg)
      revals$warnings_statistics$bad_imdanova_plot <<-
        sprintf("<p style = 'color:red'>%s</p>", msg)
      plots$statistics_mainplot <- NULL
    })
  }
)


observeEvent(
  c(
    objects$seqstats_res,
    input$seqdata_plot_type,
    input$stats_update_plot_content
  ),
  {
    req(!is.null(objects$seqstats_res), cancelOutput = TRUE)
    
    interactive <- !is.null(input$stats_interactive_yn) && 
      as.logical(input$stats_interactive_yn)
    
    fc_colors = if (all(map_lgl(
      list(
        input$imd_down_cpicker,
        input$imd_nonsig_cpicker,
        input$imd_up_cpicker
      ),
      isTruthy
    ))) {
      c(
        input$imd_down_cpicker,
        input$imd_nonsig_cpicker,
        input$imd_up_cpicker
      )
    } else
      c("red", "black", "green")
    
    color_low = if(isTruthy(input$imd_low_cpicker)) input$imd_low_cpicker else "#132B43"
    color_high = if(isTruthy(input$imd_high_cpicker)) input$imd_high_cpicker else "#56B1F7"
    
    revals$warnings_statistics$bad_imdanova_plot <- NULL
    
    color_low = if(isTruthy(input$imd_low_cpicker)) input$imd_low_cpicker else "#132B43"
    color_high = if(isTruthy(input$imd_high_cpicker)) input$imd_high_cpicker else "#56B1F7"
    
    revals$warnings_statistics$bad_imdanova_plot <- NULL
    
    tryCatch({
      plots$statistics_mainplot <- plot(
        objects$seqstats_res,
        plot_type = input$seqdata_plot_type,
        bw_theme = TRUE,
        fc_colors = fc_colors,
        fc_threshold = input$imd_plot_fc_thresh,
        interactive = interactive,
        color_low = color_low,
        color_high = color_high
      )
      updateCollapse(session, "statistics_collapse_main", open = "statistics_plots")
    },
    error = function(e) {
      msg <-
        paste0("Something went wrong plotting your statRes object.  \n System error:  ",
               e)
      message(msg)
      revals$warnings_statistics$bad_imdanova_plot <<-
        sprintf("<p style = 'color:red'>%s</p>", msg)
      plots$statistics_mainplot <- NULL
    })
  }
)


observeEvent(
  c(
    objects$diagnostic_res
  ),
  {
    req(!is.null(objects$diagnostic_res))
    
    revals$warnings_statistics$bad_diag_plot <- NULL
    
    tryCatch({
      plots$statistics_diagplot <- objects$diagnostic_res
      updateCollapse(session, "statistics_collapse_main", open = "diagnotistic_plots")
    },
    error = function(e) {
      msg <-
        paste0("Something went wrong plotting your diagnostic object.  \n System error:  ",
               e)
      message(msg)
      revals$warnings_statistics$bad_diag_plot <<-
        sprintf("<p style = 'color:red'>%s</p>", msg)
      plots$statistics_diagplot <- NULL
    })
  }
)

# apply plot styling to...

# diagnostic plot ...
observeEvent(input$statistics_apply_style_diag, {
  if (inherits(plots$statistics_diagplot, "list")) {
    plots$statistics_diagplot[[1]] <- add_plot_styling(
      input,
      "statistics", 
      plots$statistics_diagplot[[1]],
      subplot = subplot
    )
  } else if (!is.null(plots$statistics_diagplot)) {
    plots$statistics_diagplot <- add_plot_styling(
      input,
      "statistics", 
      plots$statistics_diagplot,
      subplot = subplot
    )  
  }
})

# ...first plot...
observeEvent(input$statistics_apply_style_plot_1, {
  
  if(!is.null(objects$imdanova_res)){
    comps <- get_comparisons(objects$imdanova_res)
    subplot <- nrow(comps) > 1
  } else if(!is.null(objects$seqstats_res)) {
    comps <- get_comparisons(objects$seqstats_res)
    subplot <- nrow(comps) > 1
  } else {
    subplot <-  F
  }
  
  if (inherits(plots$statistics_mainplot, "list")) {
    plots$statistics_mainplot[[1]] <- add_plot_styling(
      input,
      "statistics", 
      plots$statistics_mainplot[[1]],
      subplot = subplot
    )
 }
  else if(!is.null(plots$statistics_mainplot)){
    plots$statistics_mainplot <- add_plot_styling(
      input,
      "statistics", 
      plots$statistics_mainplot,
      subplot = subplot
    )
 }
})

# ...second plot
observeEvent(input$statistics_apply_style_plot_2, {
  
  comps <- get_comparisons(objects$imdanova_res)
  
  if (inherits(plots$statistics_mainplot, "list")) {
    plots$statistics_mainplot[[2]] <- add_plot_styling(
      input,
      "statistics", 
      plots$statistics_mainplot[[2]],
      subplot = nrow(comps) > 1
    )
  }
  else {
    plots$statistics_mainplot_2 <- add_plot_styling(
      input,
      "statistics", 
      plots$statistics_mainplot_2,
      subplot = nrow(comps) > 1
    )
  }
})

#'@details Clear plots when stats method is changed
observeEvent(input$stats_select_method, {
  plots$statistics_mainplot <- plots$statistics_diagplot <- NULL
})
