#'@details depending on the type of comparison the user wants to do, edit the 
#' table that presents the group comparisons
observe({
  req(input$peptide_imdanova_comparison_method, objects$omicsData)
  req(!is.null(attr(objects$omicsData,"group_DF")))

  if (input$peptide_imdanova_comparison_method == "Control to test condition comparisons") {
    control <- input$peptide_imdanova_control_group
    noncontrol <- input$peptide_imdanova_non_control_groups
    
    req(!is.null(control) && !is.null(noncontrol))
    
    combos <- t(combn(c(control, noncontrol), 2))
    if (nrow(combos) > 1) {
      combos <- as.matrix(combos[combos[, 1] == control, ], ncol = 2)
    }
  } else if (input$peptide_imdanova_comparison_method == "Custom comparisons") {
    combos <- input$peptide_imdanova_custom_comps
    
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
#' 
#' See statistics observers for implementation
#' 
# observeEvent(get_swap_vals(), {
#   res <- get_swap_vals()
#   req(any(res > 0))
#   
#   row <- as.numeric(names(which(res > 0)))
#   a <- comp_df_holder$comp_df[row, 1]
#   b <- comp_df_holder$comp_df[row, 2]
#   comp_df_holder$comp_df[row, 1] <- b
#   comp_df_holder$comp_df[row, 2] <- a
# })

# make statres object
observeEvent(input$peptide_apply_imdanova, {
  req(!is.null(objects$omicsData), input$top_page == "peptide_statistics_tab" &&
        !is.null(input$peptide_imdanova_test_method) &&
        !is.null(comp_df_holder$comp_df))
  
  shinyjs::show("peptide_analysis_busy")
  on.exit(hide("peptide_analysis_busy"))
  
  revals$warnings_peptide_statistics$bad_imdanova <- NULL
  
  tryCatch(
    {
      comps <- as.data.frame(comp_df_holder$comp_df)[1:2]
      colnames(comps) <- c("Control", "Test")
      
      pval_adjust_a = if (!is.null(input$peptide_imdanova_pval_adjust_a)) {
        input$peptide_imdanova_pval_adjust_a
      } else "none"
      
      pval_adjust_g = if (!is.null(input$peptide_imdanova_pval_adjust_g)) {
        input$peptide_imdanova_pval_adjust_g
      } else "none"
      
      objects$peptide_imdanova_res <- imd_anova(
        objects$omicsData, 
        comparisons = comps,
        test_method = input$peptide_imdanova_test_method, 
        pval_adjust_a = pval_adjust_a,
        pval_adjust_g = pval_adjust_g,
        pval_thresh = input$peptide_pval_thresh
      )
      
      if(is.null(objects$omicsData$e_meta)){
        buttons <- div(
          actionButton("pepstats_dismiss", "Review results", width = "75%"),
          br(), 
          actionButton("pep_goto_downloads", "Continue to Download Tab", width = "75%")
        )
      } else {
        buttons <- div(
          actionButton("pepstats_dismiss", "Review results", width = "75%"),
          br(), 
          actionButton("goto_rollup", "Continue to Protein Rollup Tab", width = "75%"),
          br(), 
          actionButton("pep_goto_downloads", "Continue to Download Tab", width = "75%")
        )
      }
      
      pval_adjust_modal_text <- switch(
        input$peptide_imdanova_test_method,
        "combined" = sprintf("ANOVA: %s, G-test: %s", str_to_title(pval_adjust_g), str_to_title(pval_adjust_a)),
        "anova" = str_to_title(pval_adjust_a),
        "gtest" = str_to_title(pval_adjust_g)
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
                         input$peptide_imdanova_test_method,
                         " test method from pmartR's iMd-ANOVA function. ",
                         "Multiple comparisons P-value correction peformed: ",
                         pval_adjust_modal_text,
                         ". P-value threshold: ",
                         input$peptide_pval_thresh
                       )
                       ),
                     hr(),
                     buttons
              )
            )
          },
          footer = NULL
        )
      )
      
      # Show success checkmarks and update collapse panels if everything succeeds.
      show("peptide_stats-statistics-ok")
      show("peptide_imdanova_groups_ok")
      show("peptide_imdanova_settings_ok")
      
      updateCollapse(session, "peptide_statistics_collapse_left", close = c("peptide_stats-statistics-options"))
      updateCollapse(session, "peptide_imdanova-sidepanel-options", 
                     close = c("peptide_imdanova-specify-comparisons", "peptide_imdanova-select-settings"))
      
    },
    error = function(e) {
      msg <- paste0("Something went wrong running the statistics.  \n System error:  ", e)
      message(msg)
      revals$warnings_peptide_statistics$bad_imdanova <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      objects$peptide_imdanova_res <- NULL
      plots$peptide_statistics_mainplot <- NULL
    }
  )
})

observeEvent(input$pepstats_dismiss, removeModal())
observeEvent(input$goto_rollup,{
  updateTabsetPanel(session, "top_page", selected = "protein_rollup_tab")
  removeModal()
})
observeEvent(input$pep_goto_downloads,{
  updateTabsetPanel(session, "top_page", selected = "download_tab")
  removeModal()
})

#'@details when imd_anova_res object is created or the style is updated, make a 
#'plot of the object and open the panel that displays it.
observeEvent(
  c(
    objects$peptide_imdanova_res,
    input$peptide_imdanova_plot_type,
    input$peptide_stats_update_plot_content
  ),
  {
    req(!is.null(objects$peptide_imdanova_res))
    
    interactive = !is.null(input$peptide_stats_interactive_yn) && 
      as.logical(input$peptide_stats_interactive_yn)
    
    fc_colors = if (all(map_lgl(
      list(
        input$pep_imd_down_cpicker,
        input$pep_imd_nonsig_cpicker,
        input$pep_imd_up_cpicker
      ),
      isTruthy
    ))) {
      c(
        input$pep_imd_down_cpicker,
        input$pep_imd_nonsig_cpicker,
        input$pep_imd_up_cpicker
      )
    } else
      c("red", "black", "green")
    
    color_low = if(isTruthy(input$pep_imd_low_cpicker)) input$pep_imd_low_cpicker else "#132B43"
    color_high = if(isTruthy(input$pep_imd_high_cpicker)) input$pep_imd_high_cpicker else "#56B1F7"
    
    revals$warnings_peptide_statistics$bad_imdanova_plot <- NULL
    
    tryCatch({
      plots$peptide_statistics_mainplot <-
        plot(
          objects$peptide_imdanova_res,
          fc_threshold = input$peptide_imd_plot_fc_thresh,
          fc_colors = fc_colors,
          plot_type = input$peptide_imdanova_plot_type,
          interactive = interactive,
          bw_theme = TRUE,
          color_low = color_low,
          color_high = color_high
        )
      updateCollapse(session, "peptide_statistics_collapse_main", open = "peptide_statistics_plots")
    },
    error = function(e) {
      msg <-
        paste0("Something went wrong plotting your imdanovaRes object.  \n System error:  ",
               e)
      message(msg)
      revals$warnings_peptide_statistics$bad_imdanova_plot <<-
        sprintf("<p style = 'color:red'>%s</p>", msg)
      plots$peptide_statistics_mainplot <- NULL
    })
  },
  priority = 5
)

# apply plot styling to...

# ...first plot...
observeEvent(input$peptide_statistics_apply_style_plot_1, {
  
  comps <- get_comparisons(objects$peptide_imdanova_res)
  
  if (inherits(plots$peptide_statistics_mainplot, "list")) {
    plots$peptide_statistics_mainplot[[1]] <- add_plot_styling(
      input,
      "peptide_statistics", 
      plots$peptide_statistics_mainplot[[1]],
      subplot = nrow(comps) > 1
      )
}
  else {
    plots$peptide_statistics_mainplot <- add_plot_styling(
      input,
      "peptide_statistics", 
      plots$peptide_statistics_mainplot,
      subplot = nrow(comps) > 1
    )
}
})

# ...second plot
observeEvent(input$peptide_statistics_apply_style_plot_2, {
  
  comps <- get_comparisons(objects$peptide_imdanova_res)
  
  if (inherits(plots$peptide_statistics_mainplot, "list")) {
    plots$peptide_statistics_mainplot[[2]] <- add_plot_styling(
      input,
      "peptide_statistics", 
      plots$peptide_statistics_mainplot[[2]],
      subplot = nrow(comps) > 1
    )
  }
  else {
    plots$peptide_statistics_mainplot_2 <- add_plot_styling(
      input,
      "peptide_statistics", 
      plots$peptide_statistics_mainplot_2,
      subplot = nrow(comps) > 1
    )
  }
})

#'@details disable the imd-anova button if we are not analyzing pepData.  If the
#'data has been rolled up, show a tooltip explaining as such.
# disable inputs if we are not working with pepdata
observeEvent(c(objects$omicsData, input$top_page, input$peptide_apply_imdanova), {
  ids <- c(
    "peptide_apply_imdanova"
  )
  
  req(!is.null(objects$omicsData))
  for (el in ids) {
    toggleState(el, condition = inherits(objects$omicsData, "pepData"))
  }
  
  is_rolled_up <- inherits(objects$uploaded_omicsData, "pepData") & inherits(objects$omicsData, "proData")
  toggleTooltip(session, "peptide_apply_imdanova_jswrapper", is_rolled_up, tooltip_text = ttext_[["ROLLUP_DISABLE_INFO"]])
  
})
