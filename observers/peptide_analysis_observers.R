#'@details depending on the type of comparison the user wants to do, edit the 
#' table that presents the group comparisons
observe({
  req(input$peptide_imdanova_comparison_method, objects$omicsData)
  
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
        !is.null( input$peptide_imdanova_test_method))
  
  tryCatch(
    {
      comps <- as.data.frame(comp_df_holder$comp_df)[1:2]
      colnames(comps) <- c("Control", "Test")
      objects$peptide_imdanova_res <- imd_anova(
        objects$omicsData, 
        comparisons = comps,
        test_method = input$peptide_imdanova_test_method, 
        pval_adjust = input$peptide_pval_adjust, 
        pval_thresh = input$peptide_pval_thresh
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
                         switch(input$peptide_pval_adjust,
                                "tukey" = "Tukey", 
                                "dunnet" = "Dunnett",
                                "holm" = "Holm",
                                "bonferroni" = "Bonferroni",
                                "none" = "none"
                                ),
                         ". P-value threshold: ",
                         input$peptide_pval_thresh
                       )
                       ),
                     hr(),
                     actionButton("pepstats_dismiss", "Review results", width = "75%"),
                     actionButton("goto_rollup", "Continue to Protein Rollup Tab", width = "75%"),
                     actionButton("pep_goto_downloads", "Continue to Download Tab", width = "75%")
              )
            )
          },
          footer = NULL
        )
      )
      
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
    
    interactive = if (is.null(input$peptide_stats_interactive_yn)){
      FALSE
    } else as.logical(input$peptide_stats_interactive_yn)
    
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
    
    tryCatch({
      if (input$peptide_imdanova_plot_type == "volcano") {
        temp <- objects$peptide_imdanova_res
        plots$peptide_statistics_mainplot <-
          plot(
            temp,
            fc_threshold = input$peptide_imd_plot_fc_thresh,
            fc_colors = fc_colors,
            plot_type = input$peptide_imdanova_plot_type,
            interactive = interactive,
            bw_theme = TRUE
          )
      } else {
        plots$peptide_statistics_mainplot <-
          plot(
            objects$peptide_imdanova_res,
            fc_colors = fc_colors,
            plot_type = input$peptide_imdanova_plot_type,
            interactive = interactive,
            bw_theme = TRUE
          )
      }
      
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
  peptide_statistics_xangle <- if (is_empty(input$peptide_statistics_xangle) | is.na(input$peptide_statistics_xangle)) 0 else input$peptide_statistics_xangle
  peptide_statistics_yangle <- if (is_empty(input$peptide_statistics_yangle) | is.na(input$peptide_statistics_yangle)) 0 else input$peptide_statistics_yangle
  
  theme <- theme(
    axis.title.x = element_text(size = input$peptide_statistics_x_fontsize),
    axis.title.y = element_text(size = input$peptide_statistics_y_fontsize),
    axis.text.x = element_text(angle = peptide_statistics_xangle, size = input$peptide_statistics_x_ticksize),
    axis.text.y = element_text(angle = peptide_statistics_yangle, size = input$peptide_statistics_y_ticksize),
    plot.title = element_text(size = input$peptide_statistics_title_fontsize)
  )
  
  if (inherits(plots$peptide_statistics_mainplot, "list")) {
    plots$peptide_statistics_mainplot[[1]] <- plots$peptide_statistics_mainplot[[1]] + xlab(input$peptide_statistics_xlab) + ylab(input$peptide_statistics_ylab) + ggtitle(input$peptide_statistics_title) + theme
  }
  else {
    plots$peptide_statistics_mainplot <- plots$peptide_statistics_mainplot + xlab(input$peptide_statistics_xlab) + ylab(input$peptide_statistics_ylab) + ggtitle(input$peptide_statistics_title) + theme
  }
})

# ...second plot
observeEvent(input$peptide_statistics_apply_style_plot_2, {
  peptide_statistics_xangle <- if (is_empty(input$peptide_statistics_xangle) | is.na(input$peptide_statistics_xangle)) 0 else input$peptide_statistics_xangle
  peptide_statistics_yangle <- if (is_empty(input$peptide_statistics_yangle) | is.na(input$peptide_statistics_yangle)) 0 else input$peptide_statistics_yangle
  
  theme <- theme(
    axis.title.x = element_text(size = input$peptide_statistics_x_fontsize),
    axis.title.y = element_text(size = input$peptide_statistics_y_fontsize),
    axis.text.x = element_text(angle = peptide_statistics_xangle, size = input$peptide_statistics_x_ticksize),
    axis.text.y = element_text(angle = peptide_statistics_yangle, size = input$peptide_statistics_y_ticksize),
    plot.title = element_text(size = input$peptide_statistics_title_fontsize)
  )
  
  if (inherits(plots$peptide_statistics_mainplot, "list")) {
    plots$peptide_statistics_mainplot[[2]] <- plots$peptide_statistics_mainplot[[2]] + xlab(input$peptide_statistics_xlab) + ylab(input$peptide_statistics_ylab) + ggtitle(input$peptide_statistics_title) + theme
  }
  else {
    plots$peptide_statistics_mainplot_2 <- plots$peptide_statistics_mainplot_2 + xlab(input$peptide_statistics_xlab) + ylab(input$peptide_statistics_ylab) + ggtitle(input$peptide_statistics_title) + theme
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
