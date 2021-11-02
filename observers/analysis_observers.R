#'@details depending on the type of comparison the user wants to do, edit the 
#' table that presents the group comparisons
observe({
  req(input$imdanova_comparison_method, objects$omicsData)
  req(!is.null(attr(objects$omicsData,"group_DF")))
  
  if (input$imdanova_comparison_method == "Control to test condition comparisons") {
    control <- input$imdanova_control_group
    noncontrol <- input$imdanova_non_control_groups
    
    req(!is.null(control) && !is.null(noncontrol))
    
    combos <- t(combn(c(control, noncontrol), 2))
    if (nrow(combos) > 1) {
      combos <- as.matrix(combos[combos[, 1] == control, ], ncol = 2)
    }
  } else if (input$imdanova_comparison_method == "Custom comparisons") {
    combos <- input$imdanova_custom_comps
    
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
  
  tryCatch(
    {
      comps <- as.data.frame(comp_df_holder$comp_df)[1:2]
      colnames(comps) <- c("Control", "Test")
      objects$imdanova_res <- imd_anova(
        objects$omicsData, 
        comparisons = comps,
        test_method = input$imdanova_test_method, 
        pval_adjust = input$pval_adjust, 
        pval_thresh = input$pval_thresh
      )
      
      show("stats-statistics-ok")
      show("imdanova_groups_ok")
      show("imdanova_settings_ok")

      updateCollapse(session, "statistics_collapse_left", close = c("stats-statistics-options"))
      updateCollapse(session, "imdanova-sidepanel-options", 
                     close = c("imdanova-specify-comparisons", "imdanova-select-settings"))

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
                         " test method from pmartR's iMd-ANOVA function. ",
                         "Multiple comparisons P-value correction peformed: ",
                         switch(input$pval_adjust,
                                       "tukey" = "Tukey", 
                                       "dunnet" = "Dunnett",
                                       "holm" = "Holm",
                                       "bonferroni" = "Bonferroni",
                                       "none" = "none"
                         ),
                         ". P-value threshold: ",
                         input$pval_thresh
                       )
                     ),
                     hr(),
                     actionButton("stats_dismiss", "Review results", width = "75%"),
                     actionButton("goto_downloads", "Continue to Download Tab", width = "75%")
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
      revals$warnings_statistics$bad_imdanova <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      objects$imdanova_res <- NULL
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
    
    tryCatch({
      plots$statistics_mainplot <- plot(
        objects$imdanova_res,
        plot_type = input$imdanova_plot_type,
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
        paste0("Something went wrong plotting your imdanovaRes object.  \n System error:  ",
               e)
      message(msg)
      revals$warnings_statistics$bad_imdanova_plot <<-
        sprintf("<p style = 'color:red'>%s</p>", msg)
      plots$statistics_mainplot <- NULL
    })
  }
)

# apply plot styling to...

# ...first plot...
observeEvent(input$statistics_apply_style_plot_1, {
  
  comps <- get_comparisons(objects$imdanova_res)
  
  if (inherits(plots$statistics_mainplot, "list")) {
    plots$statistics_mainplot[[1]] <- add_plot_styling(
      input,
      "statistics", 
      plots$statistics_mainplot[[1]],
      subplot = nrow(comps) > 1
    )
 }
  else {
    plots$statistics_mainplot <- add_plot_styling(
      input,
      "statistics", 
      plots$statistics_mainplot,
      subplot = nrow(comps) > 1
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
