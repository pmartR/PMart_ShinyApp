#'@details depending on the type of comparison the user wants to do, edit the 
#' table that presents the group comparisons
observe({
  req(input$imdanova_comparison_method, objects$omicsData)
  
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
  req(!is.null(objects$omicsData), input$top_page == "Analysis")
  
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
    },
    error = function(e) {
      msg <- paste0("Something went wrong running the analysis.  \n System error:  ", e)
      message(msg)
      revals$warnings_analysis$bad_imdanova <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      objects$imdanova_res <- NULL
      plots$analysis_mainplot <- NULL
    }
  )
})

# make plot object
observeEvent(c(objects$imdanova_res, input$imdanova_plot_type), {
  req(!is.null(objects$imdanova_res))
  tryCatch(
    {
      plots$analysis_mainplot <- plot(objects$imdanova_res, plot_type = input$imdanova_plot_type, bw_theme = TRUE)
      updateCollapse(session, "analysis_collapse_main", open = "analysis_plots")
    },
    error = function(e) {
      msg <- paste0("Something went wrong plotting your imdanovaRes object.  \n System error:  ", e)
      message(msg)
      revals$warnings_analysis$bad_imdanova_plot <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      plots$analysis_mainplot <- NULL
    }
  )
})

# apply plot styling to...

# ...first plot...
observeEvent(input$analysis_apply_style_plot_1, {
  analysis_xangle <- if (is_empty(input$analysis_xangle) | is.na(input$analysis_xangle)) 0 else input$analysis_xangle
  analysis_yangle <- if (is_empty(input$analysis_yangle) | is.na(input$analysis_yangle)) 0 else input$analysis_yangle

  theme <- theme(
    axis.title.x = element_text(size = input$analysis_x_fontsize),
    axis.title.y = element_text(size = input$analysis_y_fontsize),
    axis.text.x = element_text(angle = analysis_xangle, size = input$analysis_x_ticksize),
    axis.text.y = element_text(angle = analysis_yangle, size = input$analysis_y_ticksize),
    plot.title = element_text(size = input$analysis_title_fontsize)
  )

  if (inherits(plots$analysis_mainplot, "list")) {
    plots$analysis_mainplot[[1]] <- plots$analysis_mainplot[[1]] + xlab(input$analysis_xlab) + ylab(input$analysis_ylab) + ggtitle(input$analysis_title) + theme
  }
  else {
    plots$analysis_mainplot <- plots$analysis_mainplot + xlab(input$analysis_xlab) + ylab(input$analysis_ylab) + ggtitle(input$analysis_title) + theme
  }
})

# ...second plot
observeEvent(input$analysis_apply_style_plot_2, {
  analysis_xangle <- if (is_empty(input$analysis_xangle) | is.na(input$analysis_xangle)) 0 else input$analysis_xangle
  analysis_yangle <- if (is_empty(input$analysis_yangle) | is.na(input$analysis_yangle)) 0 else input$analysis_yangle

  theme <- theme(
    axis.title.x = element_text(size = input$analysis_x_fontsize),
    axis.title.y = element_text(size = input$analysis_y_fontsize),
    axis.text.x = element_text(angle = analysis_xangle, size = input$analysis_x_ticksize),
    axis.text.y = element_text(angle = analysis_yangle, size = input$analysis_y_ticksize),
    plot.title = element_text(size = input$analysis_title_fontsize)
  )

  if (inherits(plots$analysis_mainplot, "list")) {
    plots$analysis_mainplot[[2]] <- plots$analysis_mainplot[[2]] + xlab(input$analysis_xlab) + ylab(input$analysis_ylab) + ggtitle(input$analysis_title) + theme
  }
  else {
    plots$analysis_mainplot_2 <- plots$analysis_mainplot_2 + xlab(input$analysis_xlab) + ylab(input$analysis_ylab) + ggtitle(input$analysis_title) + theme
  }
})
