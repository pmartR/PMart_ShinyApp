# disable transforming to a scale which the object is already on
observeEvent(c(input$transform, objects$omicsData, objects$omicsData_2), {
  req(!is.null(objects$omicsData))
  if (!is.null(objects$omicsData_2)) {
    cond_scale <- all(c(attr(objects$omicsData, "data_info")$data_scale, attr(objects$omicsData_2, "data_info")$data_scale) == input$transform)
  }
  else if (!is.null(objects$omicsData)) cond_scale <- (attr(objects$omicsData, "data_info")$data_scale == input$transform)

  toggleState("apply_transform", condition = !cond_scale & !(input$transform == "Select one"))
  revals$warnings_transform$scale_mismatch <- if (isTRUE(cond_scale)) "<p style = 'color:grey'>Your data is already on the selected scale.</p>" else NULL
  revals$warnings_transform$no_selection <- if (isTRUE(input$transform == "Select one")) "<p style = 'color:grey'>Select a transformation to apply.</p>" else NULL
})

observeEvent(input$which_qc_plot, {
  axes_ids <- c("qc_xlab", "qc_ylab", "qc_title")
  dropdown_ids <- c("qc_colors", "missingval_type", "qc_order_by", "qc_color_by", "qc_color_by_2", "qc_order_by_2", "qc_title_2", "qc_colors")
  choices <- list(
    "boxplots" = c(axes_ids, "qc_order_by", "qc_order_by", "qc_color_by", "qc_color_by_2", "qc_order_by_2", "qc_title_2"),
    "missingval_bar" = c(axes_ids, "missingval_type", "qc_colors"),
    "missingval_scatter" = c(axes_ids, "qc_colors")
  )

  # Toggle axes and coloring options depending on plot type
  lapply(c(dropdown_ids, axes_ids), function(inputid) {
    toggleState(inputid, condition = inputid %in% choices[[input$which_qc_plot]])
    # toggleCssClass(paste0("js_", inputid), "grey_out", condition = !(inputid %in% choices[[input$chooseplots]]))
  })

  panel_names <- c("plot_type", "axes_opts", "boxplot_opts", "missingval_opts")

  panel_open <- switch(input$which_qc_plot,
    boxplots = "boxplot_opts",
    missingval_bar = "missingval_opts",
    missingval_scatter = "missingval_opts"
  )

  updateCollapse(session, "qc_collapse", open = panel_open, close = setdiff(c("boxplot_opts", "missingval_opts"), panel_open))
})

# flip x and y axes input labels when the graph is flipped, since xlab() actually modifies the vertical axes in this case.
observeEvent(qc_flip(),
  {
    if (!is.null(qc_flip())) {
      updateTextInput(session, "qc_xlab", "Y-axis label", value = input$qc_ylab)
      updateTextInput(session, "qc_ylab", "X-axis label", value = input$qc_xlab)
    }
    else {
      updateTextInput(session, "qc_xlab", "X-axis label", value = input$qc_ylab)
      updateTextInput(session, "qc_ylab", "Y-axis label", value = input$qc_xlab)
    }
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE
)

## Pop-up to continue
# observeEvent(input$go_to_filter, {
#   
#   showModal(
#     modalDialog(
#       title = "Proceed",
#       fluidRow(
#         column(10,
#                align = "center", offset = 1,
#                actionButton("qc_dismiss", "Stay on this tab", width = "75%"),
#                actionButton("goto_filter", "Continue to filters", style = "margin-top:5px;width:75%"),
#                # bookmarkButton(label = "Bookmark the result of this page...", style = "margin-top:5px;width:75%")
#         )
#       ),
#       footer = NULL
#     )
#   )
#   
#   
# })

# qc success buttons
observeEvent(input$go_to_filter, {
  updateTabsetPanel(session, "top_page", selected = "Filter")
  removeModal()
})

# # qc success buttons
# observeEvent(input$goto_filter, {
#   updateTabsetPanel(session, "top_page", selected = "Filter")
#   removeModal()
# })
# 
# observeEvent(input$qc_dismiss, {
#   removeModal()
# })

