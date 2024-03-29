# disable transforming to a scale which the object is already on
observeEvent(c(input$transform, objects$omicsData, objects$omicsData_2), {
  req(!is.null(objects$omicsData))
  if (!is.null(objects$omicsData_2)) {
    cond_scale <- all(c(attr(objects$omicsData, "data_info")$data_scale, attr(objects$omicsData_2, "data_info")$data_scale) == input$transform)
  }
  else if (!is.null(objects$omicsData)) cond_scale <- (attr(objects$omicsData, "data_info")$data_scale == input$transform)

  toggleState("apply_transform", condition = !cond_scale & !(input$transform == "none"))
  revals$warnings_transform$scale_mismatch <- if (isTRUE(cond_scale)) messageBox(type = "warning", "Your data is already on the selected scale") else NULL
  revals$warnings_transform$no_selection <- if (isTRUE(input$transform == "none")) messageBox(type = "warning", "Select a transformation to apply") else NULL
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

#'@details Disable options for choosing the plot type on the QC tab.  Currently:
#' - Disable missing values plots for seqData.
observe({
  req(!is.null(objects$omicsData))
  c(input$which_qc_plot, input$top_page)
  
  if(inherits(objects$omicsData, "seqData")) {
    # Positions 1,2 correspond to the missing values barplots and scatterplots
    disable(selector = "#which_qc_plot button:eq(1)")
    disable(selector = "#which_qc_plot button:eq(2)")
    toggleTooltip(session, id = "which_qc_plot_wrapper", tooltip_text = "Missing value plots disabled for RNA-seq data")
  }
})

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
  updateTabsetPanel(session, "top_page", selected = "filter_tab")
  removeModal()
})

# # qc success buttons
# 
# observeEvent(input$qc_dismiss, {
#   removeModal()
# })

