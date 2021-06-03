# disable inputs if we are not working with pepdata
observeEvent(c(objects$omicsData, input$top_page), {
  ids <- c("which_rollup", "which_combine_fn", "qrollup_thresh", "apply_rollup")
  req(!is.null(objects$omicsData))
  for (el in ids) {
    toggleState(el, condition = inherits(objects$omicsData, "pepData"))
  }
})

# apply rollup
observeEvent(input$apply_rollup, {
  shinyjs::show("rollup_busy")
  on.exit(hide("rollup_busy"))

  tryCatch(
    {
      func <- get(input$which_rollup, envir = asNamespace("pmartR"), mode = "function")

      if (input$which_rollup == "qrollup") {
        objects$omicsData <- func(objects$omicsData, input$qrollup_thresh, combine_fn = input$which_combine_fn)
      }
      else {
        objects$omicsData <- func(objects$omicsData, combine_fn = input$which_combine_fn)
      }

      updateCollapse(session, "rollup_mainpanel", open = "rollup_summary")
      revals$rollup_summary <- summary(objects$omicsData)
      plots$rollup_plot <- plot(objects$omicsData, bw_theme = TRUE)
    },
    error = function(e) {
      msg <- paste0("Something went wrong rollup up your pepdata:  \n System error:  ", e)
      message(msg)
      revals$warnings_rollup$bad_rollup <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      revals$rollup_summary <- NULL
      plots$rollup_plot <- NULL
    }
  )
})

# hide ugly empty wellpanel when there is no summary
observe({
  cond <- !is.null(revals$rollup_summary)
  toggle("rollup_data_summary_parent", condition = cond)
})
