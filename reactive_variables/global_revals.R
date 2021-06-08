# reactive which stores the normRes object for summary purposes
global_norm_1 <- reactive({
  if (input$subset_fn %in% c("all", "complete")) {
    params <- NULL
  }
  else if (input$subset_fn == "los") {
    params <- list(los = input$los)
  }
  else if (input$subset_fn == "ppp") {
    params <- list(ppp = input$ppp)
  }
  else if (input$subset_fn == "rip") {
    params <- list(rip = input$rip)
  }
  else if (input$subset_fn == "ppp_rip") {
    params <- list(ppp_rip = list(ppp = input$ppp, rip = input$rip))
  }

  tryCatch(
    {
      normalize_global(objects$omicsData,
        subset_fn = input$subset_fn,
        norm_fn = input$norm_fn,
        params = params
      )
    },
    error = function(e) {
      msg <- paste0("Something went wrong creating your normRes object.  \n System error:  ", e)
      message(msg)
      revals$warnings_normalize$bad_normres_1 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      NULL
    }
  )
})
