# disable inputs if we are not working with pepdata
observeEvent(c(objects$omicsData, input$top_page), {
  
  ids <- c(
    "which_rollup",
    "which_combine_fn", 
    "qrollup_thresh", 
    "apply_rollup",
    "bpquant_lock"#,
    # "bpquant"
           )
  
  req(!is.null(objects$omicsData))
  for (el in ids) {
    toggleState(el, condition = inherits(objects$omicsData, "pepData"))
  }
})


## BPQUANT

observeEvent(input[["bpquant"]], {
  
  req(length(input[["bpquant_comps"]]) > 2 && 
        length(input[["bpquant_comps"]]) < 6 && 
        input[["bpquant_lock"]] &&
        input[["bpquant"]] > 0)
  
  removeTab("rollup_mainpanel", "BPQuant Results", session = getDefaultReactiveDomain())
  
  showNotification("Calculating isoforms, please wait...",
                   duration = NULL,
                   closeButton = FALSE,
                   id = "bpquant_note"
  )
  
  appendTab(
    "rollup_mainpanel",
    tabPanel(
      "BPQuant Results",
      br(),
      withSpinner(plotlyOutput("bpquant_res"))
    )
  )
  
  updateTabsetPanel(session, "rollup_mainpanel",
                    selected = "BPQuant Results"
  )
  updatePrettySwitch(session, "bpquant_lock", value = TRUE)
  
  pro_class <- inherits(objects$omicsData, "proData")
  
  if (pro_class) {
    data <- objects$Prior_rollup
  } else {
    data <- objects$omicsData
  }
  
  isoformres <- bpquant(
    statRes = objects$peptide_imdanova_res,
    pep = data,
    # pi_not = ,
    max_proteoforms = input[["bpquant_max_prot"]]
  )
  
  objects$bpquant <- isoformres
  
  ### Plot
  plotter <- table(map_dbl(isoformres, function(df) {
    max(unique(df[[3]]))
  }))
  df <- as.data.frame(plotter, stringsAsFactors = F)
  plots$bpquant <- plot_ly(
    df,
    x = ~Var1,
    y = ~Freq,
    type = "bar",
    showlegend = FALSE
  ) %>%
    add_text(
      showlegend = FALSE,
      textposition = "top middle",
      data = df,
      x = ~Var1,
      y = ~Freq,
      text = ~ paste(Freq)
    ) %>%
    layout(
      title = "Isoforms Detected",
      xaxis = list(title = "Total Isoforms"),
      yaxis = list(title = "Number of Protein Groups")
    )
  
  
  
  updateRadioButtons(session, "bpquant_apply",
                     label = "Use protein isoforms?", 
                     selected = isolate(input[["bpquant_apply"]])
  )
  enable("bpquant_apply")
  
  # radioGroupButtons(
  #   "bpquant_apply",
  #   "Use protein isoforms?",
  #   choices = c("Yes", "No"),
  #   selected = isolate(input[["bpquant_apply"]])
  # )
  
  
  removeNotification("bpquant_note")
})

# lock/unlock bpquant
observeEvent(input[["bpquant_lock"]], {
  
  if (input[["bpquant_lock"]]) {
    disable("bpquant_max_prot")
    disable("bpquant_comps")
    enable("bpquant")
  } else {
    
    # check_rollup_exists()
    
    if (inherits(objects$omicsData, "proData")) {
      shinyalert(
        closeOnEsc = F,
        showCancelButton = T,
        type = "warning",
        title = "Careful!",
        text = "Re-enabling isoform identification will clear previously rolled-up results. Are you sure you want to do this?",
        callbackR = function(value) {
          if (value) {
            objects$omicsData <- objects$Prior_rollup
            objects$Prior_rollup <- NULL
            
            enable("bpquant_max_prot")
            enable("bpquant_comps")
            updateRadioButtons(session, "bpquant_apply", selected = "No")
          } else {
            updatePrettySwitch(session, "bpquant_lock", value = TRUE)
          }
        }
      )
      
    } else {
      
      enable("bpquant_max_prot")
      enable("bpquant_comps")
      disable("bpquant")
      updateRadioButtons(session, "bpquant_apply", selected = "No")
    }
  }
})


# apply rollup
observeEvent(input$apply_rollup, {
  shinyjs::show("rollup_busy")
  on.exit(hide("rollup_busy"))

  tryCatch(
    {

      objects$Prior_rollup <- objects$omicsData
      
      cname <- get_edata_cname(objects$omicsData)
      objects$omicsData$e_data[[cname]] <- as.character(objects$omicsData$e_data[[cname]]) #### Weird thing with numerics?
      objects$omicsData$e_meta[[cname]] <- as.character(objects$omicsData$e_meta[[cname]])
      
      if (input$which_rollup == "qrollup") thresh <- input$qrollup_thresh else thresh <- NULL
      if (input$bpquant_apply == "Yes") isores <- objects$bpquant else isores <- NULL
      
      objects$omicsData <- protein_quant(
        objects$omicsData,
        method = input$which_rollup,
        combine_fn = input$which_combine_fn,
        isoformRes = isores,
        qrollup_thresh = thresh
      )
      
      # func <- get(input$which_rollup, envir = asNamespace("pmartR"), mode = "function")
      # if (input$which_rollup == "qrollup") {
      #   objects$omicsData <- func(objects$omicsData, 
      #                             input$qrollup_thresh,
      #                             isoformRes = objects$bpquant,
      #                             combine_fn = input$which_combine_fn)
      # }
      # else {
      #   objects$omicsData <- func(objects$omicsData, 
      #                             isoformRes = objects$bpquant,
      #                             combine_fn = input$which_combine_fn)
      # }

      updateCollapse(session, "rollup_mainpanel", open = "rollup_summary")
      revals$rollup_summary <- summary(objects$omicsData)
      plots$rollup_plot <- plot(objects$omicsData, bw_theme = TRUE)
      
      showModal(
        modalDialog(
          {
            fluidRow(
              column(10,
                     align = "center", offset = 1,
                     tags$h4(
                       paste0(
                         "Success! Rollup has been performed using ",
                         input$which_rollup,
                         " method with ",
                         input$which_combine_fn,
                         " to combine results. ",
                         ifelse(is.null(thresh), "", 
                                paste0("Q-rollup threshold set to:", thresh)
                                ),
                         ifelse(is.null(isores), "", 
                                " Isoform results were used in during rollup.")
                         )
                     ),
                     hr(),
                     actionButton("rollup_dismiss", "Review results", width = "75%"),
                     actionButton("rollup_goto_stats", "Continue to Statistics Tab", width = "75%"),
                     actionButton("rollup_goto_downloads", "Continue to Download Tab", width = "75%")
              )
            )
          }
        )
      )
      
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

observeEvent(input$rollup_dismiss, removeModal())
observeEvent(input$rollup_goto_stats,{
  updateTabsetPanel(session, "top_page", selected = "Statistics")
  removeModal()
})
observeEvent(input$rollup_goto_downloads,{
  updateTabsetPanel(session, "top_page", selected = "Download")
  removeModal()
})
