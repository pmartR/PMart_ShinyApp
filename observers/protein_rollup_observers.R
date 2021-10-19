# disable inputs if we are not working with pepdata
observeEvent(c(objects$omicsData, input$top_page), {
  
  ids <- c(
    "which_rollup",
    "which_combine_fn", 
    "qrollup_thresh", 
    "apply_rollup"#,
    # "bpquant_lock",
    # "bpquant"
   )
  
  req(!is.null(objects$omicsData))
  for (el in ids) {
    toggleState(el, condition = inherits(objects$omicsData, "pepData"))
  }
  
  is_rolled_up <- inherits(objects$uploaded_omicsData, "pepData") & inherits(objects$omicsData, "proData")
  toggleTooltip(session, "apply_rollup_jswrapper", is_rolled_up, tooltip_text = ttext_[["ROLLUP_DISABLE_INFO"]])
  
})


## BPQUANT

observeEvent(input[["bpquant"]], {
  
  req(length(input[["bpquant_comps"]]) > 2 && 
        length(input[["bpquant_comps"]]) < 6 && 
        input[["bpquant_lock"]] &&
        input[["bpquant"]] > 0)
  
  removeTab("rollup_mainpanel", "BPQuant Results", session = getDefaultReactiveDomain())
  
  shinyjs::show("isoform_busy")
  on.exit(hide("isoform_busy"))
  
  appendTab(
    "rollup_mainpanel",
    tabPanel(
      "BPQuant Results",
      br(),
      withSpinner(plotlyOutput("bpquant_res")),
      br(),
      wellPanel(
        uiOutput("bpquant_plot_options"),
        uiOutput("bpquant_apply_style")
      )
    )
  )
  
  updateTabsetPanel(session, "rollup_mainpanel",
                    selected = "BPQuant Results"
  )
  
  show("isoidicon")
  updateCollapse(session, "rollup_sidebar", close = "isoform_identification")
  
  updatePrettySwitch(session, "bpquant_lock", value = TRUE)
  
  pro_class <- inherits(objects$omicsData, "proData")
  
  if (pro_class) {
    data <- objects$omicsData_pre_rollup
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
            objects$omicsData <- objects$omicsData_pre_rollup
            objects$omicsData_pre_rollup <- NULL
            
            enable("bpquant_max_prot")
            enable("bpquant_comps")
            updateRadioButtons(session, "bpquant_apply", selected = "No")
            hide("isoidicon")
          } else {
            updatePrettySwitch(session, "bpquant_lock", value = TRUE)
          }
        }
      )
      
    } else {
      
      enable("bpquant_max_prot")
      enable("bpquant_comps")
      disable("bpquant")
      hide("isoidicon")
      updateRadioButtons(session, "bpquant_apply", selected = "No")
    }
  }
})


# apply rollup
observeEvent(input$apply_rollup, {
  shinyjs::show("rollup_busy")
  on.exit(hide("rollup_busy"))

  objects$omicsData_pre_rollup <- objects$omicsData
  revals$warnings_rollup$bad_rollup <- NULL
  
  tryCatch(
    {
      cname <- get_edata_cname(objects$omicsData)
      objects$omicsData$e_data[[cname]] <- as.character(objects$omicsData$e_data[[cname]]) #### Weird thing with numerics?
      objects$omicsData$e_meta[[cname]] <- as.character(objects$omicsData$e_meta[[cname]])
      
      if (input$which_rollup == "qrollup") thresh <- input$qrollup_thresh else thresh <- NULL
      if (input$bpquant_apply == "Yes") isores <- objects$bpquant else isores <- NULL
      
      if(input$which_rollup == "zrollup"){
        single_pep = T
        single_observation = T
      } else {
        single_pep = FALSE
        single_observation = FALSE
      }
      
      ## Catch for all NA peptides
      if(input$which_rollup == "rrollup" && 
         !("moleculeFilt" %in% unlist(map(pmartR::get_filters(objects$omicsData), 1)))){
        objects$omicsData <- applyFilt(molecule_filter(objects$omicsData), objects$omicsData, min_num = 1)
      }
      
      objects$omicsData <- protein_quant(
        objects$omicsData,
        method = input$which_rollup,
        combine_fn = input$which_combine_fn,
        isoformRes = isores,
        qrollup_thresh = thresh,
        single_pep = single_pep,
        single_observation = single_observation
      )
      
      show("prorollicon")
      updateCollapse(session, "rollup_sidebar", open = "rollup_summary", 
                     close = c("rollup_opts", "isoform_identification"))
      updateTabsetPanel(session, "rollup_mainpanel",
                        selected = "Rollup Results"
      )
      revals$rollup_summary <- summary(objects$omicsData)
      plots$rollup_plot <- plot(objects$omicsData, bw_theme = TRUE, interactive = T, 
                                color_by = "Group", order_by = "Group")
      
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
      msg <- paste0("Something went wrong rolling up your pepdata:  \n System error:  ", e)
      message(msg)
      objects$omicsData <- objects$omicsData_pre_rollup
      revals$warnings_rollup$bad_rollup <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      revals$rollup_summary <- NULL
      plots$rollup_plot <- NULL
      
    }
  )
})

observeEvent(input$rollup_dismiss, removeModal())
observeEvent(input$rollup_goto_stats,{
  updateTabsetPanel(session, "top_page", selected = "statistics_tab")
  removeModal()
})
observeEvent(input$rollup_goto_downloads,{
  updateTabsetPanel(session, "top_page", selected = "download_tab")
  removeModal()
})

## Update plot style
observeEvent(input$peptide_statistics_apply_style_plot_1, {
  
    plots$bpquant <- add_plot_styling(
      input,
      "bpquant", 
      plots$bpquant
    )
})

observeEvent(input$peptide_statistics_apply_style_plot_1, {
  
  plots$rollup_plot <- add_plot_styling(
    input,
    "rollup", 
    plots$rollup_plot
  )
})
