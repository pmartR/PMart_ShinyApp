list(
  
  ## BPQUANT options
  output[["bpquant_options"]] <- renderUI({
    pro_class <- inherits(objects$omicsData, "proData")
    
    ## Note: only used for fdata info so don't need pep version
    data <- objects$omicsData
    
    stats <- objects$peptide_imdanova_res
    group_info <- pmartR:::get_group_DF(data)

    comp_info <- attr(stats, "comparisons")
    
    bpquant_conds <- unlist(bpquant_valid()[c("cond1", "cond2", "cond3", "cond4")])
    
    string_list <- c(
      "Specified group size(s) insufficent for isoform detection.",
      "Peptide-level ANOVA or combined IMD-ANOVA (G-test only is insufficient) has not been run.",
      "Insufficent number of comparisons used in IMD/ANOVA.",
      "ANOVA-only statistics were run and not all molecules had > 3 nonmissing observations in all groups, either filter down to relevant molecules or re-run statistics with a different method"
    )
    
    if (any(bpquant_conds)) {
      out <- div(
        "Isoform identification is computed using  BPQuant, which requires the following:",
        br(),
        br(),
        
        tags$ul(
          tags$li("At least one main effect specified with at 3+ groups OR two main effects specified with 2+ groups"),
          tags$li("ANOVA or combined IMD-ANOVA (G-test only is insufficient) statistics have been computed on the peptide level"),
          tags$li("3+ unique pairwise comparisons specified for IMD/ANOVA statistics"),
          tags$li("If only ANOVA has been run, all groups must have > 3 nonmissing observations across all peptides")
        ),
        
        br(),
        p(paste(string_list[bpquant_conds], collapse = " "), style = "color:red")
      )
      return(out)
    } else {
      choices <- gsub("_vs_", " vs ", get_comparisons(objects$peptide_imdanova_res)[[1]])
      value <- ifelse(is.null(input[["bpquant_max_prot"]]),
                      5, input[["bpquant_max_prot"]]
      )
      
      out <- div(
        pickerInput(
          "bpquant_comps",
          label = "Select comparisons to use (at least 3, no more than 5):",
          choices = choices,
          selected = isolate(input[["bpquant_comps"]]),
          multiple = TRUE,
          options = pickerOptions(maxOptions = 5#, actionsBox = TRUE
                                  )
        ),
        
        sliderInput(
          "bpquant_max_prot",
          "Maximum number of valid protein isoforms:",
          min = 2,
          max = 10,
          round = TRUE,
          value = value
        )
      )
      
      # pro_class <- inherits(objects$omicsData, "proData")
      # if(pro_class) out <- disabled(out)
      return(out)
    }
  }),
  
  
  ## BPQUANT use oin rollup
  
  # Icon manipulations
  output[["bpquant_apply_icon_UI"]] <- renderUI({
    
    req(!is.null(attr(objects$omicsData, "group_DF")))
    # input$bpquant
    
    pro_class <- inherits(objects$omicsData, "proData")

    if (pro_class) {
      data <- objects$omicsData_pre_rollup
    } else {
      data <- objects$omicsData
    }
    
    bpquant_conds <- bpquant_valid()[c("cond1", "cond2", "cond3", "cond4")]
    
    if (any(bpquant_conds)) {
      text <- "Conditions for Isoform Identification have not been met"
      
      return(
        disabled(radioGroupButtons(
          "bpquant_apply",
          div(
            "Use protein isoforms?",
            style = "display:inline-block",
            div(tipify(div(
              id = "bpquant_apply_icon",
              style = "color:deepskyblue;display:inline-block",
              icon("question-sign", lib = "glyphicon")
            ),
            title = text
            ),
            style = "display:inline-block",
            )
          ),
          choices = c("Yes", "No"),
          selected = "No"
        ))
      )
    } else if (is.null(data$bpquant) || 
               !input[["bpquant_lock"]]) {
      text <- "Compute and lock-in Isoform Identification results"
      
      return(
        disabled(radioGroupButtons(
          "bpquant_apply",
          div(
            "Use protein isoforms?",
            style = "display:inline-block",
            div(tipify(div(
              id = "bpquant_apply_icon",
              style = "color:deepskyblue;display:inline-block",
              icon("question-sign", lib = "glyphicon")
            ),
            title = text
            ),
            style = "display:inline-block",
            )
          ),
          choices = c("Yes", "No"),
          selected = "No"
        ))
      )
    } else {
      return(
        radioGroupButtons(
          "bpquant_apply",
          "Use protein isoforms?",
          choices = c("Yes", "No"),
          selected = isolate(input[["bpquant_apply"]])
        )
      )
    }
  }),
  
  output[["bpquant_res"]] <- renderPlotly({
    req(!is.null(objects$omicsData), cancelOutput = TRUE)
    plots$last_plot <- plots$bpquant
    plots$bpquant
  }),
  
  ##  Rollup plot
  
  output[["rollup_plot_UI"]] <- renderUI({
    if(is.null(plots$rollup_plot)){
      return("Please run protein roll up to view results")
    } else {
      return(withSpinner(plotlyOutput("rollup_plot")))
    }
  }),
  
  # plot of prodata after rollup
  output$rollup_plot <- renderPlotly({
    req(!is.null(objects$omicsData), cancelOutput = TRUE)
    plots$last_plot <- plots$rollup_plot
    plots$rollup_plot
  }),

  # inputs for axes labels and sizes
  output$rollup_plot_options <- renderUI({
    style_UI("rollup")
  }),

  # inputs for axes labels and sizes
  output$bpquant_plot_options <- renderUI({
    style_UI("bpquant")
  }),
  
  # apply filter plot style options
  output$rollup_apply_style <- renderUI({
    apply_style_UI("rollup", FALSE, FALSE)
  }),
  
  output$bpquant_apply_style <- renderUI({
    apply_style_UI("bpquant", FALSE, FALSE)
  }),

  output$rollup_data_summary <- renderUI({
    req(!is.null(revals$rollup_summary))
    output$rollup_data_summary_table <- renderDT(
      {
        revals$rollup_summary[1:6, ]
      },
      rownames = T,
      options = list(dom = "t") 
    )
    
    wellPanel(DTOutput("rollup_data_summary_table"))
    
  }),

  output$warnings_rollup <- renderUI({
    HTML(paste(revals$warnings_rollup, collapse = ""))
  })
)
