list(
  
  ## BPQUANT options
  output[["bpquant_options"]] <- renderUI({
    pro_class <- inherits(objects$omicsData, "proData")
    
    ## Note: only used for fdata info so don't need pep version
      data <- objects$omicsData
    
    stats <- objects$peptide_imdanova_res
    group_info <- pmartR:::get_group_DF(data)

    comp_info <- attr(stats, "comparisons")
    
    l1 <- nrow(unique(data$f_data[attr(group_info, "main_effects")]))
    l2 <- ifelse(
      !is.null(attr(group_info, "covariates")),
      nrow(unique(data$f_data[attr(group_info, "covariates")])),
      0
    )
    if (l1 == 1) l1 <- 0 # Not valid if it doesn't actually have multiple levels
    if (l2 == 1) l2 <- 0 # Not valid if it doesn't actually have multiple levels
    
    cond1 <- l1 + l2 < 3
    cond2 <- is.null(stats)
    if (is.null(nrow(comp_info))) {
      cond3 <- cond2 || length(comp_info) < 3
    } else {
      cond3 <- cond2 || nrow(comp_info) < 3
    }
    
    string_list <- c(
      "Specified group size(s) insufficent for isoform detection.",
      "Peptide-level IMD/ANOVA has not been run.",
      "Insufficent number of comparisons used in IMD/ANOVA."
    )
    
    if (cond1 || cond2 || cond3) {
      out <- div(
        "Isoform identification is computed using  BPQuant, which requires the following:",
        br(),
        br(),
        
        tags$ul(
          tags$li("At least one main effect specified with at 3+ groups OR two main effects specified with 2+ groups"),
          tags$li("IMD/ANOVA statistics have been computed on the peptide level"),
          tags$li("3+ unique pairwise comparisons specified for IMD/ANOVA statistics")
        ),
        
        br(),
        p(paste(string_list[c(cond1, cond2, cond3)], collapse = " "), style = "color:red")
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
    
    stats <- objects$peptide_imdanova_res
    group_info <- pmartR:::get_group_DF(data)
    comp_info <- attr(stats, "comparisons")
    
    l1 <- nrow(unique(data$f_data[attr(group_info, "main_effects")]))
    l2 <- ifelse(
      !is.null(attr(group_info, "covariates")),
      nrow(unique(data$f_data[attr(group_info, "covariates")])),
      0
    )
    if (l1 == 1) l1 <- 0 # Not valid if it doesn't actually have multiple levels
    if (l2 == 1) l2 <- 0 # Not valid if it doesn't actually have multiple levels
    
    
    cond1 <- l1 + l2 < 3
    cond2 <- is.null(stats)
    if (is.null(nrow(comp_info))) {
      cond3 <- cond2 || length(comp_info) < 3
    } else {
      cond3 <- cond2 || nrow(comp_info) < 3
    }
    
    if (any(c(cond1, cond2, cond3))) {
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
    
    # pro_class <- inherits(objects$omicsData, "proData")
    # 
    # # if (pro_class) {
    # #   data <- objects$omicsData_pre_rollup
    # # } else {
    #   data <- objects$omicsData
    # # }
    #   
    # req(!is.null(objects$bpquant), cancelOutput = TRUE)
    # 
    # result <- objects$bpquant
    # plotter <- table(map_dbl(result, function(df) {
    #   max(unique(df[[3]]))
    # }))
    # df <- as.data.frame(plotter, stringsAsFactors = F)
    # plot_ly(
    #   df,
    #   x = ~Var1,
    #   y = ~Freq,
    #   type = "bar",
    #   showlegend = FALSE
    # ) %>%
    #   add_text(
    #     showlegend = FALSE,
    #     textposition = "top middle",
    #     data = df,
    #     x = ~Var1,
    #     y = ~Freq,
    #     text = ~ paste(Freq)
    #   ) %>%
    #   layout(
    #     title = "Isoforms Detected",
    #     xaxis = list(title = "Total Isoforms"),
    #     yaxis = list(title = "Number of Protein Groups")
    #   )
  }),
  
  ##  Rollup plot
  
  output[["rollup_plot_UI"]] <- renderUI({
    if(is.null(plots$rollup_plot)){
      return("Please run protein roll-up to view results")
    } else {
      return(withSpinner(plotOutput("rollup_plot")))
    }
  }),
  
  # plot of prodata after rollup
  output$rollup_plot <- renderPlot({
    req(!is.null(objects$omicsData), cancelOutput = TRUE)
    plots$last_plot <- plots$rollup_plot
    plots$rollup_plot
  }),

  # inputs for axes labels and sizes
  output$rollup_plot_options <- renderUI({
    style_UI("rollup")
  }),

  # apply filter plot style options
  output$rollup_apply_style <- renderUI({
    apply_style_UI("rollup", FALSE, FALSE)
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
