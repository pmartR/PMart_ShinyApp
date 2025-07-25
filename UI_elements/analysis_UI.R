# main plot display which takes in the plot object that is created immediately after imd_anova() is run
output$statistics_mainplot <- renderUI({
  req(!is.null(plots$statistics_mainplot))
  p <- plots$statistics_mainplot
  plots$last_plot <- p
  
  # Objects should be combined by this point, don't save the second plot, which
  # may be stored from previous tabs.
  if(!is.null(objects$omicsData_2)) {
    plots$last_plot_2 <- NULL
  }
  
  if (inherits(p, "plotly")) {
    output$statistics_mainplot_plotly <- renderPlotly(p)
    plotlyOutput("statistics_mainplot_plotly")
  }
  else if (inherits(p, "ggplot")) {
    output$statistics_mainplot_ggplot <- renderPlot(p)
    plotOutput("statistics_mainplot_ggplot")
  }
})

# diagnostic plot display which takes in the diagnostic plot object for seqData
output$statistics_diagplot <- renderUI({
  req(!is.null(plots$statistics_diagplot))
  
  text <- paste0("Usage: Ensure data fits the trendline generated by diagnostic",
                 " plot. Fit may be improved with additional filtering metrics",
                 " (e.g., Total Count filter).")
  
  p <- plots$statistics_diagplot
  plots$last_plot <- p
  if (inherits(p, "plotly")) {
    output$statistics_diagplot_plotly <- renderPlotly(p)
    div(
      plotlyOutput("statistics_diagplot_plotly"),
      br(),
      br(),
      text
    )
    plotlyOutput("statistics_diagplot_plotly")
  }
  else if (inherits(p, "ggplot")) {
    output$statistics_diagplot_ggplot <- renderPlot(p)
    div(
      plotOutput("statistics_diagplot_ggplot"),
      br(),
      br(),
      text
    )
  }
})

output$stats_select_method_UI <- renderUI({
  
  seq_disables <- if(inherits(objects$omicsData, "seqData")) {
    c(F, T, T, F, F, F, F)
  } else {
    c(F, F, F, T, T, T, T)
  }
  
  pickerInput(
    "stats_select_method",
    "Select analysis method:",
    choices = c(
      "Select one" = NULLSELECT_,
      "iMd-ANOVA" = "imdanova",
      "PCA" = "pca",
      "GLM-PCA" = 'glmpca',
      "DESeq2" = "DESeq2",
      "Limma-voom" = "voom",
      "EdgeR" = "edgeR"
    ),
    choicesOpt = list(
      disabled = seq_disables
    ),
    multiple = TRUE,
    options = pickerOptions(maxOptions = 1)
  )
})

#'@details Conditional radiogroupbuttons depending on what plot types are valid
#'for the analysis that was run.
output$imdanova_plot_type_UI <- renderUI({
  validate(need(objects$imdanova_res, "No iMd-ANOVA results found, run iMd-ANOVA from the side panel."))
  
  choices = switch(
    attr(objects$imdanova_res, "statistical_test"),
    "combined" = c("Bar" = "bar", "Volcano" = "volcano", "Counts Heatmap" = "gheatmap"),
    "anova" = c("Bar" = "bar", "Volcano" = "volcano"),
    "gtest" = c("Bar" = "bar", "Counts Heatmap" = "gheatmap"),
    "__INVALID__"
  )
  
  validate(need(choices != "__INVALID__", "If you're seeing this message, someone (maybe us..) messed up and your stats results may be corrupted."))
  
  radioGroupButtons(
    "imdanova_plot_type",
    "Plot type",
    choices = choices
  )
})

#'@details Conditional radiogroupbuttons depending on what plot types are valid
#'for the analysis that was run.
output$seqdata_plot_type_UI <- renderUI({
  validate(need(objects$seqstats_res, "No differential expression results found, run differential expression function from the side panel."))
  
  choices <- c("Bar" = "bar", "Volcano" = "volcano", "MA" = "ma")
  
  radioGroupButtons(
    "seqdata_plot_type",
    "Plot type",
    choices = choices
  )
})

#'@details Pickers for ANOVA/G-test pvalue adjustment.  One or two pickers are
#'shown depending on if the user chooses the 'combined' option for iMd-ANOVA
output$imdanova_pval_adjust_UI <- renderUI({
  validate(need(
    input$imdanova_test_method %in% c("anova", "gtest", "combined"),
    "Please select test method."
  ))
  
  # to make things look nice
  if(input$imdanova_test_method == 'combined') {
    title_anova_mc <- title_anova_fdr <- "(ANOVA)"
    title_gtest_mc <- title_gtest_fdr <- "(G-test)"
  } else {
    title_anova_mc <- div(
      div(style = "float:left", "Multiple comparisons adjustment (ANOVA)"),
      tipify(blueq, ttext_[["WHAT_IS_MC"]])
    )

    title_gtest_mc <- div(
      div(style = "float:left", "Multiple comparisons adjustment (G-test)"),
      tipify(blueq, ttext_[["WHAT_IS_MC"]])
    )

    title_anova_fdr <- div(
      div(style = "float:left", "FDR adjustment (ANOVA)"),
      tipify(blueq, ttext_[["WHAT_IS_FDR"]])
    )

    title_gtest_fdr <- div(
      div(style = "float:left", "FDR adjustment (G-test)"),
      tipify(blueq, ttext_[["WHAT_IS_FDR"]])
    )
  }
  
  anova_picker_mc <-  pickerInput(
    "imdanova_pval_adjust_a_multcomp",
    title_anova_mc,
    choices = c(
      "Holm" = "holm",
      "Bonferroni" = "bonferroni",
      "Tukey" = "tukey",
      "Dunnet" = "dunnett",
      "None" = "none"
    ),
    selected = "none"
  )

  gtest_picker_mc <- pickerInput(
    "imdanova_pval_adjust_g_multcomp",
    title_gtest_mc,
    choices = c(
      "Holm" = "holm",
      "Bonferroni" = "bonferroni",
      "None" = "none"
    ),
    selected = "none"
  )

  anova_picker_fdr <-  pickerInput(
    "imdanova_pval_adjust_a_fdr",
    title_anova_fdr,
    choices = c(
      "Benjamini-Hochberg (FDR)" = "BH",
      "Benjamini-Yekutieli" = "BY",
      "Bonferroni" = "bonferroni",
      "None" = "none"
    ),
    selected = if(inherits(objects$omicsData, "seqData")) "BH" else 'none'
  )

  gtest_picker_fdr <- pickerInput(
    "imdanova_pval_adjust_g_fdr",
    title_gtest_fdr,
    choices = c(
      "Benjamini-Hochberg (FDR)" = "BH",
      "Benjamini-Yekutieli" = "BY",
      "Bonferroni" = "bonferroni",
      "None" = "none"
    ),
    selected = if(inherits(objects$omicsData, "seqData")) "BH" else 'none'
  )
  
  # dont give multiple comparison adjustment option if there is only one comparison
  n_comparisons = nrow(comp_df_holder$comp_df)
  
  if (isTruthy(n_comparisons > 1)) {
    anova_pickers = tagList(anova_picker_mc, anova_picker_fdr)
    gtest_pickers = tagList(gtest_picker_mc, gtest_picker_fdr) 
  } else {
    anova_pickers = tagList(anova_picker_fdr)
    gtest_pickers = tagList(gtest_picker_fdr)
  }
  
  if (input$imdanova_test_method == "anova") {
    return(anova_pickers)
  }
  else if (input$imdanova_test_method == "gtest") {
    return(gtest_pickers)
  } 
  else if(input$imdanova_test_method == "combined") {
    mc_divs <- if (isTruthy(n_comparisons > 2)) {
      list(
        div(class = 'inline-wrapper-1',
            tags$b("Multiple comparisons adjustment"),
            tipify(blueq, ttext_[["WHAT_IS_MC"]])),
        fluidSplitLayout(anova_picker_mc, gtest_picker_mc)
      )
    } else NULL
    
    fdr_divs = list(
      div(class = 'inline-wrapper-1',
          tags$b("FDR adjustment"),
          tipify(blueq, ttext_[["WHAT_IS_FDR"]])), 
      fluidSplitLayout(anova_picker_fdr, gtest_picker_fdr)
    )
    
    return(do.call(tagList, c(mc_divs, fdr_divs)))
  }
})


#'@details Pickers for ANOVA/G-test pvalue adjustment.  One or two pickers are
#'shown depending on if the user chooses the 'combined' option for iMd-ANOVA
output$seqdata_pval_adjust_UI <- renderUI({

  
  # to make things look nice
  prepend <- "False discovery rate control "
  
  analysis <- switch(
    input$stats_select_method,
    "voom" = "(Limma-voom)", 
    "DESeq2" = "(DESeq2)", 
    "edgeR" = "(EdgeR)")
  
  seqdata_picker <-  pickerInput(
    "seqdata_pval_adjust",
    paste0(prepend, analysis),
    choices = c(
      "Benjamini & Hochberg (FDR)" = "BH",
      "Benjamini & Yekutieli" = "BY",
      "None" = "none"
    ),
    selected = "BH"
  )
  
  out_div = div(
    class = "inline-wrapper-1",
    seqdata_picker,
    shinyBS::tipify(blueq, title = ttext_[["SEQDATA_PVAL_ADJUST"]])
  )
  
  return(out_div)
})


#'@details returns a different sidepanel of options depending on what stats
#'statistics was selected.
output$statistics_tab_sidepanel <- renderUI({
  req(input$stats_select_method != NULLSELECT_)
  
  n_groups <- length(unique(attr(objects$omicsData, 'group_DF')$Group))
  
  # If we only have one possible comparison, just default to that one and disable the other options.
  if (n_groups == 2) {
    choices = c(
      "Only one comparison" = "all_pairwise",
      "Control to test condition comparisons" = "control_to_test",
      "Custom comparisons" = "custom"
    )
    
    selected = "all_pairwise"
    
    choicesOpt = list("disabled" = c(FALSE, TRUE, TRUE))
    
  } else {
    choices = c(
      "All pairwise comparisons" = "all_pairwise",
      "Control to test condition comparisons" = "control_to_test",
      "Custom comparisons" = "custom"
    )
    
    selected = character(0)
    
    choicesOpt = NULL
  }
  
  if (input$stats_select_method %in% c("imdanova")){
    bsCollapse(
      id = "imdanova-sidepanel-options", multiple = TRUE, 
      open = c("imdanova-specify-comparisons", "imdanova-select-settings"), 
      #
      bsCollapsePanel(
        subsection_header(
          "Group Comparisons",
          "imdanova_groups_ok",
          "color:orange;float:right",
          icon("ok", lib = "glyphicon")
        ),
        value = "imdanova-specify-comparisons",
        pickerInput(
          "comparison_method",
          "Select comparisons to use (singleton groups excluded):",
          choices = choices,
          selected = selected,
          options = pickerOptions(maxOptions = 1),
          choicesOpt = choicesOpt,
          multiple = TRUE
        ),
        
        uiOutput("pairwise_comp_selector"),
        
        hr(),
        
        DTOutput("pairwise_comp_display"),
        
        tags$script('Shiny.addCustomMessageHandler("unbind-DT-RR", function(x) {
                      Shiny.unbindAll($(document.getElementById(x)).find(".dataTable"));
                      });')
      ),
      bsCollapsePanel(
        subsection_header(
          "iMd-ANOVA Test Settings",
          "imdanova_settings_ok",
          "color:orange;float:right",
          icon("ok", lib = "glyphicon")
        ),
        value = "imdanova-select-settings",
        uiOutput("imdanova_test_method_UI"),
        uiOutput("imdanova_pval_adjust_UI"),
        numericInput("pval_thresh", "Significance threshold", min = 0, max = 1, value = 0.05, step = 0.01),
        bsButton("apply_imdanova", "Perform iMd-ANOVA", style = "primary"),
        br(), br(),
        hidden(
          div(
            "Conducting analysis, please wait...",
            id = "analysis_busy",
            class = "fadein-out",
            style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
          )
        )
      )
    )
  } else if (input$stats_select_method %in% c("DESeq2", "voom", "edgeR")){
    
    if(inherits(objects$omicsData, "seqData")){
      
      bsCollapse(
        id = "seqdata-sidepanel-options", multiple = TRUE, 
        open = c("seqdata-specify-comparisons", "seqdata-select-settings"), 
        #
        bsCollapsePanel(
          subsection_header(
            "Group Comparisons",
            "seqdata_groups_ok",
            "color:orange;float:right",
            icon("ok", lib = "glyphicon")
          ),
          value = "seqdata-specify-comparisons",
          pickerInput(
            "comparison_method",
            "Select comparisons to use (singleton groups excluded):",
            choices = choices,
            selected = selected,
            options = pickerOptions(maxOptions = 1),
            choicesOpt = choicesOpt,
            multiple = TRUE
          ),
          
          uiOutput("pairwise_comp_selector"),
          
          hr(),
          
          DTOutput("pairwise_comp_display"),
          
          tags$script('Shiny.addCustomMessageHandler("unbind-DT-RR", function(x) {
                      Shiny.unbindAll($(document.getElementById(x)).find(".dataTable"));
                      });')
        ),
        bsCollapsePanel(
          subsection_header(
            paste0(
              switch(
                input$stats_select_method,
                "voom" = "Limma-voom", 
                "DESeq2" = "DESeq2", 
                "edgeR" = "EdgeR"),
            " Test Settings"),
            "seqdata_settings_ok",
            "color:orange;float:right",
            icon("ok", lib = "glyphicon")
          ),
          value = "seqdata-select-settings",
          uiOutput("seqdata_test_method_UI"),
          uiOutput("seqdata_pval_adjust_UI"),
          numericInput("pval_thresh", "Significance threshold", min = 0, max = 1, value = 0.05, step = 0.01),
          bsButton("apply_diagnostic", "Run trend-fitting diagnostics", style = "primary"),
          disabled(bsButton("apply_seqstats", "Perform differential expression", style = "primary")),
          br(), br(),
          hidden(
            div(
              "Conducting analysis, please wait...",
              id = "analysis_busy",
              class = "fadein-out",
              style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
            )
          )
        )
      )
      
    } else {
      
     bsCollapse(
        id = "seqdata-sidepanel-options", multiple = FALSE, 
        open = c("incorrect-class"), 
        #
        bsCollapsePanel(
          "Invalid Option",
          value = "incorrect-class",
          strong("Selected method requires transcriptomics data to run, please select a different method.")
        )
      )
      
    }
  } else if (input$stats_select_method %in% c("pca", "glmpca")) {
    bsCollapse(
        id = "pca-sidepanel-options", multiple = TRUE, open = c("dimred-select-settings"),
        #
        bsCollapsePanel(
          subsection_header(
            "Dimension Reduction Options",
            "pca_settings_ok",
            "color:orange;float:right",
            icon("ok", lib = "glyphicon")
          ),
          value = "dimred-select-settings",
          
          numericInput("set_seed_analysis", label = "Set random seed:", min = 0, value = 2025),
          
          # div(class = "grey_text", "No options for this method currently"),
          br(),
          bsButton("apply_dimreduction", "Apply dimension reduction", style = "primary"),
          br(), br(),
          hidden(
            div(
              "Calculating, please wait...",
              id = "analysis_busy",
              class = "fadein-out",
              style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
            )
          )
        )
      )
  }
})


#'@details returns a different mainpanel of results depending on what stats
#'statistics was selected.
output$statistics_tab_mainpanel <- renderUI({
  req(input$stats_select_method != NULLSELECT_)
  
  if (input$stats_select_method == "imdanova"){
    bsCollapse(
      id = "statistics_collapse_main", multiple = TRUE,
      bsCollapsePanel("Plots",
                      value = "statistics_plots",
                      uiOutput("imdanova_plot_type_UI"), 
                      withSpinner(uiOutput("statistics_mainplot"))
      ),
      bsCollapsePanel("Plot Options",
                      value = "statistics_plot_opts",
                      uiOutput("statistics_plot_options")
      ),
      bsCollapsePanel("Tables",
                      value = "statistics_tables",
                      withSpinner(DTOutput("statistics_summary_table"))
      )
    )
  } else if (input$stats_select_method %in% c("DESeq2", "voom", "edgeR")){
    
    if(inherits(objects$omicsData, "seqData")){
      
      bsCollapse(
        id = "statistics_collapse_main", multiple = TRUE,
        bsCollapsePanel("Diagnostic Plots",
                        value = "diagnotistic_plots",
                        withSpinner(uiOutput("statistics_diagplot"))
        ),
        bsCollapsePanel("Analysis Plots",
                        value = "statistics_plots",
                        uiOutput("seqdata_plot_type_UI"), 
                        withSpinner(uiOutput("statistics_mainplot"))
        ),
        bsCollapsePanel("Plot Options",
                        value = "statistics_plot_opts",
                        uiOutput("statistics_plot_options")
        ),
        bsCollapsePanel("Tables",
                        value = "statistics_tables",
                        withSpinner(DTOutput("statistics_summary_table"))
        )
      )
    } 
  } else if (input$stats_select_method %in% c("pca", "glmpca")){
    bsCollapse(
      id = "statistics_collapse_main", multiple = TRUE,
      bsCollapsePanel("Plots",
                      value = "statistics_plots",
                      withSpinner(uiOutput("statistics_mainplot"))
      ),
      bsCollapsePanel("Plot Options",
                      value = "statistics_plot_opts",
                      uiOutput("dimres_plot_options")
      )
    )
  }
})

#'@details Give options for comparisons depending on what user selected as the
#' method to perform comparisons (All comparisons, control vs treatment, custom)
output$pairwise_comp_selector <- renderUI({
  req(input$comparison_method)
  
  if (input$comparison_method == "all_pairwise") {
    return()
  } else if (input$comparison_method == "control_to_test") {
    groups <- objects$omicsData %>%
      pmartR:::get_group_table()
    groups <- groups[groups > 1] %>% names()
    
    return(
      div(
        pickerInput("control_group",
                    "Select control group:",
                    groups,
                    selected = input$control_group,
                    options = pickerOptions(maxOptions = 1),
                    multiple = TRUE
        ),
        uiOutput("non_control_groups")
      )
    )
  } else {
    groups <- objects$omicsData %>%
      pmartR:::get_group_table() %>%
      names()
    combos <- apply(combn(groups, 2), 2, toString)
    
    return(
      pickerInput("custom_comps",
                  "Select group comparisons of interest:",
                  combos,
                  selected = isolate(input$custom_comps),
                  multiple = TRUE
      )
    )
  }
})

#'@details Select the 'treatment' groups to compare to the selected control.
#'The control selection will be disabled.
output$non_control_groups <- renderUI({
  req(input$control_group)
  groups <- objects$omicsData %>%
    pmartR:::get_group_table()
  groups <- groups[groups > 1] %>% names()
  
  pickerInput("imdanova_non_control_groups",
              "Select group(s) to compare to control:",
              groups,
              selected = isolate(input$imdanova_non_control_groups),
              choicesOpt = list(
                disabled = groups %in% input$control_group
              ),
              multiple = TRUE
  )
})

#'@details display the data-table of possible comparisons.
output$pairwise_comp_display <- renderDT(
  {
    req(!is.null(comp_df_holder$comp_df))
    session$sendCustomMessage("unbind-DT-RR", "pairwise_comp_display")
    comp_df_holder$comp_df
  },
  options = list(
    dom = "t", scrollX = TRUE,
    preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
    drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } ")
  ),
  selection = "none",
  escape = FALSE
)

#'@details Picker for which type of statistical test to use in imd-anova
output$imdanova_test_method_UI <- renderUI({
  req(objects$omicsData)

  filt_method <- get_filters(objects$omicsData, "imdanovaFilt", "method") %>% 
    unlist()
  
  choices = c(
    "ANOVA" = "anova",
    "G-Test" = "gtest",
    "Combined" = "combined"
  )
  
  if (is.null(filt_method) || 'combined' %in% filt_method) {
    groupsizes <- pmartR:::get_group_table(objects$omicsData)
    groupsizes <- groupsizes[groupsizes > 1]
    groupsizes <- groupsizes[names(groupsizes) %in% unlist(comp_df_holder$comp_df[1:2])]
    
    # disable g-test for small group sizes and paired data
    if (any(groupsizes < 3))  {
      disabler <- choices %in% c("gtest", "combined")
    } else {
      disabler <- rep(F, length(choices))
    }
  } else {
    disabler <- !(choices %in% filt_method)
  }
  
  # disable gtest and combined for paired data
  if(omicsData_paired()) {
    disabler = disabler | choices %in% c("gtest", "combined")
  }  
  
  choicesOpt <-  list()
  content <- NULL
  
  disable_reasons <- c(
    "ANOVA" = "Requires 2+ members per group",
    "G-Test" = "Requires 3+ members per group.  Cannot be run on paired data.",
    "Combined" = "Requires 3+ members per group.  Cannot be run on paired data."
  )
  
  disable_reasons <- if(any(disabler)) {
    disable_reasons[disabler]
  } else disable_reasons
  
  disable_reasons <- lapply(names(disable_reasons), function(name) {
    sprintf("%s:  %s", name, disable_reasons[name])
  }) %>% paste(collapse = "&#013;&#010;")
  
  if (!any(disabler)) {
    tmp_tooltip = NULL
  } else {
    choicesOpt[['disabled']] = disabler
    tmp_tooltip = tipify(blueexcl, title = disable_reasons)
  }
  
  return(
    div(class = "inline-wrapper-1",
        pickerInput("imdanova_test_method",
                    "Test method:",
                    choices = choices,
                    selected = input$imdanova_test_method,
                    options = pickerOptions(maxOptions = 1),
                    multiple = TRUE,
                    choicesOpt = choicesOpt
        ),
        tmp_tooltip
        )
  )
  
})

#'@details Picker for which type of statistical test to use in DESeq2
output$seqdata_test_method_UI <- renderUI({
  req(!is.null(objects$omicsData) && input$stats_select_method == "DESeq2")
  
  choices = c(
    "Wald significance test" = "Wald",
    "Likelihood ratio test " = "LRT"
  )
  
  return(
    div(class = "inline-wrapper-1",
        pickerInput("seqdata_test_method",
                    "Test method:",
                    choices = choices,
                    selected = input$seqdata_test_method,
                    options = pickerOptions(maxOptions = 1),
                    multiple = FALSE
        )
    )
  )
  
})

# display table output from imd_anova
output$statistics_summary_table <- renderDT({
  if (inherits(objects$omicsData, "seqData")) {
    objects$seqstats_res
  } else {
    objects$imdanova_res
  }
}, options = list(scrollX =TRUE))

#'@details UI created with the helper function style_UI to edit plot options
#'for the dimension reduction plots
output$dimres_plot_options <- renderUI({
  choices <- colnames(objects$omicsData$f_data %>%
                        dplyr::select(-one_of(
                          attributes(objects$omicsData)$cnames$fdata_cname
                        )))
  
  tagList(
    div(
      class = 'inline-wrapper-1',
      pickerInput(
        inputId = "analysis_pca_color_by",
        label = "Color by:",
        choices = c("Select one" = NULLSELECT_, choices, "Group"),
        selected = "Group"
      ),
      pickerInput(
        inputId = "analysis_pca_shape_by",
        label = "Shape by:",
        choices = c("Select one" = NULLSELECT_, choices, "Group"),
        selected = NULLSELECT_
      ),
      br(),
      radioGroupButtons(
        inputId = "analysis_pca_interactive",
        choices = c("Static" = F, "Interactive" = T),
        selected = F
      ),
      br(),
      actionButton("pca_update_plot_content", "Update plot")
    ),
    style_UI(
        "statistics",
        hr(),
        h4("Axes styling")
      ),
      uiOutput("statistics_apply_style")
    )
})

#'@details UI created with the helper function style_UI to edit plot options
#'in plot.<object> function as well as axes values (if it is a ggplot)
output$statistics_plot_options <- renderUI({
  cpicker_args <- list(
    list(inputId = "imd_down_cpicker", label = "Negative", value = "red"),
    list(inputId = "imd_nonsig_cpicker", label = "Non significant", value = "black"),
    list(inputId = "imd_up_cpicker", label = "Positive", value = "green")
  )
  
  high_low_args <- list(
    list(inputId = "imd_low_cpicker", label = "Low", value = "#132B43"),
    list(inputId = "imd_high_cpicker", label = "High", value = "#56B1F7")
  )
  
  tagList(
    h4("Main plot content"),
    div(
      class = "inline-wrapper-1",
      conditionalPanel(
        "['volcano'].includes(input.imdanova_plot_type) || ['volcano'].includes(input.seqdata_plot_type)",
        numericInput("imd_plot_fc_thresh", "Fold-change Threshold", value = NULL),
      ),
      conditionalPanel(
        "['volcano', 'gheatmap'].includes(input.imdanova_plot_type) || ['volcano', 'gheatmap'].includes(input.seqdata_plot_type)",
        radioGroupButtons(
          "stats_interactive_yn",
          choices = c("Static" = F, "Interactive" = T),
          selected = F
        )
      )
    ),
    conditionalPanel(
      "['volcano', 'bar'].includes(input.imdanova_plot_type) || ['volcano', 'bar'].includes(input.seqdata_plot_type)",
      tags$b(h5("Statistical significance/fold change colors")),
      inline_cpickers(cpicker_args), # UI helper
    ),
    conditionalPanel(
      "['gheatmap'].includes(input.imdanova_plot_type) || ['gheatmap'].includes(input.seqdata_plot_type)",
      tags$b(h5("Low/High count colors")),
      inline_cpickers(high_low_args), # UI helper
    ),
    div(
      class = "inline-wrapper-1",
      actionButton("stats_update_plot_content", "Update plot"),
      conditionalPanel("input.stats_interactive_yn == 'TRUE'",
                       tipify(blueexcl, ttext_[["IMD_INTERACTIVE_MANY_POINTS"]]))
    ),
    tagList(
      style_UI(
        "statistics",
        hr(),
        h4("Axes styling")
      ),
      uiOutput("statistics_apply_style")
    )
  )
})

output$statistics_apply_style <- renderUI({
  second_apply_button <- if(inherits(objects$omicsData, "seqData")) {
    bsButton("statistics_apply_style_diag", "Update diagnostic plot style") 
  } else NULL
  
  div(class = "inline-wrapper-1",
      apply_style_UI(
        "statistics",
         FALSE,
         inherits(plots$statistics_mainplot, "list"),
         one_plot_title="Update main plot style"
      ), 
    second_apply_button
  )
})

output$warnings_analysis <- renderUI({
  HTML(paste(revals$warnings_statistics, collapse = ""))
})
