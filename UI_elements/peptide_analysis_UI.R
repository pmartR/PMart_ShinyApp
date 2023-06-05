# main plot display which takes in the plot object that is created immediately after imd_anova() is run
output$peptide_statistics_mainplot <- renderUI({
  req(!is.null(plots$peptide_statistics_mainplot))
  p <- plots$peptide_statistics_mainplot
  plots$last_plot <- p
  if (inherits(p, "plotly")) {
    output$peptide_statistics_mainplot_plotly <- renderPlotly(p)
    plotlyOutput("peptide_statistics_mainplot_plotly")
  }
  else if (inherits(p, "ggplot")) {
    output$peptide_statistics_mainplot_ggplot <- renderPlot(p)
    plotOutput("peptide_statistics_mainplot_ggplot")
  }
})

#'@details Conditional radiogroupbuttons depending on what plot types are valid
#'for the iMd-ANOVA analysis type that was run.
output$peptide_imdanova_plot_type_UI <- renderUI({
  validate(need(objects$peptide_imdanova_res, "No iMd-ANOVA results found, run iMd-ANOVA from the side panel."))
  
  choices = switch(
    attr(objects$peptide_imdanova_res, "statistical_test"),
    "combined" = c("Bar" = "bar", "Volcano" = "volcano", "Counts Heatmap" = "gheatmap"),
    "anova" = c("Bar" = "bar", "Volcano" = "volcano"),
    "gtest" = c("Bar" = "bar", "Counts Heatmap" = "gheatmap"),
    "__INVALID__"
  )
  
  validate(need(choices != "__INVALID__", "If you're seeing this message, someone (maybe us..) messed up and your stats results may be corrupted."))
  
  radioGroupButtons(
    "peptide_imdanova_plot_type",
    "Plot type",
    choices = choices
  )
})

#'@details Pickers for ANOVA/G-test pvalue adjustment.  One or two pickers are
#'shown depending on if the user chooses the 'combined' option for iMd-ANOVA
output$peptide_imdanova_pval_adjust_UI <- renderUI({
  validate(need(
    input$peptide_imdanova_test_method %in% c("anova", "gtest", "combined"),
    "Please select test method."
  ))
  
  # to make things look nice
  if(input$peptide_imdanova_test_method == 'combined') {
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
    "peptide_imdanova_pval_adjust_a_multcomp",
    title_anova_mc,
    choices = c(
      "Holm" = "holm",
      "Bonferroni" = "bonferroni",
      "Tukey" = "tukey",
      "Dunnet" = "dunnett",
      "None" = "none"
    ),
    selected = "holm"
  )
  
  gtest_picker_mc <- pickerInput(
    "peptide_imdanova_pval_adjust_g_multcomp",
    title_gtest_mc,
    choices = c(
      "Holm" = "holm",
      "Bonferroni" = "bonferroni",
      "None" = "none"
    ),
    selected = "holm"
  )
  
  anova_picker_fdr <-  pickerInput(
    "peptide_imdanova_pval_adjust_a_fdr",
    title_anova_fdr,
    choices = c(
      "Benjamini-Hochberg (FDR)" = "BH",
      "Benjamini-Yekutieli" = "BY",
      "Bonferroni" = "bonferroni",
      "None" = "none"
    ),
    selected = "BH"
  )
  
  gtest_picker_fdr <- pickerInput(
    "peptide_imdanova_pval_adjust_g_fdr",
    title_gtest_fdr,
    choices = c(
      "Benjamini-Hochberg (FDR)" = "BH",
      "Benjamini-Yekutieli" = "BY",
      "Bonferroni" = "bonferroni",
      "None" = "none"
    ),
    selected = "BH"
  )
  
  anova_pickers = tagList(anova_picker_mc, anova_picker_fdr)
  gtest_pickers = tagList(gtest_picker_mc, gtest_picker_fdr)
  
  if (input$peptide_imdanova_test_method == "anova") {
    return(anova_pickers)
  }
  else if (input$peptide_imdanova_test_method == "gtest") {
    return(gtest_pickers)
  } 
  else if(input$peptide_imdanova_test_method == "combined") {
    return(tagList(
      div(class='inline-wrapper-1', 
        tags$b("Multiple comparisons adjustment"),
        tipify(blueq, ttext_[["WHAT_IS_MC"]])
      ),
      fluidSplitLayout(anova_picker_mc, gtest_picker_mc),
      div(class='inline-wrapper-1', 
        tags$b("FDR adjustment"),
        tipify(blueq, ttext_[["WHAT_IS_FDR"]])
      ),
      fluidSplitLayout(anova_picker_fdr, gtest_picker_fdr)
    ))
  }
})

#'@details returns a different sidepanel of options depending on what stats
#'statistics was selected.
output$peptide_statistics_tab_sidepanel <- renderUI({
  req(input$peptide_stats_select_method != NULLSELECT_)
  
  if (input$peptide_stats_select_method == "imdanova"){
    bsCollapse(
      id = "peptide_imdanova-sidepanel-options", multiple = TRUE, 
      open = c(
        "peptide_imdanova-specify-comparisons", 
        "peptide_imdanova-select-settings"
      ), 
      #
      bsCollapsePanel(
        subsection_header(
          "Group Comparisons",
          "peptide_imdanova_groups_ok",
          "color:orange;float:right",
          icon("ok", lib = "glyphicon")
        ),
        value = "peptide_imdanova-specify-comparisons",
        pickerInput(
          "peptide_comparison_method",
          "Select comparisons to use (singleton groups excluded):",
          c(
            "All pairwise comparisons",
            "Control to test condition comparisons",
            "Custom comparisons"
          ),
          selected = character(0),
          options = pickerOptions(maxOptions = 1),
          multiple = TRUE
        ),
        
        uiOutput("peptide_pairwise_comp_selector"),
        
        hr(),
        
        DTOutput("peptide_pairwise_comp_display"),
        
        tags$script('Shiny.addCustomMessageHandler("unbind-DT-RR", function(x) {
                      Shiny.unbindAll($(document.getElementById(x)).find(".dataTable"));
                      });')
      ),
      bsCollapsePanel(
        subsection_header(
          "iMd-ANOVA Test Settings",
          "peptide_imdanova_settings_ok",
          "color:orange;float:right",
          icon("ok", lib = "glyphicon")
        ),
        value = "peptide_imdanova-select-settings",
        uiOutput("peptide_imdanova_test_method_UI"),
        uiOutput("peptide_imdanova_pval_adjust_UI"),
        numericInput("peptide_pval_thresh", "Significance threshold", min = 0, max = 1, value = 0.05, step = 0.01),
        div(
          id = "peptide_apply_imdanova_jswrapper", 
          class= "tooltip-wrapper",
          bsButton("peptide_apply_imdanova", "Perform iMd-ANOVA", style = "primary")
        ),
        br(), br(),
        hidden(
          div(
            "Conducting analysis, please wait...",
            id = "peptide_analysis_busy",
            class = "fadein-out",
            style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
          )
        )
      )
    )
  }
})

#'@details Give options for comparisons depending on what user selected as the
#' method to perform comparisons (All comparisons, control vs treatment, custom)
output$peptide_pairwise_comp_selector <- renderUI({
  req(input$peptide_comparison_method)
  
  if (input$peptide_comparison_method == "All pairwise comparisons") {
    return()
  } else if (input$peptide_comparison_method == "Control to test condition comparisons") {
    groups <- objects$omicsData %>%
      pmartR:::get_group_table()
    groups <- groups[groups > 1] %>% names()
    
    return(
      div(
        pickerInput("peptide_control_group",
                    "Select control group:",
                    groups,
                    selected = input$peptide_control_group,
                    options = pickerOptions(maxOptions = 1),
                    multiple = TRUE
        ),
        uiOutput("peptide_non_control_groups")
      )
    )
  } else {
    groups <- objects$omicsData %>%
      pmartR:::get_group_table() %>%
      names()
    combos <- apply(combn(groups, 2), 2, toString)
    
    return(
      pickerInput("peptide_custom_comps",
                  "Select group comparisons of interest:",
                  combos,
                  selected = isolate(input$peptide_custom_comps),
                  multiple = TRUE
      )
    )
  }
})

#'@details Select the 'treatment' groups to compare to the selected control.
#'The control selection will be disabled.
output$peptide_non_control_groups <- renderUI({
  req(input$peptide_control_group)
  groups <- objects$omicsData %>%
    pmartR:::get_group_table()
  groups <- groups[groups > 1] %>% names()
  
  pickerInput("peptide_imdanova_non_control_groups",
              "Select group(s) to compare to control:",
              groups,
              selected = isolate(input$peptide_imdanova_non_control_groups),
              choicesOpt = list(
                disabled = groups %in% input$peptide_control_group
              ),
              multiple = TRUE
  )
})

#'@details display the data-table of possible comparisons.
output$peptide_pairwise_comp_display <- renderDT(
  {
    req(!is.null(comp_df_holder$comp_df))
    session$sendCustomMessage("unbind-DT-RR", "peptide_pairwise_comp_display")
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
output$peptide_imdanova_test_method_UI <- renderUI({
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
      pickerInput("peptide_imdanova_test_method",
                  "Test method:",
                  c(
                    "ANOVA" = "anova",
                    "G-Test" = "gtest",
                    "Combined" = "combined"
                  ),
                  selected = input$peptide_imdanova_test_method,
                  options = pickerOptions(maxOptions = 1),
                  multiple = TRUE,
                  choicesOpt = choicesOpt
      ),
      tmp_tooltip
    )
  )
  
})

# display table output from imd_anova
output$peptide_statistics_summary_table <- renderDT({
  objects$peptide_imdanova_res
}, options = list(scrollX =TRUE))

#'@details UI created with the helper function style_UI to edit plot options
#'in plot.<object> function as well as axes values (if it is a ggplot)
output$peptide_statistics_plot_options <- renderUI({
  cpicker_args <- list(
    list(inputId = "pep_imd_down_cpicker", label = "Negative", value = "red"),
    list(inputId = "pep_imd_nonsig_cpicker", label = "Non significant", value = "black"),
    list(inputId = "pep_imd_up_cpicker", label = "Positive", value = "green")
  )
  
  high_low_args <- list(
    list(inputId = "pep_imd_low_cpicker", label = "Low", value = "#132B43"),
    list(inputId = "pep_imd_high_cpicker", label = "High", value = "#56B1F7")
  )
  
  tagList(
    h4("Main plot content"),
    div(
      class = "inline-wrapper-1",
      conditionalPanel(
        "['volcano'].includes(input.peptide_imdanova_plot_type)",
        numericInput("peptide_imd_plot_fc_thresh", "Fold-change Threshold", value = NULL) 
      ),
      conditionalPanel(
        "['volcano', 'gheatmap'].includes(input.peptide_imdanova_plot_type)",
        radioGroupButtons(
          "peptide_stats_interactive_yn",
          choices = c("Interactive" = T, "Static" = F),
          selected = F
        )
      )
    ),
    conditionalPanel(
      "['volcano', 'bar'].includes(input.peptide_imdanova_plot_type)",
      tags$b(h5("Statistical significance/fold change colors")),
      inline_cpickers(cpicker_args), # UI helper
    ),
    conditionalPanel(
      "['gheatmap'].includes(input.peptide_imdanova_plot_type)",
      tags$b(h5("Low/High count colors")),
      inline_cpickers(high_low_args), # UI helper
    ),
    div(
      class = "inline-wrapper-1",
      actionButton("peptide_stats_update_plot_content", "Update plot"),
      conditionalPanel("input.peptide_stats_interactive_yn == 'TRUE'",
                       tipify(blueexcl, ttext_[["IMD_INTERACTIVE_MANY_POINTS"]]))
    ),
    conditionalPanel(
      "input.peptide_stats_interactive_yn == 'FALSE'",
      
      tagList(
        style_UI(
          "peptide_statistics",
          hr(),
          h4("Axes styling")
        ),
        uiOutput("peptide_statistics_apply_style")
      )
  ))
})

output$peptide_statistics_apply_style <- renderUI({
  apply_style_UI("peptide_statistics", FALSE, inherits(plots$peptide_statistics_mainplot, "list"))
})

output$warnings_peptide_analysis <- renderUI({
  HTML(paste(revals$warnings_peptide_statistics, collapse = ""))
})

