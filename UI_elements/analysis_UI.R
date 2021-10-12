# main plot display which takes in the plot object that is created immediately after imd_anova() is run
output$statistics_mainplot <- renderUI({
  req(!is.null(plots$statistics_mainplot))
  p <- plots$statistics_mainplot
  plots$last_plot <- p
  if (inherits(p, "plotly")) {
    output$statistics_mainplot_plotly <- renderPlotly(p)
    plotlyOutput("statistics_mainplot_plotly")
  }
  else if (inherits(p, "ggplot")) {
    output$statistics_mainplot_ggplot <- renderPlot(p)
    plotOutput("statistics_mainplot_ggplot")
  }
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

#'@details returns a different sidepanel of options depending on what stats
#'statistics was selected.
output$statistics_tab_sidepanel <- renderUI({
  req(input$stats_select_method != NULLSELECT_)
  
  if (input$stats_select_method == "imdanova"){
    bsCollapse(
      id = "imdanova-sidepanel-options", multiple = TRUE, 
      open = c("imdanova-specify-comparisons", "imdanova-select-settings"), 
      #
      bsCollapsePanel(
        subsection_header(
          "Group comparisons",
          "imdanova_groups_ok",
          "color:orange;float:right",
          icon("ok", lib = "glyphicon")
        ),
        value = "imdanova-specify-comparisons",
        pickerInput(
          "imdanova_comparison_method",
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
        
        uiOutput("pairwise_comp_selector"),
        
        hr(),
        
        DTOutput("pairwise_comp_display"),
        
        tags$script('Shiny.addCustomMessageHandler("unbind-DT-RR", function(x) {
                      Shiny.unbindAll($(document.getElementById(x)).find(".dataTable"));
                      });')
      ),
      bsCollapsePanel(
        subsection_header(
          "iMd-ANOVA test settings",
          "imdanova_settings_ok",
          "color:orange;float:right",
          icon("ok", lib = "glyphicon")
        ),
        value = "imdanova-select-settings",
        uiOutput("imdanova_test_method_UI"),
        uiOutput("imdanova_pval_adjust_UI"),
        numericInput("pval_thresh", "Significance threshold", value = 0.05, step = 0.01),
        bsButton("apply_imdanova", "Perform iMd-ANOVA", style = "primary"),
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
output$pairwise_comp_selector <- renderUI({
  req(input$imdanova_comparison_method)
  
  if (input$imdanova_comparison_method == "All pairwise comparisons") {
    return()
  } else if (input$imdanova_comparison_method == "Control to test condition comparisons") {
    groups <- objects$omicsData %>%
      pmartR:::get_group_table()
    groups <- groups[groups > 1] %>% names()
    
    return(
      div(
        pickerInput("imdanova_control_group",
                    "Select control group:",
                    groups,
                    selected = input$imdanova_control_group,
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
      pickerInput("imdanova_custom_comps",
                  "Select group comparisons of interest:",
                  combos,
                  selected = isolate(input$imdanova_custom_comps),
                  multiple = TRUE
      )
    )
  }
})

#'@details Select the 'treatment' groups to compare to the selected control.
#'The control selection will be disabled.
output$non_control_groups <- renderUI({
  req(input$imdanova_control_group)
  groups <- objects$omicsData %>%
    pmartR:::get_group_table()
  groups <- groups[groups > 1] %>% names()
  
  pickerInput("imdanova_non_control_groups",
              "Select group(s) to compare to control:",
              groups,
              selected = isolate(input$imdanova_non_control_groups),
              choicesOpt = list(
                disabled = groups %in% input$imdanova_control_group
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
  
  filt_method <- attributes(objects$omicsData)$filters$imdanovaFilt$filter_method
  
  if (is.null(filt_method) || length(filt_method) == 2) {
    groupsizes <- pmartR:::get_group_table(objects$omicsData)
    groupsizes <- groupsizes[groupsizes > 1]
    groupsizes <- groupsizes[names(groupsizes) %in% unlist(comp_df_holder$comp_df[1:2])]
    
    if (any(groupsizes < 3)) {
      disabler <- c(
        "ANOVA" = "anova",
        "G-Test" = "gtest",
        "Combined" = "combined"
      ) %in% c("gtest", "combined")
    } else {
      disabler <- NULL
    }
  } else {
    disabler <- !(c(
      "ANOVA" = "anova",
      "G-Test" = "gtest",
      "Combined" = "combined"
    ) %in% filt_method)
  }
  
  choicesOpt <-  list(subtext = c(
    "Requires 2+ members per group",
    "Requires 3+ members per group",
    "Requires 3+ members per group"
  ))
  
  if (is.null(disabler)) {
    invisible()
  } else {
    choicesOpt[['disabled']] = disabler
  }
  
  return(
    pickerInput("imdanova_test_method",
                "Test method:",
                c(
                  "ANOVA" = "anova",
                  "G-Test" = "gtest",
                  "Combined" = "combined"
                ),
                selected = input$imdanova_test_method,
                options = pickerOptions(maxOptions = 1),
                multiple = TRUE,
                choicesOpt = choicesOpt
    )
  )
  
})

# limit adjustment options depending on test method
output$imdanova_pval_adjust_UI <- renderUI({
  req(input$imdanova_test_method)
  if (input$imdanova_test_method == "anova") {
    choices <- c("Holm" = "holm", "Bonferroni" = "bonferroni", "Tukey" = "tukey", "Dunnet" = "dunnett", "None" = "none")
  }
  else {
    choices <- c("Holm" = "holm", "Bonferroni" = "bonferroni", "None" = "none")
  }
  pickerInput("pval_adjust", "Multiple comparisons adjustment", choices = choices)
})

# display table output from imd_anova
output$statistics_summary_table <- renderDT({
  objects$imdanova_res[["Full_results"]]
}, options = list(scrollX =TRUE))

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
        "['volcano'].includes(input.imdanova_plot_type)",
        numericInput("imd_plot_fc_thresh", "Fold-change Threshold", value = NULL) 
      ),
      conditionalPanel(
        "['volcano', 'gheatmap'].includes(input.imdanova_plot_type)",
        radioGroupButtons(
          "stats_interactive_yn",
          choices = c("Interactive" = T, "Static" = F),
          selected = F
        )
      )
    ),
    conditionalPanel(
      "['volcano', 'bar'].includes(input.imdanova_plot_type)",
      tags$b(h5("Statistical significance/fold change colors")),
      inline_cpickers(cpicker_args), # UI helper
    ),
    conditionalPanel(
      "['gheatmap'].includes(input.imdanova_plot_type)",
      tags$b(h5("Low/High count colors")),
      inline_cpickers(high_low_args), # UI helper
    ),
    div(
      class = "inline-wrapper-1",
      actionButton("stats_update_plot_content", "Update plot"),
      conditionalPanel("input.stats_interactive_yn == 'TRUE'",
                       tipify(blueexcl, ttext_[["IMD_INTERACTIVE_MANY_POINTS"]]))
    ),
    conditionalPanel(
      "input.stats_interactive_yn == 'FALSE'",
      tagList(
        style_UI(
          "statistics",
          hr(),
          h4("Axes styling")
        ),
        uiOutput("statistics_apply_style")
      )
    )
  )
})

output$statistics_apply_style <- renderUI({
  apply_style_UI("statistics", FALSE, inherits(plots$statistics_mainplot, "list"))
})
