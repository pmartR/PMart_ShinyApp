# main plot display which takes in the plot object that is created immediately after imd_anova() is run
output$peptide_statistics_mainplot <- renderUI({
  req(!is.null(plots$peptide_statistics_mainplot))
  # browser()
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
          "Group comparisons",
          "peptide_imdanova_groups_ok",
          "color:orange;float:right",
          icon("ok", lib = "glyphicon")
        ),
        value = "peptide_imdanova-specify-comparisons",
        pickerInput(
          "peptide_imdanova_comparison_method",
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
          "iMd-ANOVA test settings",
          "peptide_imdanova_settings_ok",
          "color:orange;float:right",
          icon("ok", lib = "glyphicon")
        ),
        value = "peptide_imdanova-select-settings",
        uiOutput("peptide_imdanova_test_method_UI"),
        uiOutput("peptide_imdanova_pval_adjust_UI"),
        numericInput("peptide_pval_thresh", "Significance threshold", value = 0.05, step = 0.01),
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
  req(input$peptide_imdanova_comparison_method)
  
  if (input$peptide_imdanova_comparison_method == "All pairwise comparisons") {
    return()
  } else if (input$peptide_imdanova_comparison_method == "Control to test condition comparisons") {
    groups <- objects$omicsData %>%
      pmartR:::get_group_table()
    groups <- groups[groups > 1] %>% names()
    
    return(
      div(
        pickerInput("peptide_imdanova_control_group",
                    "Select control group:",
                    groups,
                    selected = input$peptide_imdanova_control_group,
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
      pickerInput("peptide_imdanova_custom_comps",
                  "Select group comparisons of interest:",
                  combos,
                  selected = isolate(input$peptide_imdanova_custom_comps),
                  multiple = TRUE
      )
    )
  }
})

#'@details Select the 'treatment' groups to compare to the selected control.
#'The control selection will be disabled.
output$peptide_non_control_groups <- renderUI({
  req(input$peptide_imdanova_control_group)
  groups <- objects$omicsData %>%
    pmartR:::get_group_table()
  groups <- groups[groups > 1] %>% names()
  
  pickerInput("peptide_imdanova_non_control_groups",
              "Select group(s) to compare to control:",
              groups,
              selected = isolate(input$peptide_imdanova_non_control_groups),
              choicesOpt = list(
                disabled = groups %in% input$peptide_imdanova_control_group
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
    )
  )
  
})

# limit adjustment options depending on test method
output$peptide_imdanova_pval_adjust_UI <- renderUI({
  req(input$peptide_imdanova_test_method)
  if (input$peptide_imdanova_test_method == "anova") {
    choices <- c("Holm" = "holm", "Bonferroni" = "bonferroni", "Tukey" = "tukey", "Dunnet" = "dunnett", "None" = "none")
  }
  else {
    choices <- c("Holm" = "holm", "Bonferroni" = "bonferroni", "None" = "none")
  }
  pickerInput("peptide_pval_adjust", "Multiple comparisons adjustment", choices = choices)
})

# display table output from imd_anova
output$peptide_statistics_summary_table <- renderDT({
  objects$peptide_imdanova_res[["Full_results"]]
}, options = list(scrollX =TRUE))

#'@details UI created with the helper function style_UI to edit plot options
#'in plot.<object> function as well as axes values (if it is a ggplot)
output$peptide_statistics_plot_options <- renderUI({
  cpicker_args <- list(
    list(inputId = "pep_imd_down_cpicker", label = "Negative", value = "red"),
    list(inputId = "pep_imd_nonsig_cpicker", label = "Non significant", value = "black"),
    list(inputId = "pep_imd_up_cpicker", label = "Positive", value = "green")
  )
  
  tagList(
    h4("Main plot content"),
    conditionalPanel(
      "input.peptide_imdanova_plot_type == 'volcano'",
      div(
        class = "inline-wrapper-1",
        numericInput("peptide_imd_plot_fc_thresh", "Fold-change Threshold", value = NULL),
        radioGroupButtons(
          "peptide_stats_interactive_yn",
          choices = c("Interactive" = T, "Static" = F),
          selected = F
        )
      )
    ),
    h5("Statistical significance/fold change colors"),
    inline_cpickers(cpicker_args), # UI helper
    div(
      class = "inline-wrapper-1",
      actionButton("peptide_stats_update_plot_content", "Update plot"),
      conditionalPanel("input.peptide_stats_interactive_yn == 'TRUE'",
                       tipify(blueexcl, ttext_[["IMD_INTERACTIVE_MANY_POINTS"]]))
    ),
    conditionalPanel(
      "input.peptide_stats_interactive_yn != 'TRUE'",
      
      tagList(
        style_UI(
          "peptide_statistics",
          hr(),
          h4("Axes styling")
        ),
        uiOutput("peptide_statistics_apply_style")
      )
    )
  )
})

output$peptide_statistics_apply_style <- renderUI({
  apply_style_UI("peptide_statistics", FALSE, inherits(plots$peptide_statistics_mainplot, "list"))
})
