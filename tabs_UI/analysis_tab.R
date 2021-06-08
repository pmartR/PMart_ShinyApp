analysis_UI <- function() {
  tabPanel("Analysis",
    class = "collapse_page",
    column(
      4,
      bsCollapse(
        id = "analysis_collapse_left", multiple = TRUE, open = "imdanova_options",
        bsCollapsePanel(div(
          "iMd-ANOVA",
          hidden(div(id = "ok_imdanova", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
        ),
        value = "imdanova_options",

        radioGroupButtons("test_method", "Test method:", c("ANOVA" = "anova", "G-Test" = "gtest", "Combined" = "combined")),
        pickerInput("pval_adjust", "Multiple comparisons adjustment", choices = c("Holm" = "holm", "Bonferroni" = "bonferroni", "Tukey" = "tukey", "Dunnet" = "dunnett", "None" = "none")),
        numericInput("pval_thresh", "Significance threshold", value = 0.05, step = 0.01),
        hr(),
        bsButton("apply_imdanova", "Perform iMd-ANOVA")
        )
      )
    ),
    column(
      8,
      bsCollapse(
        id = "analysis_collapse_main", multiple = TRUE,
        bsCollapsePanel("Plots",
          value = "analysis_plots",
          radioGroupButtons("imdanova_plot_type", "Plot type", choices = c("Bar" = "bar", "Volcano" = "volcano")),
          plotOutput("analysis_mainplot")
        ),
        bsCollapsePanel("Plot Options",
          value = "analysis_plot_opts",
          uiOutput("analysis_plot_options"),
          uiOutput("analysis_apply_style")
        ),
        bsCollapsePanel("Tables",
          value = "analysis_tables",
          DTOutput("analysis_summary_table")
        )
      )
    )
  )
}
