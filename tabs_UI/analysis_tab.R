analysis_UI <- function() {
  tabPanel("Analysis",
    class = "collapse_page",
    column(
      4,
      bsCollapse(
        id = "analysis_collapse_left", multiple = TRUE, open = "stats-analysis-options",
        bsCollapsePanel(
          subsection_header(
            "Specify Statistical Analysis",
            "stats-analysis-ok",
            "color:orange;float:right",
            icon("ok", lib = "glyphicon")
          ),
          value = "stats-analysis-options",
          pickerInput(
            "stats_select_method",
            "Select analysis method:",
            choices = c(
              "Select one" = NULLSELECT_,
              "iMd-ANOVA" = "imdanova"
            ),
            multiple = TRUE,
            options = pickerOptions(maxOptions = 1)
          )
        )
      ),
      uiOutput("analysis_tab_sidepanel")
    ),
    column(
      8,
      # TODO replace with either contitional UI or tabpanel that displays the 
      # appropriate plot for whatever statistical analysis we did
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
