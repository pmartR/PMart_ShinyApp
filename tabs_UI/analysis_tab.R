statistics_UI <- function() {
  tabPanel("Statistics",
    value = "statistics_tab",
    class = "collapse_page",
    column(
      4,
      inlineCSS("#statistics_collapse_left {margin-bottom:5px;}"),
      bsCollapse(
        id = "statistics_collapse_left", multiple = TRUE, open = "stats-statistics-options",
        bsCollapsePanel(
          subsection_header(
            "Specify Statistic Method",
            "stats-statistics-ok",
            "color:orange;float:right",
            icon("ok", lib = "glyphicon")
          ),
          value = "stats-statistics-options",
          pickerInput(
            "stats_select_method",
            "Select statistics method:",
            choices = c(
              "Select one" = NULLSELECT_,
              "iMd-ANOVA" = "imdanova"
            ),
            multiple = TRUE,
            options = pickerOptions(maxOptions = 1)
          )
        )
      ),
      uiOutput("statistics_tab_sidepanel"),
      uiOutput("warnings_analysis")
    ),
    column(
      8,
      # TODO replace with either contitional UI or tabpanel that displays the 
      # appropriate plot for whatever statistical statistics we did
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
    )
  )
}
