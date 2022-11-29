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
          uiOutput("stats_select_method_UI")
        )
      ),
      uiOutput("statistics_tab_sidepanel"),
      uiOutput("warnings_analysis")
    ),
    column(
      8,
      # TODO replace with either contitional UI or tabpanel that displays the 
      # appropriate plot for whatever statistical statistics we did
      uiOutput("statistics_tab_mainpanel")
    )
  )
}
