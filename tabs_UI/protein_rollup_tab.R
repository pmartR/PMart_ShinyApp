protein_rollup_UI <- function() {
  tabPanel("Protein Rollup",
    class = "collapse_page",
    column(
      4,
      bsCollapse(
        id = "rollup_sidebar", open = "rollup_opts", multiple = TRUE,
        bsCollapsePanel("Protein Rollup Options",
          value = "rollup_opts",
          radioGroupButtons("which_rollup", "Rollup Method", c("Reference" = "rrollup", "Z-Score" = "zrollup", "Quantile" = "qrollup")),
          radioGroupButtons("which_combine_fn", "Center By:", c("Median" = "median", "Mean" = "mean")),
          numericInput("qrollup_thresh", "Quantile cutoff", value = 0),
          hr(),
          bsButton("apply_rollup", "Roll-up"),
          hidden(div("Applying rollup, please wait...",
            id = "rollup_busy", class = "fadein-out",
            style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
          ))
        )
      ),
      uiOutput("rollup_data_summary"),
      uiOutput("warnings_rollup")
    ),
    column(
      8,
      bsCollapse(
        id = "rollup_mainpanel", multiple = TRUE, open = c("rollup_summary", "rollup_plot_opts"),
        bsCollapsePanel("Rollup Results",
          value = "rollup_summary",
          plotOutput("rollup_plot")
        ),
        bsCollapsePanel("Plot Options",
          value = "rollup_plot_opts",
          uiOutput("rollup_plot_options"),
          uiOutput("rollup_apply_style")
        )
      )
    )
  )
}
