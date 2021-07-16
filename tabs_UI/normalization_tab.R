normalization_UI <- function() {
  tabPanel("Normalization",
    value = "normalization_tab",
    class = "collapse_page",
    column(
      4,
      bsCollapse(
        id = "normalization_sidebar", open = "normalize_global_sidebar",
        bsCollapsePanel("Global Normalization",
          value = "normalize_global_sidebar",
          hidden(radioGroupButtons("spans_or_manual", "Use SPANS or manually select a Normalization?", choices = c("Manual" = "manual", "SPANS" = "spans"))),
          # spans sub-collapse
          bsCollapse(
            id = "spans_submenu",
            hidden(bsCollapsePanel("Use SPANS to identify normalization (peptides or proteins only)",
              value = "use_spans",
              div(uiOutput("spans_conditionalbuttons"))
            )),
            bsCollapsePanel("Choose normalization parameters",
              value = "choose_params",
              #
              pickerInput("subset_fn", "Subset Function",
                choices = c(
                  "Everything" = "all", "Top L order statistics (los)" = "los", "Percentage present (ppp)" = "ppp",
                  "Complete" = "complete", "Rank invariant (rip)" = "rip", "Percentage present and rank invariant (ppp+rip)" = "ppp_rip"
                )
              ),

              div(
                id = "subset_params",
                numericInput("los", "Top order statistics Percentage (los)", value = 0.05),
                numericInput("ppp", "Percentage present (ppp)", value = 0.5),
                numericInput("rip", "Rank invariance p-value (rip)", value = 0.2)
              ),

              pickerInput("norm_fn", "Normalization Function",
                choices = c("Mean" = "mean", "Median" = "median", "Z-norm" = "zscore", "Median Absolute Distance" = "mad")
              ),
              radioGroupButtons("backtransform", "Apply backtransformation?", choices = c("Yes" = TRUE, "No" = FALSE)),
              hr(),
              hidden(bsButton("use_selected_spans", "Use parameters from table selection")),
              hidden(
                bsButton(
                "inspect_norm", 
                "Analyze currently selected normalization method",
                style = "primary"
                )
              ),
              hidden(
                div(
                  "Analyzing, please wait...",
                   id = "analyze_norm_busy", class = "fadein-out",
                   style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
                )
              )
            )
          ),
          hr(),
          hidden(bsButton("apply_normalization", "Apply normalization"))
        )
      ),
      uiOutput("warnings_normalize")
    ),
    column(
      8,
      bsCollapse(
        id = "normalization_mainpanel",
        bsCollapsePanel("SPANS Results",
          value = "spans_mainpanel",
          column(
            6,
            withSpinner(plotOutput("spans_plot"))
          ),
          column(
            6,
            div(
              withSpinner(DTOutput("spans_table"))
            )
          )
        ),
        bsCollapsePanel("Normalized Data Plots",
          value = "normdata_mainpanel",
          uiOutput("normalized_boxplots_cond")
        )
      )
    )
  )
}
