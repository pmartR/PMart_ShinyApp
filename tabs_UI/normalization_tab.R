normalization_UI <- function() {
  tabPanel("Normalization",
    value = "normalization_tab",
    class = "collapse_page",
    column(
      4,
      bsCollapse(
        id = "normalization_sidebar", 
        open = "normalize_global_sidebar",
        bsCollapsePanel(
          div("Global Normalization", 
              hidden(div(
                id = "ok_normalization", 
                style = "color:orange;float:right", 
                icon("ok", lib = "glyphicon")
                ))
              ),
          value = "normalize_global_sidebar",
          hidden(
            radioGroupButtons("spans_or_manual", 
                              "Use SPANS or manually select a normalization?", 
                              choices = c("Manual" = "manual", 
                                          "SPANS" = "spans")
                              )
            ),
          # spans sub-collapse
          bsCollapse(
            id = "spans_submenu",
            open = "choose_params",
            hidden(
              bsCollapsePanel(
                "Use SPANS to Identify Normalization (Peptides or Proteins Only)",
                value = "use_spans",
                div(uiOutput("spans_conditionalbuttons"))
            )),
            bsCollapsePanel("Choose Normalization Parameters",
              value = "choose_params",
              #
              pickerInput("subset_fn", "Subset function",
                choices = c(
                  "Everything" = "all", 
                  "Top L order statistics (los)" = "los", 
                  "Percentage present (ppp)" = "ppp",
                  "Complete" = "complete", 
                  "Rank invariant (rip)" = "rip", 
                  "Percentage present and rank invariant (ppp+rip)" = "ppp_rip"
                )
              ),

              div(
                id = "subset_params",
                numericInput("los", 
                             "Top order statistics percentage (los)", 
                             value = 0.05),
                numericInput("ppp", 
                             "Percentage present (ppp)", 
                             value = 0.5),
                numericInput("rip", 
                             "Rank invariance p-value (rip)", 
                             value = 0.2)
              ),

              pickerInput(
                "norm_fn",
                "Normalization function",
                choices = c(
                  "Median" = "median",
                  "Mean" = "mean",
                  "Z-norm" = "zscore",
                  "Median Absolute Distance" = "mad"
                )
              ), 
              radioGroupButtons("backtransform", 
                                "Apply backtransformation?", 
                                choices = c("Yes" = TRUE, "No" = FALSE)),
              hr(),
              hidden(bsButton("use_selected_spans", 
                              "Use parameters from table selection")),
              tagAppendAttributes(
                hidden(div(
                  id = "inspect_norm_tooltip",
                  class = "tooltip-wrapper",
                  bsButton("inspect_norm","Diagnostics for normalization selection",style = "primary"),
                  style = "width: 100%;"
                ))
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
          hidden(
            div(
              id = "apply_normalization_tooltip",
              class = "tooltip-wrapper",
              bsButton("apply_normalization", "Apply normalization", style = "primary")
            )
            # need to have observer run when that button is visible in the panel
            # observer maybe run on spans or manual
            # update priorities so that it runs last
            # does it need to be hidden?
            )
        )
      ),
      div(
        id = "apply_bc_method_tooltip",
        class = "tooltip-wrapper",
        bsButton("apply_bc_method", "Apply batch correction", style = "primary")
      ),
      disabled(bsButton("reset_normalization", 
                        "Remove normalization", 
                        style = "primary")),
      hidden(
        div(
          "Analyzing, please wait...",
          id = "analyze_batch_busy", class = "fadein-out",
          style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
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
            withSpinner(plotlyOutput("spans_plot"))
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
        ,
        bsCollapsePanel("Normalized Data Plots (Batch Correction)",
                        value = "batchdata_mainpanel",
                        uiOutput("batch_boxplots_cond"))
      )
    )
  )
}
