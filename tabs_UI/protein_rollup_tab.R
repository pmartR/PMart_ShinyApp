protein_rollup_UI <- function() {
  tabPanel("Protein Rollup",
    value = "protein_rollup_tab",
    class = "collapse_page",
    column(
      4,
      bsCollapse(
        id = "rollup_sidebar",
        open = c("rollup_opts"),
        multiple = TRUE,
        
        bsCollapsePanel(
          div(
            "Isoform Identification",
            hidden(div(id = "isoidicon", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          # "Isoform Identification",
          value = "isoform_identification",
          
          uiOutput("bpquant_options"),
          
          hr(),
          
          splitLayout(
            disabled(bsButton("bpquant",
                              "Compute Isoforms",
                              style = "primary")),
            prettySwitch(
              inputId = "bpquant_lock",
              label = "Unlock/Lock",
              width = "100%"
            )
          ),
          br(),
          hidden(
            div(
              "Calculating isoforms, please wait...",
              id = "isoform_busy",
              class = "fadein-out",
              style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
            )
          )
        ),
        
        bsCollapsePanel(
          div(
            "Protein Rollup Options",
            hidden(div(id = "prorollicon", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          # "Protein Rollup Options",
          value = "rollup_opts",
          radioGroupButtons(
            "which_rollup",
            "Rollup Method",
            c(
              "Reference" = "rrollup",
              "Z-Score" = "zrollup",
              "Quantile" = "qrollup"
            )
          ),
          
          conditionalPanel("input.which_rollup == 'qrollup'",
                           numericInput("qrollup_thresh", "Quantile cutoff", value = 0)
          ),
          radioGroupButtons(
            "which_combine_fn",
            "Center By:",
            c("Median" = "median", "Mean" = "mean")
          ),

          uiOutput("bpquant_apply_icon_UI"),
          hr(),
          div(
            id = "apply_rollup_jswrapper",
            class = "tooltip-wrapper",
            bsButton("apply_rollup", "Roll-up", style = "primary")
          ),
          br(),
          hidden(
            div(
              "Applying rollup, please wait...",
              id = "rollup_busy",
              class = "fadein-out",
              style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
            )
          )
        )
      ),
      uiOutput("rollup_data_summary"),
      uiOutput("warnings_rollup")
    ),
    column(
      8,
      tabsetPanel(
        id = "rollup_mainpanel",
        tabPanel(
          "Rollup Results",

          br(),
          uiOutput("rollup_plot_UI"),

          br(),

          wellPanel(
            uiOutput("rollup_plot_options"),
            uiOutput("rollup_apply_style")
          )

        )
      )

    )
  )
}
