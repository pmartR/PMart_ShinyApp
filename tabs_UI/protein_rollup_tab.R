protein_rollup_UI <- function() {
  tabPanel("Protein Roll Up",
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
                              "Compute isoforms",
                              style = "primary")),
            prettySwitch(
              inputId = "bpquant_lock",
              label = "Unlock/Lock",
              width = "100%"
            )
          )
        ),
        
        bsCollapsePanel(
          div(
            "Protein Roll Up Options",
            hidden(div(id = "prorollicon", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          # "Protein Roll Up Options",
          value = "rollup_opts",
          radioGroupButtons(
            "which_rollup",
            "Roll up method",
            c(
              "Reference" = "rrollup",
              "Z-score" = "zrollup",
              "Quantile" = "qrollup"
            )
          ),
          radioGroupButtons(
            "which_combine_fn",
            "Center By:",
            c("Median" = "median", "Mean" = "mean")
          ),
          hidden(numericInput("qrollup_thresh", "Quantile cutoff", value = 0)),
          uiOutput("bpquant_apply_icon_UI"),
          hr(),
          div(
            id = "apply_rollup_jswrapper",
            class = "tooltip-wrapper",
            bsButton("apply_rollup", "Roll up", style = "primary")
          ),
          hidden(
            div(
              "Applying roll up, please wait...",
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
          "Roll Up Results",

          # bsCollapse(
            # id = "rollup_mainpanel", multiple = TRUE, open = c("rollup_summary", "rollup_plot_opts"),
            # bsCollapsePanel("Result Plot",
                            # value = "rollup_summary",
          br(),
          uiOutput("rollup_plot_UI"),
                            # withSpinner(plotOutput("rollup_plot")),
          br(),
            # ),
            # bsCollapsePanel("Plot Options",
                            # value = "rollup_plot_opts",
          wellPanel(
            uiOutput("rollup_plot_options"),
            uiOutput("rollup_apply_style")
          )
            # )
          # )
        )
      )
      # bsCollapse(
      #   id = "rollup_mainpanel", multiple = TRUE, open = c("rollup_summary", "rollup_plot_opts"),
      #   bsCollapsePanel("Rollup Results",
      #     value = "rollup_summary",
      #     withSpinner(plotOutput("rollup_plot"))
      #   ),
      #   bsCollapsePanel("Plot Options",
      #     value = "rollup_plot_opts",
      #     uiOutput("rollup_plot_options"),
      #     uiOutput("rollup_apply_style")
      #   )
      # )
    )
  )
}
