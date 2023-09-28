data_summary_UI <- function() {
  tabPanel("Data Summary",
    value = "data_summary_tab",
    class = "collapse_page",
    fluidRow(
      column(
        4,
        div(
          id = "which_qc_plot_wrapper",
          class = "tooltip-wrapper",
          radioGroupButtons("which_qc_plot", "Choose a Plot Type:",
            choiceNames = list(
              div(class='inline-wrapper-1', 
                "Boxplots",
                tipify(
                  div(style = "max-width:750px", blueq),
                  ttext_[["EXPLAIN_QC_BOXPLOTS"]], 
                  options = list(container = "body")
                )
              ),
              div(class='inline-wrapper-1', 
                "Missing values barplots",
                tipify(
                  div(style = "max-width:750px", blueq),
                  ttext_[["EXPLAIN_QC_BARPLOTS"]],
                  options = list(container = "body")
                )
              ),
              div(class='inline-wrapper-1', 
                "Missing values scatterplots",
                tipify(
                  div(style = "max-width:750px", blueq),
                  ttext_[["EXPLAIN_QC_SCATTERPLOTS"]],
                  options = list(container = "body")
                )
              ),
              div(class='inline-wrapper-1', 
                "Principal components",
                tipify(
                  div(style = "max-width:750px", blueq),
                  ttext_[["EXPLAIN_QC_PCA"]],
                  options = list(container = "body")
                )
              )
            ),       
            choiceValues = c("boxplots", "bar", "scatter", "pca"),
          )
        ),
        bsCollapse(
          id = "qc_collapse", multiple = TRUE, open = c("qc_plot_params"),
          bsCollapsePanel("Plot Options",
            value = "qc_plot_params",
            conditionalPanel(
              "['boxplots', 'bar'].includes(input.which_qc_plot)",
              tagList(
                div("Order By:", class='split-title'),
                fluidRow(
                  column(6, uiOutput("qc_order_by_UI")),
                  column(6, uiOutput("qc_order_by_2_UI"))
                )
              )    
            ),
            # color selection
            tagList(
              div("Color By:", class = 'split-title'),
              fluidRow(
                column(6, uiOutput("qc_color_by_UI")),
                column(6, uiOutput("qc_color_by_2_UI"))
              )
            ),
            # shape selection
            conditionalPanel(
              "['pca'].includes(input.which_qc_plot)",
              tagList(
                div("Shape By:", class='split-title'),
                fluidRow(
                  column(6, uiOutput("qc_shape_by_UI")),
                  column(6, uiOutput("qc_shape_by_2_UI"))
                )
              )
            ),
            conditionalPanel(
              "['bar', 'scatter'].includes(input.which_qc_plot)",
                id = "js_qc_colors",
                pickerInput(
                  "qc_colors",
                  "Missing Values Colors",
                  choices = global_input_choices$MISSINGVAL_COLORS
              )
            )
          )
        ), # parent collapse
        
        actionButton("go_to_filter", "Continue to Filter Tab", width = "100%")
        
      ), # column 4
      column(
        8,
        bsCollapse(
          id = "qc_collapse_main", multiple = TRUE, open = c("plots", "axes_options"),
          bsCollapsePanel("Plots",
            value = "plots",
            uiOutput("qc_plots")
          ),
          bsCollapsePanel("Axes Options",
            value = "axes_options",
            uiOutput("qc_plot_options"),
            uiOutput("qc_apply_style")
          ),
          bsCollapsePanel("Summaries",
            value = "summaries",
            uiOutput("qc_data_summary")
          )
        )
      ) # column 8
    )
  )
}
