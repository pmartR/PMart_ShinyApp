data_summary_UI <- function() {
  tabPanel("Data Summary",
    value = "data_summary_tab",
    class = "collapse_page",
    fluidRow(
      column(
        4,
        radioGroupButtons("which_qc_plot", "Choose a Plot Type:",
          choices = c("Boxplots" = "boxplots", "Missing Values Barplots" = "missingval_bar", "Missing Values Scatterplots" = "missingval_scatter")
        ),
        bsCollapse(
          id = "qc_collapse", multiple = TRUE, open = c("plot_type", "axes_opts"),
          bsCollapsePanel("Boxplots Options",
            value = "boxplot_opts",
            tagList(
              div("Order Boxplots By:", style = "font-weight:bold"),
              fluidRow(
                column(6, uiOutput("qc_order_by")),
                column(6, uiOutput("qc_order_by_2"))
              )
            ),
            # color selection
            tagList(
              div("Color Boxplots By:", style = "font-weight:bold"),
              fluidRow(
                column(6, uiOutput("qc_color_by")),
                column(6, uiOutput("qc_color_by_2"))
              )
            )
          ),
          bsCollapsePanel("Missing Value Options",
            value = "missingval_opts",
            # By sample or by molecule if missingval plot
            div(id = "js_missingval_type", radioGroupButtons("missingval_type", "Plot Missing Values By:",
              choices = c("Sample" = "bySample", "Biomolecule" = "byMolecule")
            )),
            div(id = "js_qc_colors", pickerInput("qc_colors", "Missing Values Colors",
              choices = c(
                "YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges", "Greys",
                "Greens", "GnBu", "BuPu", "BuGn", "Blues", "Set3", "Set2", "Set1", "Pastel2", "Pastel1", "Paired", "Dark2", "Accent",
                "Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"
              )
            ))
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
