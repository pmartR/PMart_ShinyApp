filter_UI <- function() {
  tabPanel("Filter",
    class = "collapse_page",
    column(
      4,
      bsCollapse(
        id = "filter_collapse", multiple = FALSE, open = c("data_filters"),
        # biomolecule filters
        bsCollapsePanel(div(
          "Biomolecule Filters",
          hidden(div(id = "ok_data_filters", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
        ),
        value = "data_filters",
        # molecule filter options
        fluidRow(
          column(
            6,
            actionButton(
              inputId = "add_molfilt",
              label = div("Add/Remove molecule filter", hidden(div(id = "molfilt_exists", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))),
              width = "100%"
            ),
            actionButton("plot_molfilt", "Plot filter with current values", width = "100%")
          ),
          column(
            6,
            numericInput("mol_min_num", "Minimum number observed", 2, step = 1)
          )
        ),

        hr(),
        # cv filter options
        fluidRow(
          column(
            6,
            actionButton(
              inputId = "add_cvfilt",
              label = div("Add/Remove CV filter", hidden(div(id = "cvfilt_exists", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))),
              width = "100%"
            ),
            actionButton("plot_cvfilt", "Plot this filter", width = "100%")
          ),
          column(
            6,
            uiOutput("cv_threshold_UI")
          )
        ),

        hr(),
        # imd-anova filter options
        fluidRow(
          column(
            6,
            actionButton(
              inputId = "add_imdanovafilt",
              label = div("Add/Remove imd-ANOVA filter", hidden(div(id = "imdanovafilt_exists", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))),
              width = "100%"
            ),
            actionButton("plot_imdanovafilt", "Plot this filter", width = "100%")
          ),
          column(
            6,
            numericInput("min_nonmiss_anova", "Minimum number observed to perform ANOVA", 2, step = 1),
            numericInput("min_nonmiss_gtest", "Minimum number observed to perform G-test", 3, step = 1)
          )
        ),
        hr(),
        # proteomics filter
        div(
          id = "profilt_UI",
          tagList(
            fluidRow(
              column(
                6,
                actionButton("add_profilt",
                  label = div("Add/Remove proteomics filter", hidden(div(id = "profilt_exists", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))),
                  width = "100%"
                ),
                actionButton("plot_profilt", "Plot this filter", width = "100%")
              ),
              column(
                6,
                numericInput("min_num_peps", "Minimum number of peptides mapped to each protein:", 2, step = 1),
                checkboxInput("degen_peps", "Remove Degenerate Peptides?", TRUE)
              )
            ),
            hr()
          )
        )
        ), # end biomolecule filter collapse

        # sample filters
        bsCollapsePanel(div(
          "Sample Filters",
          hidden(div(id = "ok_meta_filters", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
        ),
        value = "sample_filters",
        # rmd filter
        fluidRow(
          column(
            5,
            actionButton(
              inputId = "add_rmdfilt",
              label = div(class="flex-inline",
                "Add/Remove rMd filter", 
                tipify(
                  icon("question-sign", lib = "glyphicon", class = "info-right"),
                  title = ttext_[["RMD_CUSTOM_FILTER_INFO"]]
                ),
                hidden(div(id = "rmdfilt_exists", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
              ),
              width = "100%"
            ),
            actionButton("plot_rmdfilt", "Plot this filter", width = "100%")
          ),
          column(
            7,
            numericInput("pvalue_threshold", "P-value threshold:", 0.001, step = 0.001),
            div(
              id = "rmd_metrics_js", 
              class = "inline-wrapper-1",
              uiOutput("rmd_metrics_out"),
              uiOutput("rmd_propmis_warn_icon")
            ),
            pickerInput("rmdfilt_plot_type", "Plot everything or inspect certain samples?", choices = c("Plot all samples" = "all", "Select from all samples" = "subset", "Select from outliers" = "outliers")),
            uiOutput("rmdfilt_plot_type")
          )
        ),
        hr(),
        # custom filter
        fluidRow(
          column(
            6,
            actionButton(
              inputId = "add_customfilt",
              label = div("Add/Remove custom filter", hidden(
                div(id = "customfilt_exists", style = "color:orange;float:right", icon("ok", lib = "glyphicon"))
              )), 
              width = "100%"
            )
          ),
          column(
            6,
            radioGroupButtons("remove_or_keep", label = "Remove or keep these choices?", choices = c("Remove", "Keep"), selected = "Remove")
          )
        ),
        uiOutput("fdata_customfilt"),
        uiOutput("fdata_regex")
        )
      ), # parent collapse
      bsButton("review_filters", "Review and apply filters", style = "primary"),
      uiOutput("warnings_filter"),
      uiOutput("filter_data_summary", style="margin-top:3px")
    ), # column 4
    column(
      8,
      bsCollapse(
        id = "filter_plots", multiple = TRUE, open = c("filter_plots", "axes_options"),
        bsCollapsePanel("Visualize filters",
          value = "filter_plots",
          uiOutput("filter_dynamic_mainplot")
        ),
        bsCollapsePanel("Axes options",
          value = "axes_options",
          uiOutput("filter_plot_options"),
          uiOutput("filter_apply_style")
        )
      )
    ) # column 8
  )
}
