

filter_UI <- function() {
  tabPanel("Filter",
    value = "filter_tab",
    class = "collapse_page",
    column(
      4,
      bsCollapse(
        id = "filter_collapse", multiple = FALSE, open = c("data_filters"),
        # biomolecule filters
        bsCollapsePanel(
          div(
            "Biomolecule Filters",
            hidden(div(id = "ok_data_filters", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          value = "data_filters",
          # molecule filter options
          add_filter_UI(
            filter_name = "molfilt",
            title = "Molecule Filter",
            tooltip_text = ttext_[["MOLECULE_FILTER_INFO"]],
            numericInput("mol_min_num", "Minimum number observed", 2, step = 1)
          ),
          
          # cv filter options
          add_filter_UI(
            filter_name = "cvfilt",
            title = "Coefficient of Variation Filter",
            tooltip_text = ttext_[["CV_FILTER_INFO"]],
            uiOutput("cv_threshold_UI")
          ),
          
          # imd-anova filter options
          add_filter_UI(
            filter_name = "imdanovafilt",
            title = "Coefficient of Variation Filter",
            tooltip_text = ttext_[["IMDANOVA_FILTER_INFO"]],
            numericInput("min_nonmiss_anova", "Minimum number observed to perform ANOVA", 2, step = 1),
            numericInput("min_nonmiss_gtest", "Minimum number observed to perform G-test", 3, step = 1)
          ),
          
          # proteomics filter
          add_filter_UI(
            filter_name = "profilt",
            title = "Proteomics Filter",
            tooltip_text = ttext_[["PROTEOMICS_FILTER_INFO"]],
            numericInput("min_num_peps", "Minimum number of peptides mapped to each protein:", 2, step = 1),
            checkboxInput("degen_peps", "Remove Degenerate Peptides?", TRUE)
          ),
          
          ## total count filter
          add_filter_UI(
            filter_name = "tcfilt",
            title = "Total Count Filter",
            add_btn_title = "Add/Remove",
            trailing_hr = FALSE,
            tooltip_text = ttext_[["TOTAL_COUNT_FILT_INFO"]],
            numericInput("min_num_trans", "Minimum number of transcript counts:", 10, step = 1, min = 0)
          )
        ), # end biomolecule filter collapse

        # sample filters
        bsCollapsePanel(div(
          "Sample Filters",
          hidden(div(id = "ok_sample_filters", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          value = "sample_filters",
  
          # rmd filter
          add_filter_UI(
            filter_name = "rmdfilt",
            title = "rMd Filter",
            tooltip_text = ttext_[["RMD_FILTER_INFO"]],
            add_btn_title = div(
              class = "flex-inline",
              "Add/Remove",
              tipify(
                icon("exclamation-sign", lib = "glyphicon", class = "info-right"),
                title = ttext_[["RMD_CUSTOM_FILTER_INFO"]]
              )
            ),
            colsize_l = 5, colsize_r = 7,
            numericInput("pvalue_threshold", "P-value threshold:", 0.001, step = 0.001),
            div(
              id = "rmd_metrics_js", 
              class = "inline-wrapper-1",
              uiOutput("rmd_metrics_out"),
              uiOutput("rmd_propmis_warn_icon")
            ),
            pickerInput("rmdfilt_plot_type", "Plot everything or inspect certain samples?", choices = c("Plot all samples" = "all", "Select from all samples" = "subset", "Select from outliers" = "outliers")),
            uiOutput("rmdfilt_plot_type_UI")
          ),
          
          ## RNA_filt - Library Size
          add_filter_UI(
            filter_name = "rnafilt_libsize",
            title = "RNA-seq filter (Library Size)",
            tooltip_text = ttext_[["RNA_FILT_LIB_INFO"]],
            numericInput("rnafilt_min_lib_size", "Minimum library size in sample:", value = NULL, step = 1, min = 0)
          ),
          
          ## RNA_filt - Minimum Nonzero
          add_filter_UI(
            filter_name = "rnafilt_min_nonzero",
            title = "RNA-seq filter (Minimum Nonzero)",
            trailing_hr = FALSE,
            tooltip_text = ttext_[["RNA_FILT_MIN_NONZERO_INFO"]],
            numericInput("rnafilt_min_nonzero", "Minimum number of nonzero observations:", value = NULL, step = 1, min = 0)
          )
        ),
        
        # custom filter
        bsCollapsePanel(
          div(
            "Custom Filter",
            hidden(div(id = "ok_custom_filter", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          value = "customfilt",
          div(class = "inline-wrapper-1",
            uiOutput("fdata_customfilt"),
            radioGroupButtons("remove_or_keep", label = "Remove or keep these choices?", choices = c("Remove", "Keep"), selected = "Remove")
          ), 
          hr(),
          uiOutput("edata_customfilt_pickers"), 
          hr(),
          h4("Filter by biomolecule information:"),
          uiOutput("emeta_customfilt_which_col"),
          uiOutput("emeta_customfilt_pickers"),
          hr(),
          actionButton(
            inputId = "add_customfilt",
            label = div("Add/Remove custom filter", hidden(
              div(id = "customfilt_exists", style = "color:orange;float:right", icon("ok", lib = "glyphicon"))
            )), 
            width = "50%"
          )
        )
      ), # parent collapse
      bsButton("review_filters", "Review and apply filters", style = "primary"),
      bsButton("reset_filters", "Unselect all filters", style = "primary"),
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
