peptide_statistics_UI <- function() {
  tabPanel("Peptide Statistics",
           value = "peptide_statistics_tab",
           class = "collapse_page",
           column(
             4,
             inlineCSS("#peptide_statistics_collapse_left {margin-bottom:5px;}"),
             bsCollapse(
               id = "peptide_statistics_collapse_left", multiple = TRUE, open = "peptide_stats-statistics-options",
               bsCollapsePanel(
                 subsection_header(
                   "Specify Statistics Method",
                   "peptide_stats-statistics-ok",
                   "color:orange;float:right",
                   icon("ok", lib = "glyphicon")
                 ),
                 value = "peptide_stats-statistics-options",
                 pickerInput(
                   "peptide_stats_select_method",
                   "Select statistics method:",
                   choices = c(
                     "Select one" = NULLSELECT_,
                     "iMd-ANOVA" = "imdanova"
                   ),
                   multiple = TRUE,
                   options = pickerOptions(maxOptions = 1)
                 )
               )
             ),
             uiOutput("peptide_statistics_tab_sidepanel")
           ),
           column(
             8,
             # TODO replace with either contitional UI or tabpanel that displays the 
             # appropriate plot for whatever statistical statistics we did
             bsCollapse(
               id = "peptide_statistics_collapse_main", multiple = TRUE,
               bsCollapsePanel("Plots",
                               value = "peptide_statistics_plots",
                               radioGroupButtons("peptide_imdanova_plot_type", "Plot type", choices = c("Bar" = "bar", "Volcano" = "volcano")),
                               withSpinner(uiOutput("peptide_statistics_mainplot"))
               ),
               bsCollapsePanel("Plot Options",
                               value = "peptide_statistics_plot_opts",
                               uiOutput("peptide_statistics_plot_options")
               ),
               bsCollapsePanel("Tables",
                               value = "peptide_statistics_tables",
                               withSpinner(DTOutput("peptide_statistics_summary_table"))
               )
             )
           )
  )
}
