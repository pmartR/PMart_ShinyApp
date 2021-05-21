download_UI <- function() {
  tabPanel("Download",
    class = "collapse_page",
    bsCollapse(
      id = "download_collapse", multiple = FALSE, open = c("download_plots", "download_tables"),
      bsCollapsePanel(div(
        "Choose plots to download",
        hidden(div(id = "ok_download_display", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
      ),
      value = "download_plots",
      fluidRow(
        column(4, DTOutput("download_plot_table")),
        column(6, plotOutput("download_plot"))
      ),
      div(
        style = "float:left",
        bsButton("mark_plot_download", "Select/de-select for download", icon = icon("minus")),
        bsButton("remove_plot_download", "Remove selected plot", icon = icon("remove"))
      )
      ),
      bsCollapsePanel(div(
        "Choose tables and results files to download",
        hidden(div(id = "ok_download_display", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
      ),
      value = "download_tables",
      DTOutput("download_tables_table"),
      div(
        style = "float:left",
        bsButton("mark_table_download", "Select/de-select for download", icon = icon("minus"))
      )
      )
    ),
    div(id = "js_zipbutton", style = "float:left", class = "grey_button", bsButton("makezipfile", label = tags$b("Bundle up all selected items"), icon = icon("briefcase"), lib = "glyphicon")),
    div(id = "js_downloadbutton", style = "margin-left:4px;float:left", class = "grey_button", downloadButton("download_processed_data", tags$b("Download bundle")))
  )
}
