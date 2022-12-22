download_UI <- function() {
  tabPanel("Download",
    value = "download_tab",
    class = "collapse_page",
    bsCollapse(
      id = "download_collapse", multiple = TRUE, open = c("download_plots", "download_tables"),
      bsCollapsePanel(div(
        "Choose plots to download",
        hidden(div(id = "ok_download_display", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
      ),
      value = "download_plots",
      fluidRow(
        column(4, withSpinner(DTOutput("download_plot_table"))),
        column(8, 
               uiOutput("download_plot_UI"),
               hidden(div(id = "download_plot_options_UI",
                 bsCollapsePanel("Axes Options",
                                 value = "axes_options",
                                 uiOutput("download_apply_style_UI"),
                                 apply_style_UI("download", FALSE, FALSE)
                 ),
                 bsCollapsePanel("Save Options",
                                 value = "save_options",
                                 div(
                                   uiOutput("plot_selected_save_options"),
                                   div(class = "inline-wrapper-1",
                                       div(id = "download_apply_save_options_tooltip", class = "tooltip-wrapper", actionButton("download_apply_save_options", "Apply")),
                                       conditionalPanel("input.download_file_type!='HTML Widget'", actionButton("download_preview_image", "Preview"))
                                   ),
                                   br(),
                                   br(),
                                   div(style="overflow:auto", uiOutput("download_image_preview"))
                                 ))
                                 
                ))
               )
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
      ),
      bsCollapsePanel("Generate Report", 
                      textInput("ReportName", "Name Report", "pmartR_Report"), 
                      pickerInput("ReportType", "Report Type", c("HTML", "PDF"), "HTML"),
                      downloadButton("ReportDownload", "Make Report"))
    ),
    div(id = "js_zipbutton", style = "float:left", class = "grey_button", bsButton("makezipfile", label = tags$b("Bundle up all selected items"), icon = icon("briefcase"), lib = "glyphicon")),
    div(id = "js_downloadbutton", style = "margin-left:4px;float:left", class = "grey_button", downloadButton("download_processed_data", tags$b("Download bundle")))
  )
}
