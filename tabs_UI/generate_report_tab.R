generate_report_UI <- function() {
  tabPanel("Generate Report",
    class = "collapse_page",
    fluidRow(
      column(4,
        bsCollapse(open = c("Report_Preferences"),
          bsCollapsePanel(title = "Select Report Preferences", value = "Report_Preferences",
            textInput("ReportName", "Type Report Name", value = paste0("Report_", Sys.time() %>% as.character() %>% strsplit(" ", fixed = T) %>% unlist() %>% paste0(collapse = "_"))),
            pickerInput("ReportType", "Select Report Type", c("HTML", "PDF"), "HTML"), 
            materialSwitch("ReportCode", HTML("<strong>Add Example Code</strong>"), value = FALSE),
            downloadButton("ReportDownload", "Download Report")
          )
        )       
      )
    )
  )
}