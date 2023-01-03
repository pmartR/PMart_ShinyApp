# This script contains the server portion of the "generate report" tab. 

list(

  # Download the markdown
  output$ReportDownload <- downloadHandler(
    
    # Set the name of the HTML file
    filename = function() {
      
      # Get the output type as HTML or PDF
      outputType <- ifelse(input$ReportType == "HTML", ".html", ".pdf")
      
      return(paste0(input$ReportName, outputType))
      
    },
    
    content = function(file) {
      
      # If no omics object has been created, trigger a warning and return NULL
      if (is.null(objects$omicsData)) {
        sendSweetAlert(session, "Markdown Report Error", 
          "Reports can only be built if the 'Upload Data' tab has been completed.", "error")
        return(NULL)
      }
          
      # Create a temporary directory and copy the report there
      report <- file.path(tempdir(), "Abundance_Template.Rmd")
      file.copy(file.path("www", "markdowns", "Abundance_Template.Rmd"), report, overwrite = TRUE)
      
      # Rearrange names if rollup has happened 
      if (!is.null(objects$omicsData_pre_rollup)) {
        omicData <- objects$omicsData_pre_rollup
        omicStats <- objects$imdanova_res
        omicDataRoll <- objects$omicsData
        omicStatsRoll <- objects$peptide_imdanova_res
      } else {
        omicData <- objects$omicsData
        omicStats <- objects$imdanova_res
        omicDataRoll <- NULL
        omicStatsRoll <- NULL
      }
      
      # Pull the original edata, emeta, and fdata 
      if (is.null(input$file_edata$datapath)) {edata <- NULL} else {edata <- read.csv(input$file_edata$datapath)}
      if (is.null(input$file_emeta$datapath)) {edata <- NULL} else {emeta <- read.csv(input$file_emeta$datapath)}
      if (is.null(input$file_fdata$datapath)) {edata <- NULL} else {fdata <- read.csv(input$file_fdata$datapath)}
      
      browser()
      
      # Set up specific parameters needed by the peptide data markdown
      params <- list(titleName = input$ReportName,
                     edata = edata,
                     emeta = emeta,
                     fdata = fdata,
                     omicData = omicData,
                     omicStats = omicStats,
                     omicDataRoll = omicDataRoll,
                     omicStatsRoll = omicStatsRoll,
                     spans_results = objects$spans_res,
                     pmart_inputs = reactiveValuesToList(input))
          
      # This general function passes the parameters and the correct report to the render function 
      rmarkdown::render(report, output_file = file, params = params, envir = new.env(parent = globalenv()))
      
      }
    
  )
  
  
)