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
      if (two_lipids()) {
        report <- file.path(tempdir(), "Abundance_Template_2lipids.Rmd")
        file.copy(file.path("www", "markdowns", "Abundance_Template_2lipids.Rmd"), report, overwrite = TRUE)  
      } else {
        report <- file.path(tempdir(), "Abundance_Template.Rmd")
        file.copy(file.path("www", "markdowns", "Abundance_Template.Rmd"), report, overwrite = TRUE) 
      }
      
      # Rearrange names if rollup has happened 
      if (!is.null(objects$omicsData_pre_rollup)) {
        omicData <- objects$omicsData_pre_rollup
        omicStats <- objects$imdanova_res
        omicDataRoll <- objects$omicsData
        omicStatsRoll <- objects$peptide_imdanova_res
        omicData_2 <- NULL
      } else {
        omicData <- objects$omicsData
        omicData_2 <- objects$omicsData_2
        omicStats <- objects$imdanova_res
        omicDataRoll <- NULL
        omicStatsRoll <- NULL
      }
      
      # Pull the original edata, emeta, and fdata 
      if (two_lipids()) {
        edata <- objects$uploaded_omicsData$e_data
        edata_2 <- objects$uploaded_omicsData_2$e_data
        
        fdata <- f_data()
        
        emeta <- objects$uploaded_omicsData$e_meta
        emeta_2 <- objects$uploaded_omicsData_2$e_meta
      } else {
        if (is.null(input$file_edata$datapath)) {edata <- NULL} else {edata <- read.csv(input$file_edata$datapath)}
        if (is.null(input$file_emeta$datapath)) {emeta <- NULL} else {emeta <- read.csv(input$file_emeta$datapath)}
        if (is.null(input$file_fdata$datapath)) {fdata <- NULL} else {fdata <- read.csv(input$file_fdata$datapath)}
      
        edata_2 <- fdata_2 <- emeta_2 <- NULL
        
      }
      
      # Set up specific parameters needed by the peptide data markdown
      params <- list(titleName = input$ReportName,
                     edata = edata,
                     emeta = emeta,
                     fdata = fdata,
                     edata_2 = edata_2,
                     emeta_2 = emeta_2,
                     omicData = omicData,
                     omicData_2 = omicData_2,
                     uploaded_omicData = objects$uploaded_omicsData,
                     uploaded_omicData_2 = objects$uploaded_omicsData_2,
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