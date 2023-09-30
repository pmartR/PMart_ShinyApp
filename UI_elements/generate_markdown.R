# This script contains the server portion of the "generate report" tab. 

  # Download the markdown
output$ReportDownload <- downloadHandler(
  
  # Set the name of the HTML file
  filename = function() {
    
    # Get the output type as HTML or PDF
    outputType <- ifelse(input$ReportType == "HTML", ".html", ".pdf")
    
    return(paste0(input$ReportName, outputType))
    
  },
  
  content = function(file) {
    withProgress(message = "Creating your report...", value = 1, {
      ### Warnings ###
      
      # If no omics object has been created, trigger a warning and return NULL
      if (is.null(objects$omicsData)) {
        sendSweetAlert(
          session, 
          "Markdown Report Error", 
          "Reports can only be built if the 'Upload Data' tab has been completed.", 
          "error")
        return(NULL)
      }
      
      ### OG files ###
      # Pull the original edata, emeta, and fdata 
      edata <- if (!isTruthy(e_data())) NULL else e_data()
      emeta <- if (!isTruthy(f_data())) NULL else f_data()
      fdata <- if (!isTruthy(revals$e_meta)) NULL else revals$e_meta
          
      ### Different rmd parameters per datatype ###
      
      ## seqdata ##
      if(inherits(objects$omicsData, "seqData")){
        template <- "Counts_Template.Rmd"
        
        add_params <- list(
          omicData = objects$omicsData,
          omicStats = objects$seqstats_res,
          omicDiag = objects$diagnostic_res
        )
      ## two lipid data
      } else if (two_lipids()) {
        template <- "Abundance_Template_2lipids.Rmd"
        
        edata <- objects$uploaded_omicsData$e_data
        edata_2 <- objects$uploaded_omicsData_2$e_data
        
        # re-assign fdata
        fdata <- f_data()
        
        emeta <- objects$uploaded_omicsData$e_meta
        emeta_2 <- objects$uploaded_omicsData_2$e_meta
  
        add_params  <- list(
          omicData  = objects$omicsData,
          omicData_2  = objects$omicsData_2,
          omicStats = objects$imdanova_res,
          edata_2  = edata_2,
          emeta_2  = emeta_2,
          uploaded_omicData = objects$uploaded_omicsData,
          uploaded_omicData_2 = objects$uploaded_omicsData_2
        )
  
      } else {
        
        ## Define data pieces
        template <- "Abundance_Template.Rmd"
        
        ## pepdata ##
        # Rearrange names if rollup has happened 
        if (!is.null(objects$omicsData_pre_rollup)) {
          omicData <- objects$omicsData_pre_rollup
          omicDataRoll <- objects$omicsData
          omicStatsRoll <- objects$peptide_imdanova_res
        } else {
          omicData <- objects$omicsData
          omicDataRoll <- NULL
          omicStatsRoll <- NULL
        }
              
        add_params <- list(
          omicData = omicData,
          omicStats = objects$imdanova_res,
          omicDataRoll = omicDataRoll,
          omicStatsRoll = omicStatsRoll,
          spans_results = objects$spans_res
        )
        
      }
      
      # parameters are some common parameters plus 'extra parameters'
      params <- c(
        list(
          titleName = input$ReportName,
          edata = edata,
          emeta = emeta,
          fdata = fdata,
          pmart_inputs = reactiveValuesToList(input),
          output_format = input$ReportType
        ),
        add_params
      )
  
      # Create a temporary directory and copy the report there
      report <- file.path(tempdir(), template)
      file.copy(file.path("www", "markdowns", template),
                report,
                overwrite = TRUE)
  
      # Pass the parameters and the correct report to the render function
      rmarkdown::render(
        report,
        output_file = file,
        output_format = ifelse(input$ReportType == "HTML", "html_document", "pdf_document"),
        params = params,
        envir = new.env(parent = globalenv())
      )
    })
  } ## content bracket
) ## downloadhandler
