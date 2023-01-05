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
    if (is.null(input$file_edata$datapath)) edata <- NULL else 
      edata <- read.csv(input$file_edata$datapath)
    if (is.null(input$file_emeta$datapath)) emeta <- NULL else 
      emeta <- read.csv(input$file_emeta$datapath)
    if (is.null(input$file_fdata$datapath)) fdata <- NULL else 
      fdata <- read.csv(input$file_fdata$datapath)
    
    ### Specify constant params ###
    
    params <- list(titleName = input$ReportName,
                   edata = edata,
                   emeta = emeta,
                   fdata = fdata,
                   pmart_inputs = reactiveValuesToList(input))
    
    ### Different rmd parameters per datatype ###
    
    ## seqdata ##
    if(inherits(objects$omicsData, "seqData")){
      template <- "Count_Template.Rmd"
      
      add_params <- list(
        omicData = objects$omicsData,
        omicStats = objects$seqstats_res,
        omicDiag = objects$diagnostic_res
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
    
    ## final params ##
    params <- c(params, add_params)
    
    # Create a temporary directory and copy the report there
    report <- file.path(tempdir(), template)
    file.copy(file.path("www", "markdowns", template),
              report,
              overwrite = TRUE)

    # Pass the parameters and the correct report to the render function
    rmarkdown::render(
      report,
      output_file = file,
      params = params,
      envir = new.env(parent = globalenv())
      )

    } ## content bracket
) ## downloadhandler
