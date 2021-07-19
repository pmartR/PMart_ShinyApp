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
      
      # There are 5 report styles: 1. peptide data, 2. rolled up protein data, 
      # 3. protein data, 4. metabolite data, 5. lipid data
      
      # Generate the peptide data report if the input data is peptides and roll-up did NOT happen
      if (input$datatype == "pep") {
        
        # Check for peptide roll-up into protein data
        if (class(objects$omicData) != "proData") {
          
          # Create a temporary directory and copy the report there
          report <- file.path(tempdir(), "Peptide_Template.Rmd")
          file.copy(file.path("markdowns", "Peptide_Template.Rmd"), report, overwrite = TRUE)
          
          # Set up specific parameters needed by the peptide data markdown
          params <- list(titleName = input$ReportName,
                         pepData = objects$omicsData,
                         pepStats = objects$imdanova_res,
                         pmart_inputs = reactiveValuesToList(input),
                         spans_results = objects$spans_res)
          
          
        } else {
      
          # objects$omicsData_pre_rollup
          
          
        }
        
  
      } else
      
      # Generate the metabolite data report if the input data is metabolites
      if (input$datatype == "metab") {
        
        # Create a temporary directory and copy the report there 
        report <- file.path(tempdir(), "Metabolite_Template.Rmd")
        file.copy(file.path("markdowns", "Metabolite_Template.Rmd"), report, overwrite = TRUE)
        
        # Set up specific parameters needed by the metabolite data markdown
        params <- list(titleName = input$ReportName,
                       metabData = objects$omicsData,
                       metabStats = objects$imdanova_res, 
                       pmart_inputs = reactiveValuesToList(input))
        
      }
      
      # This general function passes the parameters and the correct report to the render function 
      rmarkdown::render(report, output_file = file, params = params, envir = new.env(parent = globalenv()))
      
      }
    
  )
  
  
)