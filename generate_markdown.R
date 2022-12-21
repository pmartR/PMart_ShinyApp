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
        if (class(objects$omicsData) != "proData") {
          
          # Create a temporary directory and copy the report there
          report <- file.path(tempdir(), "Abundance_Template.Rmd")
          file.copy(file.path("www", "markdowns", "Abundance_Template.Rmd"), report, overwrite = TRUE)
          
          # Set up specific parameters needed by the peptide data markdown
          params <- list(titleName = input$ReportName,
                         omicData = objects$omicsData,
                         omicStats = objects$imdanova_res,
                         omicDataRoll = NULL,
                         omicStatsRoll = NULL,
                         spans_results = objects$spans_res,
                         pmart_inputs = reactiveValuesToList(input))
          
        } else {
      
          message("Using the roll-up template")
          
          report <- file.path(tempdir(), "Rollup_Template.Rmd")
          file.copy(file.path("www", "markdowns", "Rollup_Template.Rmd"), report, overwrite = TRUE)
          
          # Set up specific parameters need by the rollup data markdown
          params <- list(titleName = input$ReportName,
                         pepData = objects$uploaded_omicsData,
                         proStats = objects$imdanova_res,
                         pmart_inputs = reactiveValuesToList(input),
                         spans_results = objects$spans_res)
        
         }
        
      } else
        
      # Generate the protein data report if the input is proteins
      if (input$datatype == "pro") {
        
        # Create a temporary directory and copy the report there
        report <- file.path(tempdir(), "Protein_Template.Rmd")
        file.copy(file.path("www", "markdowns", "Protein_Template.Rmd"), report, overwrite = TRUE)
        
        # Set up specific parameters needed by the protein data markdown
        params <- list(titleName = input$ReportName,
                       proData = objects$omicsData,
                       proStats = objects$imdanova_res,
                       pmart_inputs = reactiveValuesToList(input),
                       spans_results = objects$spans_res)
        
      } else
      
      # Generate the metabolite data report if the input data is metabolites
      if (input$datatype == "metab") {
        
        # Create a temporary directory and copy the report there 
        report <- file.path(tempdir(), "Metabolite_Template.Rmd")
        file.copy(file.path("www", "markdowns", "Metabolite_Template.Rmd"), report, overwrite = TRUE)
        
        # Set up specific parameters needed by the metabolite data markdown
        params <- list(titleName = input$ReportName,
                       metabData = objects$omicsData,
                       metabStats = objects$imdanova_res, 
                       pmart_inputs = reactiveValuesToList(input))
        
      } else
        
      # Generate the lipid data report if the input data is lipidomics
      if (input$datatype == "lip") {
        
        # Create a temporary directory and copy the report there 
        report <- file.path(tempdir(), "Lipid_Template.Rmd")
        file.copy(file.path("www", "markdowns", "Lipid_Template.Rmd"), report, overwrite = TRUE)
        
        # Set up specific parameters needed by the metabolite data markdown
        params <- list(titleName = input$ReportName,
                       lipidData = objects$omicsData,
                       lipidStats = objects$imdanova_res, 
                       pmart_inputs = reactiveValuesToList(input))
        
      }
      
      else {warning("Unrecognized input multi-omics data type!")}
        
      # This general function passes the parameters and the correct report to the render function 
      rmarkdown::render(report, output_file = file, params = params, envir = new.env(parent = globalenv()))
      
      }
    
  )
  
  
)