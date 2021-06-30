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
      
      # Create a temporary directory and copy report there
      tempReport <- file.path(tempdir(), "Metabolite_Template.Rmd")
      file.copy(file.path("markdowns", "Metabolite_Template.Rmd"), tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(titleName = input$ReportName,
                     metabData = objects$omicsData,
                     metabStats = objects$imdanova_res, 
                     pmart_inputs = reactiveValuesToList(input))
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      }
    
  )
  
  
)