## Functionality that was added for MAP 
## Last Updated: 2021_09_14

list(
  
  # Read parameters file for URL 
  Parameters <- read.table("./Parameters.txt", header = TRUE),
  
  observe({
    
    # Parse the query string at the url header
    query <- parseQueryString(session$clientData$url_search)
    
    # Set a conditional test. We only care if the "data" parameter exists. 
    cond <- length(query) != 0 && "data" %in% names(query) 
    
    # If true, open the project data and put each piece where it belongs 
    if (cond) {
      
      # Get the data that was uploaded, and determine whether it is a project object,
      # or a midpoint object.
      pullData <- get_data(MapConnect$MapConnect, query$data)
      
      # If project in the names, then it's a project object
      if ("Project" %in% names(pullData)) {
        
        # Pull the project object
        project <- get_data(MapConnect$MapConnect, query$data)
        
        # Update MAP Connect object (used in upload_UI.R tab)
        MapConnect$Project <- project
        
        # Convert from David's project annotations to Daniel's shorthand
        convertList <- list("Peptide" = "pep", "Protein" = "pro", "Lipidomics" = "lip", 
                            "Metabolomics" = "metab")
        
        # Get the data type
        theDataType <- convertList[lapply(names(convertList), function(name) {
          grepl(name, project$Project$DataType)}) %>% unlist()] %>% unlist()
        
        # Specify file type and disable input
        updatePickerInput(session, "datatype", selected = theDataType)
        disable(id = "datatype")
        
      } else {
        
        # If the object isn't a project, then it's a midpoint
        MidPointFile <- pullData
        
        # Ensure this is a pmart app midpoint file
        if (MidPointFile$MidPointFile$`App Name` == "pmart app") {
        
          # Freeze all pages up to "Tab"
          Tab <- MidPointFile$MidPointFile$Tracking$Tab
          
          message(paste("Loading data through", Tab, "tab"))
          
          # These tabs will need to be disabled regardless
          js$disableTab("upload_data_tab")
          js$disableTab("group_samples_tab")
          js$disableTab("data_summary_tab")
          js$disableTab("filter_tab")
          js$disableTab("normalization_tab")
          
          # Add additional tabs depending on what kind of data was exported
          if (Tab %in% c("peptide_statistics", "protein_rollup", "Statistics")) {
            js$disableTab("peptide_statistics_tab")
          } 
          if (Tab %in% c("protein_rollup", "Statistics")) {
            js$disableTab("protein_rollup_tab")
          } 
          if (Tab == "Statistics") {
            js$disableTab("statistics_tab")
          }
          
          # Load omics data 
          objects$omicsData <- MidPointFile$MidPointFile$`Data Objects`
    
          
        } else {
          
          sendSweetAlert(session, "Wrong MidPoint File Upload!", 
                         "Please email david.degnan@pnnl.gov if you see this error.", "error")
          
        }
        
      }
    
    }
    
    
  }), 
  
  # Set an observer to enable / disable the "Save and Export Progress" button
  observe({
    
    # Disable midpoint button by default
    disable("exportMid")
    
    # If omics data is not NULL, check that "is_normalized" is set to TRUE
    if (is.null(objects$omicsData) == FALSE) {
      if (attr(objects$omicsData, "data_info")$norm_info$is_normalized) {
        enable("exportMid")
      } 
    }
    
  }),
  
  # Observe the export button being clicked, and generate a modal UI to confirm operation
  observeEvent(input$exportMid, {
    
    showModal(modalDialog(fluidPage(
      paste0("This will save results through the Normalization tab and close the app.", 
      " Results can be reopened at this point in MAP. Continue?"),
      textInput("MidpointName", "Enter Name for Save File:", value = "MySavedData")
    ),
      title = HTML('<p style="text-align: center;">Confirm Save and Export?</p>'),
      footer = list(actionButton("exportMidConfirm", "Confirm Data Export & Close App"),
                    onclick = paste0("window.top.location.href = ", Parameters[Parameters$Parameter == "MAP_URL", "Local"]), 
                    modalButton("Exit")), size = "m", easyClose = T))
    
  }),
  
  # If the confirm exit button is clicked, midpoint data is exported to MAP and the app closes. 
  observeEvent(input$exportMidConfirm, {
    
    # If no omics data, which is possible if the user clicks this button before
    # the app finishes initializing, return NULL
    if (is.null(objects$omicsData)) {removeModal(); return(NULL)}
    
    # Get all current file names 
    FileNames <- lapply(get_all_data_ids(MapConnect$MapConnect), function(id) {
      c(get_tags(MapConnect$MapConnect, id)$ProjectName, 
        get_tags(MapConnect$MapConnect, id)$MidPointName)
    }) %>% unlist()
    if (input$MidpointName %in% FileNames) {
      sendSweetAlert(session, "File Name is Already in Use", "Please type a different filename to save the data.", 
                     type = "warning")
      return(NULL)
    }
    
    # Set OmicsData, OmicsStatsPep, and OmicsStats objects 
    OmicsData <- objects$omicsData
    OmicsStatsPep <- objects$peptide_imdanova_res
    OmicsStats <- objects$imdanova_res
    
    browser()
    
    # Determine which tab the exporting is on 
    if (is.null(objects$imdanova_res) == FALSE) {
      Tab <- "Statistics"
    } else if (input$datatype == "pep" & class(objects$omicsData) == "proData") {
      Tab <- "protein_rollup"
    } else if (is.null(objects$peptide_imdanova_res) == FALSE) {
      Tab <- "peptide_statisics"
    } else {
      Tab <- "Normalization"
    }
    
    # Generate pmartRpep midpoint object
    MidPointFile <- list(
      "MidPointFile" = list(
        "App Name" = "pmart app",
        "Data Objects" =
          list(
            "OmicsData" = OmicsData,
            "OmicsStatsPep" = OmicsStatsPep,
            "OmicsStats" =  OmicsStats
          ),
        "Tracking" = 
          list(
            "Timestamp" = Sys.time(),
            "Tab" = Tab,
            "Original Files" = MapConnect$Project 
          )
      )
    )

    # Export results
    UUID <- put_data(MapConnect$MapConnect, MidPointFile)
    
    # Add Tags 
    set_tags(MapConnect$MapConnect, UUID, tags = list(
      "ProjectName" = input$MidpointName,
      "DataType" = MidPointFile$MidPointFile$Tracking$`Original Files`$Project$DataType,
      "ObjectType" = "MidPoint", 
      "App" = "pmart app",
      "Tab" = MidPointFile$MidPointFile$Tracking$Tab
    ))
    
  })

  
)