## Functionality that was added for MAP 
## Last Updated: Feb 8th, 2022

list(
  
  # Read parameters file for URL 
  Parameters <- read.table("./Parameters.txt", header = TRUE),
  
  observeEvent(input$`__startup__`, {
    
    if (isTruthy(Sys.getenv("SHINYTEST_LOAD_MAP_OBJECT"))) {
      query <- list(data = Sys.getenv("SHINYTEST_LOAD_MAP_OBJECT"))
    } else {
      # Parse the query string at the url header
      query <- parseQueryString(session$clientData$url_search)
    }
    
    # Set a conditional test. We only care if the "data" parameter exists. 
    cond <- length(query) != 0 && "data" %in% names(query) 
    
    # If true, open the project data and put each piece where it belongs 
    if (cond) {
      
      # Get the data that was uploaded, and determine whether it is a project object,
      # or a midpoint object.
      pullData <- get_data(MapConnect$MapConnect, query$data)
      
      # If project in the names, then it's a project object
      if (class(pullData) == "project omic") {
        
        # Create a loading screen
        html(
          "loading-gray-overlay", 
          paste("<div class='fadein-out busy relative-centered', style='font-size:xx-large'>", "Loading", 
                 pullData$Project$DataType, "data...</div>")
        )
        
        # Update MAP Connect object (used in upload_UI.R tab)
        project <- pullData
        MapConnect$Project <- project
        
        # Convert from David's project annotations to Daniel's shorthand
        # TODO:  Probably want to change the app/package to use the class names
        # from pmartR
        convertList <-
          list(
            "Peptide" = "pep",
            "Protein" = "pro",
            "Lipidomics" = "lip",
            "Metabolomics" = "metab",
            "RNA-seq" = "seq",
            "Transcriptomics" = "seq" ## Keep for backwards compatibility with old project objects
          )
        
        updateTabsetPanel(session, inputId = "top_page", selected = "upload_data_tab")
        
        # Get the data type
        theDataType <- convertList[lapply(names(convertList), function(name) {
          grepl(name, project$Project$DataType)}) %>% unlist()] %>% unlist()
        
        # Specify file type and disable input
        updatePickerInput(session, "datatype", selected = theDataType)
        disable(id = "datatype")
        
        # If metabolomics, select the appropriate data type and disable choices 
        if (theDataType == "metab") {
          DataType <- pullData$Project$DataType
          if (pullData$Project$DataType == "Metabolomics-NMR") {
            updateRadioGroupButtons(session, "metab_type", selected = "nmr")
          } else {updateRadioGroupButtons(session, "metab_type", selected = "metab")}
        }
        
        # If isobaric, check that this is labeled peptide data 
        if (grepl("Isobaric", project$Project$DataType)) {
          updateRadioGroupButtons(session, "labeled_yn", selected = "iso")
        }
        
      } else if (class(pullData) == "midpoint pmart") {
        
        # If the object isn't a project, then it's a midpoint
        MidPointFile <- pullData
        
        # Create a loading screen
        html(
          "loading-gray-overlay", 
          paste("<div class='fadein-out busy relative-centered', style='font-size:xx-large'>", "Loading midpoint", 
                MidPointFile$Tracking$`Original Files`$Project$DataType, "data...</div>")
        )
        
        # Freeze all pages up to "Tab"
        MapConnect$Midpoint <- MidPointFile
        Tab <- MidPointFile$Tracking$Tab
        
        message(paste("Loading data through", Tab, "tab"))
        
        # These tabs will need to be disabled regardless
        js$disableTab("upload_and_datareqs")
        js$disableTab("group_samples_tab")
        js$disableTab("data_summary_tab")
        js$disableTab("filter_tab")
        js$disableTab("normalization_tab")
        
        # Enable downloads tab
        js$enableTab("download_tab")
        
        # Add additional tabs depending on what kind of data was exported
        if (Tab %in% c("peptide_statistics_tab", "protein_rollup_tab", "statistics_tab")) {
          js$disableTab("peptide_statistics_tab")
        } 
        if (Tab %in% c("protein_rollup_tab", "statistics_tab")) {
          js$disableTab("protein_rollup_tab")
        } 
        if (Tab == "statistics_tab") {
          js$disableTab("statistics_tab")
        }
        
        # Jump to a specific tab if protein data 
        if (class(MidPointFile$`Data Objects`$OmicsData) == "pepData") {
          if (Tab == "normalization_tab") {
            js$enableTab("peptide_statistics_tab")
            js$enableTab("protein_rollup_tab")
            updateTabsetPanel(session, inputId = "top_page", selected = "peptide_statistics_tab")
          } else if (Tab == "peptide_statistics_tab") {
            js$enableTab("protein_rollup_tab")
            updateTabsetPanel(session, inputId = "top_page", selected = "protein_rollup_tab")
          }
        } else {
          updateTabsetPanel(session, inputId = "top_page", selected = "statistics_tab")
        }
        
        # Load omics data 
        objects$omicsData <- MidPointFile$`Data Objects`$OmicsData
        objects$peptide_imdanova_res <-  MidPointFile$`Data Objects`$OmicsStatsPep
        
        if(inherits(objects$omicsData, "seqData")) {
          objects$seqstats_res <-  MidPointFile$`Data Objects`$OmicsStats
        } else {
          objects$imdanova_res <-  MidPointFile$`Data Objects`$OmicsStats 
        }
    
        } else {
        
        sendSweetAlert(session, "Uploaded object type not recognized.", 
                       "Please email david.degnan@pnnl.gov if you see this error.", "error")
        
      }
      
    
    }
    
    # Exit loading screen
    on.exit({
      Sys.sleep(2)
      hide("loading-gray-overlay")
    })
    
  }, priority = -10, ignoreNULL = FALSE, once = TRUE), 
  
  # Set an observer to enable / disable the "Save and Export Progress" button
  observe({
    
    # Disable midpoint button by default
    disable("exportMid")
    is_disabled = T
    
    # If omics data is not NULL, check that "is_normalized" is set to TRUE, OR
    # if we are dealing with a datatype not requiring norm, give it se separate
    # condition
    if (is.null(objects$omicsData) == FALSE) {
      object_is_normed <- attr(objects$omicsData, "data_info")$norm_info$is_normalized
      
      # require statistics results for seqData
      seq_has_stats <- inherits(objects$omicsData, "seqData") & !is.null(objects$seqstats_res)
      
      if (any(seq_has_stats, object_is_normed)) {
        enable("exportMid")
        is_disabled = F
      }
    }
    
    toggleTooltip(session, "js_midpoint", is_disabled, tooltip_text = ttext_[["MIDPOINT_EXPORT_DISABLED"]])
    
  }),
  
  # Observe the export button being clicked, and generate a modal UI to confirm operation
  observeEvent(input$exportMid, {
    showModal(modalDialog(
      fluidPage(
        paste0(
          "This will save results through the Normalization tab and close the app.",
          " Results can be reopened at this point in MAP. Continue?"
        ),
        textInput("MidpointName", "Enter project name:", value = "MySavedData")
      ),
      title = HTML('<p style="text-align: center;">Confirm Save and Export?</p>'),
      footer = list(
        hidden(
          div(
            "Exporting midpoint object...",
            id = "export_mid_busy",
            class = "fadein-out",
            style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
          )
        ),
        actionButton(
          "exportMidConfirm",
          "Export Data",
        ),
        modalButton("Exit")
      ),
      size = "m",
      easyClose = T
    ))
  }),
  
  # If the confirm exit button is clicked, midpoint data is exported to MAP and the app closes. 
  observeEvent(input$exportMidConfirm, {
    req(input$MidpointName)
    show("export_mid_busy")
    on.exit(
      hide("export_mid_busy")
    )
    
    tryCatch({
      # If no omics data, which is possible if the user clicks this button before
      # the app finishes initializing, return NULL
      if (is.null(objects$omicsData)) {removeModal(); return(NULL)}
      
      # Get all current file names 
      FileNames <- lapply(get_all_data_ids(MapConnect$MapConnect), function(id) {
        get_tags(MapConnect$MapConnect, id)$ProjectName
      }) %>% unlist()
      if (isTruthy(input$MidpointName %in% FileNames)) {
        sendSweetAlert(session, "File Name is Already in Use", "Please type a different filename to save the data.", 
                       type = "warning")
        return(NULL)
      }
      
      # Set OmicsData, OmicsStatsPep, and OmicsStats objects 
      OmicsData <- objects$omicsData
      OmicsStatsPep <- objects$peptide_imdanova_res
      OmicsStats <- if(inherits(objects$omicsData, "seqData")) objects$seqstats_res else objects$imdanova_res
  
      # Determine which tab the exporting is on 
      if (!is.null(OmicsStats)) {
        Tab <- "statistics_tab"
      } else if (input$datatype == "pep" & class(objects$omicsData) == "proData") {
        Tab <- "protein_rollup_tab"
      } else if (is.null(objects$peptide_imdanova_res) == FALSE) {
        Tab <- "peptide_statistics_tab"
      } else {
        Tab <- "normalization_tab" # we should never enter this observer if it is before normalization
      }
      
      # Get the project data 
      if (class(MapConnect$Project) != "project omic") {
        project <- MapConnect$Midpoint$Tracking$`Original Files`
      } else {
        project <- MapConnect$Project
      }
      
      # Generate pmartR midpoint object
      Midpoint <- midpoint_pmart(
        omics_data = OmicsData,
        tab = Tab, 
        project = project,
        name = input$MidpointName,
        omics_stats = OmicsStats,
        omics_stats_pep = OmicsStatsPep
      )
      
      # Export results
      UUID <- put_data(MapConnect$MapConnect, Midpoint)
      
      # Add Tags 
      set_tags(MapConnect$MapConnect, UUID, tags = pull_tags_from_object(Midpoint))
    }, error = function(e) {
      shinyalert(
        sprintf("Something went wrong exporting your data: %s \n Try exporting your data manual or contact the developers.", e),
        type = "error"
      )
      return(NULL)
    })
    
    showModal(
      modalDialog(
        title = "Export Success!", 
        div(
          "Navigate back to the", 
          tags$a("MAP home page", href = Parameters[Parameters$Parameter == "MAP_URL", "Local"], target="_blank"), 
          "to use your data in another app."
        )
      )
    )
    
  })

  
)
