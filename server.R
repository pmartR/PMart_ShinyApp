options(shiny.maxRequestSize = 250 * 1024^2, ch.dir = TRUE, DT.TOJSON_ARGS = list(na = "string"))

# internal function: throws error message if variable is not found and can_be_empty==FALSE
get_config_variable <- function(cfg, varname, can_be_empty=FALSE) {
  if (!(varname %in% names(cfg))) {
    if (!can_be_empty)
      stop(sprintf("Cannot find '%s' in config file", varname))
    else
      return('')
  }
  value <- cfg[[varname]]
  return(value)
}

shinyServer(function(session, input, output) {
  # Named list whose values are either references to objects or filepaths for certain resources
  # needs to exist here because session doesnt exist in global.R
  resources_locations <- list(
    "Expression Data (e_data)" = list("objects", "omicsData", "e_data"),
    "Sample Information (f_data)" = list("objects", "omicsData", "f_data"),
    "Biomolecule Information (e_meta)" = list("objects", "omicsData", "e_meta"),
    "Statistics" = list("objects", "imdanova_res"),
    "SeqData Statistics" = list("objects", "seqstats_res")
  )
  
  resources_locations_peprollup <- list(
    "Protein Expression Data (e_data)" = list("objects", "omicsData", "e_data"),
    "Peptide Expression Data (e_data)" = list("objects", "omicsData_pre_rollup", "e_data"),
    "Sample Information (f_data)" = list("objects", "omicsData", "f_data"),
    "Biomolecule Information (e_meta)" = list("objects", "omicsData", "e_meta"),
    "Protein Statistics" = list("objects", "imdanova_res"),
    "Peptide Statistics" = list("objects", "peptide_imdanova_res")
  )
  
  # misc reactive values
  revals <- reactiveValues(
    warnings_upload = list(), warnings_groups = list(), warnings_transform = list(), warnings_normalize = list(),
    e_meta = NULL, e_meta_2 = NULL, emeta_info = NULL,
    cvcol1 = NULL, cvcol2 = NULL, gcol1 = NULL, gcol2 = NULL,
    cvcol1_2 = NULL, cvcol2_2 = NULL, gcol1_2 = NULL, gcol2_2 = NULL
  )

  # data objects, other things like filter objects will be stored here
  objects <- reactiveValues(
    omicsData = NULL, omicsData_2 = NULL,
    upload_summary = NULL, groupdes_summary = NULL,
    upload_summary_2 = NULL, groupdes_summary_2 = NULL
  )

  # store last plot and all plots for download
  plots <- reactiveValues(allplots = list(), plot_save_options = list(), plot_table = data.frame("Select a plot" = character(0), "Download?" = character(0), check.names = F, stringsAsFactors = F))

  # tables of results and other things (intentionally have plot_table in plots$... reactive list)
  # +1000 points for variable called tables_table, which is accessed by calling tables$tables_table
  tables <- reactiveValues(tables_table = data.frame("Table" = names(resources_locations), 
                                                     "Download?" = dt_checkmark, 
                                                     stringsAsFactors = FALSE, 
                                                     check.names = FALSE),
                           revenge_of_tables_table = data.frame(
                             "Table" = names(resources_locations_peprollup), 
                             "Download?" = dt_checkmark, 
                             stringsAsFactors = FALSE,
                             check.names = FALSE)
                           
                           )

  # local file, not tracked by git.  Create one if you would like to perform postmortem debugging
  tryCatch(
    {
      source("store_postmortem_objects.R", local = TRUE)
    },
    error = function(e) message("Not storing postmortem objects")
  )
  #
  # EXAMPLE store_postmortem_objects.R:
  #
  # observeEvent(c(objects$omicsData, objects$omicsData_2),{
  #   omicsData_postmortem <<- objects$omicsData
  #   omicsData_2_postmortem <<- objects$omicsData_2
  # })
  #
  # # postmortem debugging for plots
  # observeEvent(plots$allplots,{
  #   if(length(plots$allplots) > 0){
  #     plots_postmortem <<- reactiveValuesToList(plots)
  #   }
  # })
  #
  # # postmortem reactive value debugging
  # observeEvent(reactiveValuesToList(objects),{
  #   objects_postmortem <<- reactiveValuesToList(objects)
  # })

  # save and restore reactive values on bookmark
  setBookmarkExclude(c("apply_transform", "group_designation", "apply_filters"))

  onBookmark(function(state) {
    state$values$revals <- reactiveValuesToList(revals)
    state$values$objects <- reactiveValuesToList(objects)
  })

  onRestore(function(state) {
    for (name in names(state$values$revals)) {
      revals[[name]] <- state$values$revals[[name]]
    }
    for (name in names(state$values$objects)) {
      objects[[name]] <- state$values$objects[[name]]
    }
  })
  #

  # handy to store all inputs in a reactive object
  all_inputs <- eventReactive(c(input$top_page, objects$omicsData, objects$omicsData_2), {
    x <- isolate(reactiveValuesToList(input))
    x
  })

  # Sys.setenv("SHINY_DEBUG" = 1) to get a developer button
  output$developer_buttons <- renderUI({

   if (Sys.getenv("SHINY_DEBUG") == 1) {
      div(
        style = "position:absolute;z-index:9999;bottom:10px;left:10px;",
        actionButton("Browser", "whats wrong!?!?", style = "background:deepskyblue")
      )
    }
    else {
       return(NULL)
    }
  })

  observeEvent(input$Browser, {
    browser()
  })

  # source all UI elements
  for (res_folder in c("reactive_variables", "observers", "UI_elements")) {
    for (f in Sys.glob(sprintf("./%s/*.R", res_folder))) {
      source(f, local = TRUE)
    }
  }

  #
  output$download_fdata <- downloadHandler(
    filename = "f_data_template.csv",
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(fname) {
      req(sample_names())

      temp_fdata <- data.frame(Sample_IDS = sample_names(), Group_1 = rep("Group", length(sample_names())))
      print(temp_fdata)

      write.csv(temp_fdata, row.names = FALSE, file = fname)
    },
    contentType = "text/csv"
  )

  output$download_processed_data <- downloadHandler(
    filename = paste("pmartR_output_", proc.time()[1], ".zip", sep = ""),
    content = function(fname) {

      # this is necessary to zip up nested directories
      # specifically, we dont want to have to use the -j flag to get non-directory files..
      # .. so, we navigate to where everything is (tempdir()) and download with just -r
      orig_wd <- getwd()
      on.exit(setwd(orig_wd))
      setwd(tempdir())
      
      zip(zipfile = fname, files = revals$fs, flags = "-r")
      if (file.exists(paste0(fname, ".zip"))) {
        file.rename(paste0(fname, ".zip"), fname)
      }
    },
    contentType = "application/zip"
  )
  
  cfg_path = if(isTruthy(Sys.getenv("MAP_CONFIG"))) Sys.getenv("MAP_CONFIG") else "./cfg/minio_config.yml"
  
  if (MAP_ACTIVE) {
    
    # Connect to map data access library
    library(mapDataAccess)
    
    # Soure MAP-specific functionality (reading from header, etc)
    source("./MAP_Functions.R", local = TRUE)
    
    # If we are testing, use the MAP_* environment variables
    if (Sys.getenv("MAP_SHINYTEST") == "1") {
      cfg_path <- NA
    }
      
    # Create a reactive value to hold MAP-specific objects
    MapConnect <- reactiveValues(MapConnect = map_data_connection(cfg_path),
                                 Project = NULL, Midpoint = NULL)
    
  } else {
    hide(id = "loading-gray-overlay")
    
    cfg <- yaml::read_yaml(cfg_path)
    
    python_venv <- get_config_variable(cfg, "python_venv")
    
    conda_envs <- tryCatch(
      {
        reticulate::conda_list()$python
      },
      error = function(e) {
        NULL
      }
    )
    
    is_conda <- any(sapply(conda_envs, function(envpath) {
      grepl(sprintf("^%s", normalizePath(python_venv)), envpath)
    }))
    
    if (is_conda) {
      reticulate::use_condaenv(python_venv, required = TRUE)
    } else {
      reticulate::use_virtualenv(python_venv, required = TRUE)
    }
  }
  
})
