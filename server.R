options(shiny.maxRequestSize=250*1024^2, ch.dir = TRUE, DT.TOJSON_ARGS = list(na = 'string')) 

shinyServer(function(session, input, output) {
  # Named list whose values are either references to objects or filepaths for certain resources
  # needs to exist here because session doesnt exist in global.R
  resources_locations <- list('Data File (e_data)' = list('objects', 'omicsData', 'e_data'), 
                              'Sample Info (f_data)' = list('objects', 'omicsData', 'f_data'), 
                              'Biomolecule Information (e_meta)' = list('objects', 'omicsData', 'e_meta'), 
                              'iMd-Anova Table' = list('objects', 'imdanova_res', 'Full_results'),
                              'Trelliscope Displays' = paste0('Trelliscope_', session$token))
  
  # misc reactive values
  revals <- reactiveValues(warnings_upload=list(), warnings_groups = list(), warnings_transform = list(), warnings_normalize = list(),
                           e_meta=NULL, e_meta_2=NULL,
                           cvcol1=NULL, cvcol2=NULL, gcol1=NULL, gcol2=NULL,
                           cvcol1_2=NULL, cvcol2_2=NULL, gcol1_2=NULL, gcol2_2=NULL)
  
  # data objects, other things like filter objects will be stored here
  objects <- reactiveValues(omicsData = NULL, omicsData_2 = NULL, 
                            upload_summary = NULL, groupdes_summary = NULL,
                            upload_summary_2 = NULL, groupdes_summary_2 = NULL)
  
  # store last plot and all plots for download
  plots <- reactiveValues(allplots = list(), plot_table = data.frame('Select a plot' = character(0), 'Download?' = character(0), check.names = F, stringsAsFactors = F))
  
  # tables of results and other things (intentionally have plot_table in plots$... reactive list)
  # +1000 points for variable called tables_table, which is accessed by calling tables$tables_table
  tables <- reactiveValues(tables_table = data.frame('Table' = names(resources_locations), 'Download?' = dt_checkmark, stringsAsFactors = FALSE, check.names = FALSE))
  
  # local file, not tracked by git.  Create one if you would like to perform postmortem debugging
  tryCatch({
    source('store_postmortem_objects.R', local = TRUE)
  }, error = function(e) message('Not storing postmortem objects'))
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
  setBookmarkExclude(c('apply_transform', 'group_designation', 'apply_filters'))
  
  onBookmark(function(state) {
    state$values$revals <- reactiveValuesToList(revals)
    state$values$objects <- reactiveValuesToList(objects)
  })
  
  onRestore(function(state) {
    for(name in names(state$values$revals)){
      revals[[name]] <- state$values$revals[[name]]
    }
    for(name in names(state$values$objects)){
      objects[[name]] <- state$values$objects[[name]]
    }
  })
  #
  
  # handy to store all inputs in a reactive object
  all_inputs <- eventReactive(c(input$top_page, objects$omicsData, objects$omicsData_2),{
    x <- isolate(reactiveValuesToList(input))
    x
  })
  
  # global observers, modal UI elements, helper functions
  source('UI_helper_functions.R', local = TRUE)
  source('helper_functions.R', local = TRUE)
  source('./observers/global_observers.R', local = TRUE)
  source('./UI_elements/plot_modal_UI.R', local = TRUE)
  
  ###### UPLOAD DATA TAB ########
  
  ### Upload reactive values ###
  # e_data/e_data_2:  uploaded abundance data file, and (in the case of lipids) a second data file for pos/neg ionization
    # NOTE: e_meta/e_meta_2 are contained in revals$e_meta and revals$e_meta_2
  # (emeta/edata)_cnames, (emeta/edata)_cnames_2:  column names of the data and metadata files
  # sample_names:  sample names, which are usually just the column names of the e_data file minus the mass ID column
  # f_data_upload:  fake f_data just used so that the call to as.objects$omicsData will run without error
  # two_lipids: logical indicating whether the user has selected lipids AND that they have 2 files.  for coding cleanliness
  
  source('./reactive_variables/upload_revals.R', local = TRUE)
  source('./observers/upload_observers.R', local = TRUE)
  source('./UI_elements/upload_UI.R', local = TRUE)
  
  ###### GROUPS TAB #######
  
  ### Groups reactive values: ###
  # f_data/f_data_2 the tables uploaded by the user for each f_data
  # main_effects/main_effects_2 the possible main effects for each dataset
  # covariates/covariates_2 the possible covariates for each dataset
  
  source('./observers/groups_observers.R', local=TRUE)
  source('./reactive_variables/groups_revals.R', local=TRUE)
  source('./UI_elements/groups_UI.R', local=TRUE)
  
  output$download_fdata<- downloadHandler(
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
  
  ### QC TAB ###
  source('UI_elements/qc_UI.R', local = TRUE)
  source('observers/qc_observers.R', local = TRUE)
  source('reactive_variables/qc_revals.R', local = TRUE)
  
  #### FILTER TAB ####
  source('./observers/filter_observers.R', local = TRUE)
  source('./UI_elements/filter_UI.R', local = TRUE)
  
  ###### NORMALIZATION TAB #######
  
  source('./observers/normalization_observers.R', local = TRUE)
  source('./UI_elements/normalization_UI.R', local = TRUE)
  
  ###### PROTEIN ROLLUP TAB ######
  
  source('./observers/protein_rollup_observers.R', local = TRUE)
  source('./UI_elements/protein_rollup_UI.R', local = TRUE)

  ###### ANALYSIS TAB ########
  
  source('./observers/analysis_observers.R', local = TRUE)
  source('./UI_elements/analysis_UI.R', local = TRUE)
  
  ###### TRELLISCOPE TAB ######
  #source('./observers/trelliscope_observers.R', local = TRUE)
  #source('./UI_elements/trelliscope_UI.R', local = TRUE)
  
  ###### DOWNLOAD TAB ########
  
  source('./observers/download_observers.R', local = TRUE)
  source('./UI_elements/download_UI.R', local = TRUE)
  
  output$download_processed_data <- downloadHandler(
    filename = paste("pmartR_output_",proc.time()[1],".zip", sep = ""),
    content = function(fname){
      
      # this is necessary to zip up the entire trelliscope directory
      # specifically, we dont want to have to use the -j flag to get non-directory files..
      # .. so, we navigate to where everything is (tempdir()) and download with just -r
      orig_wd <- getwd()
      on.exit(setwd(orig_wd))
      setwd(tempdir())
      
      zip(zipfile=fname, files=revals$fs, flags = "-r")
      if (file.exists(paste0(fname,".zip"))){file.rename(paste0(fname,".zip"),fname)}
    },
    contentType = "application/zip"
  )
  
  observeEvent(input$BROWSER, {
    browser()
  })
  
})