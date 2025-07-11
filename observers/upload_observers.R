## For filling omicsData stuff here and for resetting
makeobject <- function(use_iso = T){
  
  req(e_data(), f_data_upload())
  
  ## two lipid inputs
  if (two_lipids() || two_metab()) {
    req(e_data_2(), f_data_upload_2())
    edata2 <- e_data_2()
    edata_cname2 <- input$id_col_2
    emeta2 <- revals$e_meta_2 
    emeta_cname2 <- input$id_col_2
    fdata2 <- f_data_upload_2()
  }
  
  ## inputs
  selection <- input$datatype
  
  pep_type <- input$labeled_yn
  edata <- e_data()
  edata_cname <- input$id_col
  emeta <- revals$e_meta
  emeta_cname <- if(!is.null(input$protein_column)) {
    input$protein_column
  } else {
    input$id_col
  }
  fdata <- f_data_upload()
  data_scale <- input$data_scale
  transform <- input$transform
  is_normalized <- as.logical(as.integer(input$normalized_yn))
  if(!isTruthy(is_normalized)) is_normalized <- FALSE
  
  na_replace <- input$na_symbol
  
  # store appropriate function call for data type
  fn_list <- list("lip" = "as.lipidData", "pep" = "as.pepData", 
                  "iso" = "as.isobaricpepData", "pro" = "as.proData", 
                  "metab" = "as.metabData", "nmr" = "as.nmrData",
                  "seq" = "as.seqData")
  
  if(selection == "metab") {
    req(input$metab_type)
    
    metab_type <- switch(
      input$metab_type,
      metab1 = "metab",
      metab2 = "metab",
      nmr = "nmr"
    )
    selection <- metab_type
  }
  
  if(selection == "pep" && use_iso) selection <- pep_type
  
  object_fn <- get(fn_list[[selection]])
  revals$warnings_upload <- NULL
  
  # create first object
  objects$uploaded_omicsData <- objects$omicsData <- tryCatch(
    {
      res <- object_fn(
        e_data = edata, e_meta = emeta, f_data = fdata,
        edata_cname = edata_cname, emeta_cname = emeta_cname, fdata_cname = "SampleId",
        data_scale = data_scale, is_normalized = is_normalized
      )
      
      if(selection != "seq") res <- res %>% edata_replace(na_replace, NA)
      res
    },
    error = function(e) {
      msg <- paste0("Something went wrong processing your omicsData object <br> System error:  ", e)
      revals$warnings_upload$badupload <- messageBox(type = "error", msg)
      NULL
    }
  )
  
  # create second object for lipid data
  if (two_lipids() || two_metab()) {
    objects$uploaded_omicsData_2 <- objects$omicsData_2 <- tryCatch(
      {
        object_fn(
          e_data = edata2, e_meta = emeta2, f_data = fdata2,
          edata_cname = edata_cname2, emeta_cname = emeta_cname2,
          fdata_cname = "SampleId",
          data_scale = data_scale, is_normalized = is_normalized
        ) %>%
          edata_replace(na_replace, NA)
      },
      error = function(e) {
        msg <- paste0("Something went wrong processing the second object <br> System error:  ", e)
        revals$warnings_upload$badupload_2 <<- messageBox(type = "error", msg)
        NULL
      }
    )
  }
  else {
    objects$omicsData_2 <- objects$uploaded_omicsData_2 <- NULL
  }
  
  # transform data objects
  if (!is.null(objects$uploaded_omicsData) && 
      length(transform) > 0 &&
      transform != "none") {
    if (attr(objects$uploaded_omicsData, "data_info")$data_scale != transform) {
      objects$omicsData <- objects$uploaded_omicsData <- tryCatch(
        {
          edata_transform(objects$uploaded_omicsData, data_scale = transform)
        },
        error = function(e) {
          msg <- paste0("Something went wrong processing your omicsData object <br> System error:  ", e)
          revals$warnings_upload$bad_transform <<- messageBox(type = "error", msg)
          NULL
        }
      )
    }
  }
  
  if (!is.null(objects$uploaded_omicsData_2) &&
      length(transform) > 0 && 
      transform != "none") {
    if (attr(objects$uploaded_omicsData_2, "data_info")$data_scale != transform) {
      objects$omicsData_2 <- objects$uploaded_omicsData_2 <- tryCatch(
        {
          edata_transform(objects$uploaded_omicsData_2, data_scale = transform)
        },
        error = function(e) {
          msg <- paste0("Something went wrong processing the second object <br> System error:  ", e)
          revals$warnings_upload$bad_transform_2 <<- messageBox(type = "error", msg)
          NULL
        }
      )
    }
  }
}

# highlight dropdown and hide file uploads if they havent specified a data type
observeEvent(input$datatype, {
  cond <- (input$datatype != "none")
  toggle("edata_UI_parent", condition = cond)
  toggle("emeta_UI_parent", condition = cond)
  toggleClass(id = "js_datatype", class = "attention", condition = !cond)
})

# close edata upload panel and open column id panel when they successfully upload data
observeEvent(c(input$file_edata, input$file_edata_2), {
  Sys.sleep(0.7)
  if (two_lipids() || two_metab()) {
    cond <- !is.null(input$file_edata) & !is.null(input$file_edata_2)
  }
  else {
    cond <- !is.null(input$file_edata)
  }

  if (cond) {
    updateCollapse(session, "upload_collapse_left", open = c("columnids"), close = "datselect")
  }
  else {
    updateCollapse(session, "upload_collapse_left", close = c("columnids"))
  }

  toggle("js_id_col", condition = cond)
  toggle("js_datascale", condition = cond)
  toggle("js_normalized_yn", condition = cond)
  toggle("js_na_symbol", condition = cond)
  toggle("js_emeta_UI", condition = cond)
  toggle("ok_datselect", condition = cond)
})

# user manually specifies they are done selecting columns, closes the collapsible bar
observeEvent(input$done_idcols, {
  updateCollapse(session, "upload_collapse_left", close = "columnids", open = "meta_collapse")
  shinyjs::show("ok_columnids")
})

#'@details disable if they try to log transform data with zeros.  Store an
#'indicator that is TRUE if all is well.
observe({
  req(!is.null(e_data_has_zeros()))
  isolate(revals$warnings_upload$bad_transform <- NULL)
  
  cond <- e_data_has_zeros() & 
    input$na_symbol != "0" &
    grepl("^log", input$transform) & 
    !grepl("^log", input$data_scale)
  
  isolate(
    revals$warnings_upload$bad_transform <- if(isTRUE(cond)) {
      messageBox(
        sprintf(
          infotext_[["LOG_TRANSFORM_ZEROS"]], 
          "<b>",
          ifelse(trimws(input$na_symbol) == "", "-no selection-", input$na_symbol),
          "</b>"
        ),
        type = "error"
      )
    } else NULL
  )
  
  revals$boolean$upload$log_zeros <- !cond
})

#'@details Check that everything in the biomolecule information section is ok.
#' If it is, then collapse the sidebar, show a checkmark, and activate the create
#' omicsData button.
observe({
  # 2 file lipid conditions:
  # two emeta files uploaded
  # all 4 files contain the id column
  
  if (two_lipids() || two_metab()) {
    req(!is.null(input$id_col) & !is.null(input$id_col_2))
    cond_files <- (!is.null(input$file_emeta) & !is.null(input$file_emeta_2))
    cond_idcol_edata <- all(
      input$id_col %in% colnames(e_data()),
      input$id_col_2 %in% colnames(e_data_2())
    )
    cond_idcol_emeta <- all(
      isTRUE(input$id_col %in% colnames(revals$e_meta)) | is.null(revals$e_meta),
      isTRUE(input$id_col %in% colnames(revals$e_meta_2)) | is.null(revals$e_meta_2)
    ) | !isTruthy(input$emeta_yn)
    cond_nasymbol <- !is.null(input$na_symbol)
    cond_shared_ids <-
      all(e_data()[[input$id_col]] %in% revals$e_meta[[input$id_col]]) &
      all(e_data_2()[[input$id_col_2]] %in% revals$e_meta_2[[input$id_col_2]])
    cond <-
      all(cond_idcol_edata,
          cond_idcol_emeta,
          cond_nasymbol,
          cond_shared_ids) | isTruthy(!as.logical(input$emeta_yn))
  }
  # peptide with protein id col conditions:
  # emeta is uploaded
  # protein column is in the columns of e_meta that are NOT the id column
  # both files contain id column
  else if (!is.null(input$datatype) && !is.null(input$proteins_yn) && 
           input$datatype == "pep" && isTRUE(input$proteins_yn == "TRUE")) {
    req(!is.null(input$id_col))
    # req(!is.null(input$protein_column)) ## Doesn't load if cond_shared_ids is false
    cond_files <- !is.null(input$file_emeta)
    cond_procol <- !is.null(input$protein_column) && 
      input$protein_column %in% colnames(revals$e_meta)[-which(colnames(revals$e_meta) == input$id_col)]
    cond_idcol_edata <- isTRUE(input$id_col %in% colnames(e_data()))
    cond_idcol_emeta <- isTRUE(input$id_col %in% colnames(revals$e_meta)) | is.null(revals$e_meta)
    cond_nasymbol <- !is.null(input$na_symbol)
    cond_shared_ids <- if (!is.null(revals$e_meta)) all(e_data()[[input$id_col]] %in% revals$e_meta[[input$id_col]]) else TRUE
    cond_emeta <- all(cond_files, cond_shared_ids, cond_idcol_emeta, cond_procol) | isTruthy(!as.logical(input$emeta_yn))
    
    cond <- all(cond_idcol_edata, cond_nasymbol, cond_emeta)
  }
  # anything else conditions:
  # emeta is uploaded
  # both files contain id column
  else {
    req(!is.null(input$id_col))
    cond_files <- !is.null(input$file_emeta)
    cond_idcol_edata <- isTRUE(input$id_col %in% colnames(e_data()))
    cond_idcol_emeta <- isTRUE(input$id_col %in% colnames(revals$e_meta)) | is.null(revals$e_meta)
    cond_nasymbol <- !is.null(input$na_symbol)
    cond_shared_ids <- all(e_data()[[input$id_col]] %in% revals$e_meta[[input$id_col]]) | (MAP_ACTIVE & is.null(revals$e_meta))
    cond_emeta <- all(cond_files, cond_shared_ids, cond_idcol_emeta) | isTruthy(!as.logical(input$emeta_yn))
    
    cond <- all(cond_idcol_edata, cond_nasymbol, cond_emeta)
  }

  # toggle css and display warnings
  toggleCssClass("js_id_col", "error-textcolor", condition = any(!cond_idcol_edata, !cond_idcol_emeta, !cond_shared_ids) & cond_files)
  revals$warnings_upload$bad_identifier <-
    if (any(!cond_idcol_edata,!cond_idcol_emeta,!cond_shared_ids) &
        cond_files &
        isTruthy(as.logical(input$emeta_yn))) {
      messageBox(type = "error", "One or more of your identifier columns are not found in or have inconsistent values across the e_data and e_meta files.")
    } else NULL
  
  toggle("ok_metadata", condition = cond)
  
  revals$boolean$upload$emeta_ok <- cond
})

#'@details Collect all values that indicate all required inputs are correct and
#'toggle the button to make the object on/off depending.
observe({
  revals$boolean$upload
  #' TODO:  Add info messages as to why things were disabled.  (or rather, check
  #' that every info message that appears in red below the button is a good 
  #' reason for the button to be disabled)
  toggleState("makeobject",
              condition = all(unlist(revals$boolean$upload)) & 
                length(unlist(revals$boolean$upload)) > 0)
})

#'@details Disable create object if the sample names of the two edatas are not 
#'the same for two lipids 
#'TODO:  Make this a reactive
observe({
  isolate({
    #revals$boolean$upload$lipids_sampnames_setequal <- NULL
    #revals$warnings_upload$samps_not_equal <- NULL
    
    revals$boolean$upload$lipids_ids_unique <- NULL
    revals$warnings_upload$edata_not_unique <- NULL
  })
  
  req(two_lipids() || two_metab())
  req(!is.null(input$id_col) & !is.null(input$id_col_2))
  
  # if(!lipids_samps_eq()) {
  #   isolate({
  #     revals$warnings_upload$samps_not_equal <- "<p style = 'color:red'>Your data files for both lipids must have the same column names.</p>"
  #   })
  # }
  # isolate({
  #   revals$boolean$upload$lipids_sampnames_setequal <- lipids_samps_eq()
  # })
  
  if(!omics_edata_unq()) {
    isolate({
      revals$warnings_upload$edata_not_unique <- messageBox(type = "error", "There were duplicate entries in the identifier columns of your lipid datasets.")
    })
  }
  isolate({
    revals$boolean$upload$lipids_ids_unique <- omics_edata_unq()
  })
})

# make data and display success message on successful objects$omicsData object creation
observeEvent(input$makeobject, {

  makeobject(use_iso = F)

  # store warning message if data did not successfully create
  if(is.null(objects$omicsData) | ((two_lipids() | two_metab()) & is.null(objects$omicsData_2))){
    revals$warnings_upload$failed_object <- messageBox(type = "warning", "Something went wrong processing your objects$omicsData object(s), please verify all fields are correct")
  } else revals$warnings_upload$failed_object <- NULL
  
  cond_one_obj <- (!two_lipids() && !two_metab()) & !is.null(objects$omicsData)
  cond_two_obj <- (two_lipids() || two_metab()) & !is.null(objects$omicsData_2)
  
  if (cond_one_obj | cond_two_obj) { 

    is_logtrf  <- isTRUE(
      grepl("^log", attributes(objects$omicsData)$data_info$data_scale)
    )

    nolog_warn  <- if(!is_logtrf & !inherits(objects$omicsData, "seqData")) {
      div(class = "warning-msg", infotext_[["DATA_NOT_LOG"]])
    } else NULL

    showModal(
      modalDialog(
        title = "Upload Success",
        fluidRow(
          column(10,
            align = "center", offset = 1,
            nolog_warn,
            HTML('<h4 style= "color:#1A5276">Your data has been successfully uploaded! 
                      Future processing will be performed on the uploaded data.</h4>'),
            hr(),
            actionButton("upload_dismiss", "Review results", style = "margin:5px;width:75%"),
            br(),
            actionButton("goto_groups", "Continue to Group Samples tab", style = "margin:5px;width:75%")
          )
        ),
        footer = NULL
      )
    )

    updateCollapse(session, "upload_preview_collapse", open = "summary_boxplots")
    updateCollapse(session, "upload_collapse_left", close = c("columnids", "meta_collapse"))
    revals$upload_summary <- summary(objects$omicsData)
    revals$upload_summary_2 <- if (two_lipids() || two_metab()) summary(objects$omicsData_2) else NULL
  }
  else {
    revals$upload_summary <- NULL
    revals$upload_summary_2 <- NULL
  }
})

# toggle dataset view selection depending on whether 2 datasets are being uploaded (lipid data)
observe({
  cond_file2exists <- !is.null(e_data_2()) | !is.null(revals$e_meta_2)

  toggleElement("toggle_table", condition = (two_lipids() || two_metab()) & cond_file2exists)
})

#'@details store emeta info in an intermediate container that can be NULLED
observeEvent(input$file_emeta, {
  revals$e_meta_info <- input$file_emeta
}, priority = 10)


if (MAP_ACTIVE) {
  observe({
    Sys.sleep(3)
    if (is.null(MapConnect$Project) == FALSE) {
      revals$e_meta <- MapConnect$Project$Data$e_meta
    }
  })
}


if (MAP_ACTIVE == FALSE) {
  ## store null values in e_meta if no file chosen since it is not required to make object
  observe({
    if(!isTruthy(as.logical(input$emeta_yn))) {
      revals$e_meta <- NULL
      revals$e_meta_info <- NULL
      shinyjs::reset("file_emeta")
    }
    else if (is.null(revals$e_meta_info$datapath)) {
      revals$e_meta <- NULL
    }
    else {
      filename <- revals$e_meta_info$datapath
      revals$e_meta <- read.csv(filename, stringsAsFactors = FALSE, check.names = F)
    }
  })
}

observe({
  if(!isTruthy(as.logical(input$emeta_yn))) {
    revals$e_meta_2 <- NULL
    shinyjs::reset("file_emeta_2")
  }
  else if (is.null(input$file_emeta_2$datapath)) {
    revals$e_meta_2 <- NULL
  }
  else {
    filename <- input$file_emeta_2$datapath
    revals$e_meta_2 <- read.csv(filename, stringsAsFactors = FALSE, check.names = F)
  }
})
##

#'@details navigate to the data requirements sub-tab
observeEvent(input$upload_to_datareqs, {
  updateCollapse(session, "upload_preview_collapse", open = "data_requirements")
})

# modal dialog behavior
observeEvent(input$upload_dismiss, {
  removeModal()
})

observeEvent(input$goto_groups, {
  updateTabsetPanel(session, "top_page", selected = "group_samples_tab")
  removeModal()
})

observeEvent(input$goto_reference, {
  updateTabsetPanel(session, "top_page", selected = "reference_tab")
  removeModal()
})
#
