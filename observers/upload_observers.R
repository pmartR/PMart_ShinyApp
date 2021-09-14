## For filling omicsData stuff here and for resetting
makeobject <- function(use_iso = T){
  
  req(e_data(), f_data_upload())
  
  ## two lipid inputs
  if (two_lipids()) {
    req(e_data_2(), f_data_upload_2())
    edata2 <- e_data_2()
    emeta2 <- revals$e_meta_2 
    emeta_cname2 <- colnames(emeta2)[1] #### should emeta cname just be the same regardless? this does force the emeta cname
    fdata2 <- f_data_upload_2()
  }
  
  ## inputs
  selection <- input$datatype
  metab_type <- input$metab_type
  pep_type <- input$labeled_yn
  edata <- e_data()
  edata_cname <- input$id_col
  emeta <- revals$e_meta
  emeta_cname <- input$protein_column
  fdata <- f_data_upload()
  data_scale <- input$data_scale
  transform <- input$transform
  norm_info <- as.logical(as.integer(input$normalized_yn))
  na_replace <- input$na_symbol
  
  # store appropriate function call for data type
  fn_list <- list("lip" = "as.lipidData", "pep" = "as.pepData", 
                  "iso" = "as.isobaricpepData", "pro" = "as.proData", 
                  "metab" = "as.metabData", "nmr" = "as.nmrData")
  
  if(selection == "metab") selection <- metab_type
  if(selection == "pep" && use_iso) selection <- pep_type
  
  object_fn <- get(fn_list[[selection]])
  revals$warnings_upload <- NULL
  
  # create first object
  objects$uploaded_omicsData <- objects$omicsData <- tryCatch(
    {
      object_fn(
        e_data = edata, e_meta = emeta, f_data = fdata,
        edata_cname = edata_cname, emeta_cname = emeta_cname, fdata_cname = "SampleId",
        data_scale = data_scale, norm_info = list(is_normalized = norm_info)
      ) %>%
        edata_replace(na_replace, NA)
    },
    error = function(e) {
      msg <- paste0("Something went wrong processing your omicsData object \n System error:  ", e)
      revals$warnings_upload$badupload <- sprintf("<p style = color:red>%s</p>", msg)
      NULL
    }
  )
  
  # create second object for lipid data
  if (two_lipids()) {
    objects$uploaded_omicsData_2 <- objects$omicsData_2 <- tryCatch(
      {
        object_fn(
          e_data = edata2, e_meta = emeta2, f_data = fdata2,
          edata_cname = edata_cname, emeta_cname = emeta_cname2,
          fdata_cname = "SampleId",
          data_scale = data_scale, norm_info = list(is_normalized = norm_info)
        ) %>%
          edata_replace(na_replace, NA)
      },
      error = function(e) {
        msg <- paste0("Something went wrong processing the second object \n System error:  ", e)
        revals$warnings_upload$badupload_2 <<- sprintf("<p style = color:red>%s</p>", msg)
        NULL
      }
    )
  }
  else {
    objects$omicsData_2 <- objects$uploaded_omicsData_2 <- NULL
  }
  
  # transform data objects
  if (!is.null(objects$uploaded_omicsData) & transform != "Select one") {
    if (attr(objects$uploaded_omicsData, "data_info")$data_scale != transform) {
      objects$omicsData <- objects$uploaded_omicsData <- tryCatch(
        {
          edata_transform(objects$uploaded_omicsData, data_scale = transform)
        },
        error = function(e) {
          msg <- paste0("Something went wrong processing your omicsData object \n System error:  ", e)
          revals$warnings_upload$bad_transform <<- sprintf("<p style = color:red>%s</p>", msg)
          NULL
        }
      )
    }
  }
  
  if (!is.null(objects$uploaded_omicsData_2) & transform != "Select one") {
    if (attr(objects$uploaded_omicsData_2, "data_info")$data_scale != transform) {
      objects$omicsData_2 <- objects$uploaded_omicsData_2 <- tryCatch(
        {
          edata_transform(objects$uploaded_omicsData_2, data_scale = transform)
        },
        error = function(e) {
          msg <- paste0("Something went wrong processing the second object \n System error:  ", e)
          revals$warnings_upload$bad_transform_2 <<- sprintf("<p style = color:red>%s</p>", msg)
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
  if (two_lipids()) {
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

# disable if they try to log transform data with zeros
observe({
  isolate(revals$warnings_upload$bad_transform <- NULL)
  
  cond <- e_data_has_zeros() & 
    input$na_symbol != "0" &
    grepl("^log", input$transform) & 
    !grepl("^log", input$data_scale)
  
  isolate(
    revals$warnings_upload$bad_transform <- if(isTRUE(cond)) {
      sprintf("<div style = 'color:red'>%s</div>",
        sprintf(
          infotext_[["LOG_TRANSFORM_ZEROS"]], 
          ifelse(trimws(input$na_symbol) == "", "-no selection-", input$na_symbol)
        )
      )
    } else NULL
  )
  
  toggleState("makeobject", condition = !cond)
})

# when they select e_meta files, check that all columns are in order and close the collapsebar if everything looks ok
observe({
  # 2 file lipid conditions:
  # two emeta files uploaded
  # all 4 files contain the id column
  if (two_lipids()) {
    req(!is.null(input$id_col) & !is.null(input$id_col_2))
    cond_files <- (!is.null(input$file_emeta) & !is.null(input$file_emeta_2))
    cond_idcol_edata <- all(sapply(
      list(e_data(), e_data_2()),
      function(df) {
        isTRUE(input$id_col %in% colnames(df))
      }
    ))
    cond_idcol_emeta <- all(sapply(
      list(revals$e_meta, revals$e_meta_2),
      function(df) {
        isTRUE(input$id_col %in% colnames(df)) | is.null(df)
      }
    )) | !isTruthy(input$emeta_yn)
    cond_nasymbol <- !is.null(input$na_symbol)
    cond_shared_ids <- all(e_data()[[input$id_col]] %in% revals$e_meta[[input$id_col]]) & all(e_data_2()[[input$id_col_2]] %in% revals$e_meta_2[[input$id_col_2]])
    cond <- all(cond_idcol_edata, cond_idcol_emeta, cond_nasymbol, cond_shared_ids) | isTruthy(!as.logical(input$emeta_yn))
  }
  # peptide with protein id col conditions:
  # emeta is uploaded
  # protein column is in the columns of e_meta that are NOT the id column
  # both files contain id column
  else if (input$datatype == "pep" & isTRUE(input$proteins_yn == "TRUE")) {
    req(!is.null(input$id_col))
    # req(!is.null(input$protein_column)) ## Doesn't load if cond_shared_ids is false
    cond_files <- !is.null(input$file_emeta)
    cond_procol <- !is.null(input$protein_column) && 
      input$protein_column %in% colnames(revals$e_meta)[-which(colnames(revals$e_meta) == input$id_col)]
    cond_idcol_edata <- isTRUE(input$id_col %in% colnames(e_data()))
    cond_idcol_emeta <- isTRUE(input$id_col %in% colnames(revals$e_meta))
    cond_nasymbol <- !is.null(input$na_symbol)
    cond_shared_ids <- if (!is.null(revals$e_meta)) all(e_data()[[input$id_col]] %in% revals$e_meta[[input$id_col]]) else TRUE
    cond <- all(cond_files, cond_procol, cond_idcol_edata, cond_idcol_emeta, cond_nasymbol, cond_shared_ids)
  }
  # anything else conditions:
  # emeta is uploaded
  # both files contain id column
  else {
    req(!is.null(input$id_col))
    cond_files <- !is.null(input$file_emeta)
    cond_idcol_edata <- isTRUE(input$id_col %in% colnames(e_data()))
    cond_idcol_emeta <- isTRUE(input$id_col %in% colnames(revals$e_meta)) | is.null(revals$e_meta) | !isTruthy(input$emeta_yn)
    cond_nasymbol <- !is.null(input$na_symbol)
    cond_shared_ids <- all(e_data()[[input$id_col]] %in% revals$e_meta[[input$id_col]]) 
    cond <- all(cond_idcol_edata, cond_idcol_emeta, cond_nasymbol, cond_shared_ids) | isTruthy(!as.logical(input$emeta_yn))
  }

  # if all conditions met, wait a moment, close the panel
  if (cond) {
    Sys.sleep(0.6)
    updateCollapse(session, "upload_collapse_left", close = "meta_collapse")
  }

  # toggle css and display warnings
  toggleCssClass("js_id_col", "error-textcolor", condition = any(!cond_idcol_edata, !cond_idcol_emeta, !cond_shared_ids) & cond_files)
  revals$warnings_upload$bad_identifier <- if (any(!cond_idcol_edata, !cond_idcol_emeta, !cond_shared_ids) & cond_files & isTruthy(as.logical(input$emeta_yn))) "<p style = 'color:red'>One or more of your identifier columns are not found in or have inconsistent values across the e_data and e_meta files.</p>" else NULL

  # condition debugger
  # print(c(cond_files, cond_idcol_edata, cond_idcol_emeta, cond_nasymbol, cond_shared_ids))

  toggle("ok_metadata", condition = cond)
  toggleState("makeobject", condition = cond)
})

# make data and display success message on successful objects$omicsData object creation
observeEvent(input$makeobject, {
  
  makeobject(use_iso = F)

  # store warning message if data did not successfully create
  if(is.null(objects$omicsData) | (two_lipids() & is.null(objects$omicsData_2))){
    revals$warnings_upload$failed_object <- "<p style = 'color:grey'>Something went wrong processing your objects$omicsData object(s), please verify all fields are correct.</p>"
  } else revals$warnings_upload$failed_object <- NULL
  
  cond_one_obj <- !two_lipids() & !is.null(objects$omicsData)
  cond_two_obj <- two_lipids() & !is.null(objects$omicsData_2)
  
  if (cond_one_obj | cond_two_obj) {
    
    # if(inherits(objects$omicsData, "nmrData") || 
    #    (inherits(objects$omicsData, "pepData") && 
    #     input$labeled_yn == "iso")){
    #   usebutton <- actionButton("goto_reference", "Continue to Reference Tab", style = "margin:5px;width:75%")
    # } else {
    #   usebutton <- actionButton("goto_qc", "Continue to Groups Tab", style = "margin:5px;width:75%")
    # }
    
    showModal(
      modalDialog(
        title = "Upload Success",
        fluidRow(
          column(10,
            align = "center", offset = 1,
            HTML('<h4 style= "color:#1A5276">Your data has been successfully reference normalized. 
                      Future processing will be performaed on the reference normalized data.</h4>'),
            hr(),
            actionButton("upload_dismiss", "Review results", width = "75%"),
            actionButton("goto_groups", "Continue to Groups Tab", style = "margin:5px;width:75%")
          )
        ),
        footer = NULL
      )
    )

    updateCollapse(session, "upload_preview_collapse", open = "summary_boxplots")
    revals$upload_summary <- summary(objects$omicsData)
    revals$upload_summary_2 <- if (two_lipids()) summary(objects$omicsData_2) else NULL
  }
  else {
    revals$upload_summary <- NULL
    revals$upload_summary_2 <- NULL
  }
})

# toggle dataset view selection depending on whether 2 datasets are being uploaded (lipid data)
observe({
  cond_file2exists <- !is.null(e_data_2()) | !is.null(revals$e_meta_2)

  toggleElement("toggle_table", condition = two_lipids() & cond_file2exists)
})

observe({
  cond_fdata2exists <- isTRUE(nrow(f_data_2()) > 0)

  toggleElement("toggle_fdata", condition = two_lipids() & cond_fdata2exists)
})

## store null values in e_meta if no file chosen since it is not required to make object
observe({
  if(!isTruthy(as.logical(input$emeta_yn)) & input$datatype != "pep") {
    revals$e_meta <- NULL
    shinyjs::reset("file_emeta")
  }
  else if (is.null(input$file_emeta$datapath)) {
    revals$e_meta <- NULL
  }
  else {
    filename <- input$file_emeta$datapath
    revals$e_meta <- read.csv(filename, stringsAsFactors = FALSE, check.names = F)
  }
})

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

# modal dialog behavior
observeEvent(input$upload_dismiss, {
  removeModal()
})

observeEvent(input$goto_groups, {
  updateTabsetPanel(session, "top_page", selected = "Group Samples")
  removeModal()
})

observeEvent(input$goto_reference, {
  updateTabsetPanel(session, "top_page", selected = "Reference")
  removeModal()
})
#
