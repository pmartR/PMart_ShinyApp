## Reference observers
success_modal <-
    modalDialog(
      title = "Reference Normalization Success",
      fluidRow(
        column(10,
               align = "center", offset = 1,
               HTML('<h4 style= "color:#1A5276">Your data has been successfully reference normalized. 
                      Future processing will be performed on the reference normalized data.</h4>'),
               hr(),
               actionButton("upload_dismiss", "Review results", width = "75%"),
               actionButton("goto_filter", "Continue to Filter tab", style = "margin:5px;width:75%")
        )
      ),
      footer = NULL
    )
    
success_modal_no_input <-
  modalDialog(
    title = "No Reference Normalization",
    fluidRow(
      column(10,
             align = "center", offset = 1,
             HTML('<h4 style= "color:#1A5276">No reference normalization will be applied. 
                      You may proceed to the subsequent tabs.</h4>'),
             hr(),
             actionButton("ref_dismiss", "Review results", style = "margin:5px;width:75%"),
             actionButton("goto_qc", "Continue to Data Summary tab", style = "margin:5px;width:75%"),
             actionButton("goto_filter", "Continue to Filter tab", style = "margin:5px;width:75%")
      )
    ),
    footer = NULL
  )

#'@details Help info modal for the pmartR::normalize_isobaric argument selectors
observeEvent(input$refnorm_see_example, {
  showModal({
    modalDialog(
      div(
        infotext_[["REFNORM_INTRO"]],
        br(),
        infotext_[["REFNORM_COLUMN_INFO_1"]],
        infotext_[["REFNORM_COLUMN_INFO_2"]]
      ),
      div(
        tags$img(src = "refnorm_example_1.png"),
        tags$img(src = "refnorm_example_2.png")
      )
    )
  })
})

#'@details Applies reference normalization inplace to isobaric or NMR omicsData.
refnorm <- function(){
  
  ## Tab sort
  if(inherits(objects$omicsData, "nmrData")){
    tab <- "NMR"
    cond <- get_nmr_norm(objects$omicsData)
  } else if (inherits(objects$omicsData, "isobaricpepData")){
    tab <- "Isobaric"
    cond <- get_isobaric_norm(objects$omicsData)
  } else {
    return()
  }

  ## Cancel Conditions
  if(is.null(input[[paste0(tab, "_ref_samps")]]) || 
     input[[paste0(tab, "_ref_samps")]] == "No")  return()
  
  req(!cond && input[[paste0(tab, "_ref_done_idcols")]] > 0)
  
  show("ref_norm_busy")
  
  on.exit({
    hide("ref_norm_busy")
  })
  
  if (tab == "Isobaric") {
    
    applied_norm <- tryCatch({
        Fdata_group_col <- input[[paste0(tab, "_ref_group")]]
        Fdata_ref_col <- input[[paste0(tab, "_ref_col")]]
        Ref_notation <- input[[paste0(tab, "_ref_notation")]]
        
        subtr <- which(colnames(objects$omicsData$e_data) == get_edata_cname(objects$omicsData))
        
        fdata_cname <- which(map_lgl(1:ncol(f_data()), function(col) {
          all(colnames(objects$omicsData$e_data)[-subtr] %in% f_data()[[col]])
        }))
        fdata_cname <- colnames(f_data())[fdata_cname]

        applied_norm <- as.isobaricpepData(
          e_data = objects$omicsData$e_data,
          f_data = f_data(),
          e_meta = objects$omicsData$e_meta,
          edata_cname = get_edata_cname(objects$omicsData),
          fdata_cname = fdata_cname,
          emeta_cname = get_emeta_cname(objects$omicsData),
          data_scale = get_data_scale(objects$omicsData),
          is_normalized = get_data_norm(objects$omicsData),
          isobaric_norm = get_isobaric_norm(objects$omicsData),
          check.names = F
        )
        
        applied_norm <- normalize_isobaric(applied_norm,
                                           exp_cname = Fdata_group_col,
                                           refpool_cname = Fdata_ref_col,
                                           refpool_notation = Ref_notation,
                                           apply_norm = TRUE
        )
        applied_norm
      },
      error = function(e) {
        msg <- paste0("Something went wrong reference normalizing your omicsData object.  <br> System error:  ", e)
        message(msg)
        revals$warnings_reference$bad_norm <<- messageBox(type = "error", msg)
        objects$omicsData
      }
    )
    
  } else if (tab == "NMR") {

    applied_norm <- tryCatch({
      if (input$NMR_reference_source == "Row in Data File (e.g. metabolite)") {
        applied_norm <- normalize_nmr(objects$omicsData,
                                           apply_norm = TRUE,
                                           backtransform = TRUE,
                                           metabolite_name = input$NMR_picker_reference
        )
      } else {
        objects$omicsData$f_data[[input$NMR_picker_reference]] <-
          as.numeric(f_data()[[input$NMR_picker_reference]])
        
        applied_norm <- normalize_nmr(objects$omicsData,
                                           apply_norm = TRUE,
                                           backtransform = TRUE,
                                           sample_property_cname = input$NMR_picker_reference
        )
      }
      applied_norm
    },
    error = function(e){
      msg <- paste0("Something went wrong reference normalizing your omicsData object.  <br> System error:  ", e)
      message(msg)
      revals$warnings_reference$bad_norm <<- messageBox(type = "error", msg)
      objects$omicsData
    })
    
  }
  
  objects$omicsData <- applied_norm
}

observeEvent(c(input$groups_dismiss, input$goto_reference), {
  
  req(!is.null(objects$omicsData))
  item <- objects$omicsData
  tabname <- ifelse(inherits(item, "nmrData"), "NMR", "Isobaric")
  assign_ref_uploads(tabname)
  output[["reference_data_ui"]] <- renderUI({
    upload_reference(tabname)
  })
})

observeEvent(input$ref_dismiss, {
  # updateCollapse(session, "groups_collapse_right", open = "fdata_plots")
  removeModal()
})

## Based on if ref sampls used, open, close collapse panels and show/hide icons
observeEvent(c(input$Isobaric_ref_samps, input$NMR_ref_samps, input$NMR_reference_source), {
  tabname <- ifelse(inherits(objects$omicsData, "nmrData"), "NMR", "Isobaric")

  refsamps <- input[[paste0(tabname, "_ref_samps")]]
  req(!is.null(refsamps))

  # showElement(paste0(tabname, "_reference_option_icon"))
  cond <- refsamps == "Yes"

  # toggleElement("ok_data_filters", condition = cond, anim = TRUE)
  # toggleElement("ok_data_filters", condition = !cond || objects$omicsData, anim = TRUE)

  if(cond){
    if(tabname != "NMR" || !is.null(input$NMR_reference_source)){
      showElement(paste0(tabname, "_reference_option_icon"))
      hideElement(paste0(tabname, "_reference_input_icon"))
      updateCollapse(session, "references_collapse_left", open = "columnids", close = "datselect")
    } else {
      hideElement(paste0(tabname, "_reference_option_icon"))
    }

  } else {
    showElement(paste0(tabname, "_reference_option_icon"))
    showElement(paste0(tabname, "_reference_input_icon"))
    updateCollapse(session, "references_collapse_left", close = c("columnids", "datselect"))
    showModal(success_modal_no_input)
    
  }

})

observeEvent(
  c(input[["NMR_ref_done_idcols"]], input[["Isobaric_ref_done_idcols"]]),
  {
    tab <- ifelse(inherits(objects$omicsData, "nmrData"), "NMR", "Isobaric")
    
    if(tab == "NMR"){
      cond <- get_nmr_norm(objects$omicsData)
    } else if (tab == "Isobaric"){
      cond <- get_isobaric_norm(objects$omicsData)
    } else {
      return()
    }
    
    req((is.null(cond) || !cond) && input[[paste0(tab, "_ref_done_idcols")]] > 0)

    makeobject()
    refnorm()
    makegroup()
    
    showModal(success_modal)
    
    showElement(paste0(tab, "_reference_input_icon"))
    
    updateCollapse(session, "references_collapse_left", close = "columnids")
    updateTabsetPanel(session, paste0(tab, "_ref_out_tabset"),
                      selected = "Reference Normalized"
    )
    updateTabsetPanel(session, paste0(tab, "_ref_preview_tables"),
                      selected = paste0("Reference Normalized ", tab, " Data File")
    )
  }
)

observeEvent(input$goto_filter, {
  updateTabsetPanel(session, "top_page", selected = "filter_tab")
  removeModal()
})

observeEvent(input$ref_done, {
  tabname <- ifelse(inherits(objects$omicsData, "nmrData"), "NMR", "Isobaric")

  if(tabname == "NMR"){
    cond_disable_apply <- is.null(input$NMR_picker_reference) ||
      get_nmr_norm(objects$omicsData) ||
      input$NMR_picker_reference %in% c(
        "Only applicable with reference normalization",
        "Please select a reference source",
        "Please upload Group file"
      )
  } else {
    cond_disable_apply <- any(map_lgl(
      list(
        input[[paste0(tabname, "_ref_group")]],
        input[[paste0(tabname, "_ref_col")]],
        input[[paste0(tabname, "_ref_notation")]]
      ),
      is.null
    )) ||
      any(map_lgl(
        list(
          input[[paste0(tabname, "_ref_group")]],
          input[[paste0(tabname, "_ref_col")]]
        ),
        function(input) input == "Only applicable with reference normalization"
      )) ||
      input[[paste0(tabname, "_ref_notation")]] == "Reference column required" ||
      (!is.null(get_isobaric_norm(objects$omicsData)) && get_isobaric_norm(objects$omicsData)) ||
      (!is.null(input$Isobaric_ref_samps) && input$Isobaric_ref_samps == "No")
    
  }
  
  req(!cond_disable_apply)
  
  updateCollapse(session, "references_collapse_left", close = "columnids")
  showElement(paste0(tabname, "_reference_input_icon"))
})

observeEvent(input$ref_reset,
  {
    req(input[["ref_reset"]] > 0)
    
    tab <- ifelse(inherits(objects$omicsData, "nmrData"), "NMR", "Isobaric")
    makeobject(use_iso = F)
    makegroup()
    
    hideElement(paste0(tab, "_reference_input_icon"))
    updateCollapse(session, "references_collapse_left", open = "columnids")
    
    updateTabsetPanel(session, paste0(tab, "_ref_out_tabset"),
                      selected = "Prior to Reference Normalization"
    )
    
    updateTabsetPanel(session, paste0(tab, "_ref_preview_tables"),
                      selected = paste0("Uploaded ", tab, " Data File")
    )
  }
)



# #### Enable Groups tab when all selected data types are complete ####
# observe({
#   selecteddata <- names(objects$omicsData) # Grab selected entries
#   req(length(selecteddata) > 0)
# 
#   # Catch disable conditions
#   if (!any(selecteddata %in% c("NMR", "Isobaric"))) { # should disable this tab
#     js$disableTab("Reference")
#     tooltip_span <- "<span id = 'reference-warning-tooltip'', class='glyphicon glyphicon-remove'', style='color:black;margin-right:3px'></span>"
#     html(
#       html = sprintf('<div>%sReference <span class="caret"></span></div>', tooltip_span),
#       selector = '.dropdown-toggle[data-value="Reference"]'
#     )
#     addTooltip(session, "reference-warning-tooltip", "Only applicable to NMR or Isobaric data")
#     return()
#   } else if (!complete_goals() || !complete_uploads() || is.null(input$upload_engage) || input$upload_engage < 1) { # Stuff that should disable this tab
#     js$disableTab("Reference")
#     html(html = 'Reference <span class="caret"></span>', selector = '.dropdown-toggle[data-value="Reference"]') # Blank the html and break
#     return()
#   }
# 
#   if (complete_reference()) {
#     tooltip_span <- "<span id = 'reference-progress-warning-tooltip', class='glyphicon glyphicon-exclamation-sign', style='color:red;margin-right:3px'></span>"
#     html(
#       html = sprintf("<div>%sProgress</div>", tooltip_span),
#       selector = 'a[data-value="Reference_progress"]'
#     )
# 
#     show_add_tooltip(
#       session,
#       "reference-progress-warning-tooltip",
#       complete_goals() && complete_uploads() && complete_reference() &&
#         (is.null(input$reference_engage) || input$reference_engage == 0),
#       "Confirm reference selections to proceed"
#     )
#   } else {
#     js$disableTab("Groups")
#     html(html = 'Groups <span class="caret"></span>', selector = '.dropdown-toggle[data-value="Groups"]')
# 
#     tooltip_span <- "<span id = 'reference-warning-tooltip'', class='glyphicon glyphicon-exclamation-sign'', style='color:red;margin-right:3px'></span>"
#     html(
#       html = sprintf('<div>%sReference <span class="caret"></span></div>', tooltip_span),
#       selector = '.dropdown-toggle[data-value="Reference"]'
#     )
#     addTooltip(session, "reference-warning-tooltip", "One or more required items are missing")
#   }
# })


### ############################################################################  Disable/enable mechanic
# observeEvent(input$reference_engage, {
#   js$enableTab("Groups")
# 
#   html(
#     html = 'Reference <span class="caret"></span>',
#     selector = '.dropdown-toggle[data-value="Reference"]'
#   )
# 
#   html(html = "<div>Progress</div>", selector = 'a[data-value="Reference_progress"]')
# 
#   updateNavbarPage(session, "top_page",
#                    selected = "Groups"
#   )
# })

