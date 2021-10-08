## Reference tab info

reference_UI <- function() {
  tabPanel(
    class = "collapse_page",
    "Reference",
    value = "reference_tab",
    uiOutput("reference_data_ui")
  )
}

upload_reference <- function(tabname) {
  
  ##### Sidebar by Isobaric #####
  if (tabname == "Isobaric") {
    sidebar <- column( # sidebarpanel
      4,
      # Upload fdata
      bsCollapse(
        id = "references_collapse_left",
        open = c("datselect"),
        multiple = TRUE, # parent collapse div
        
        bsCollapsePanel(
          value = "datselect",
          subsection_header(
            paste0("Use Isobaric Reference Data"),
            "Isobaric_reference_option_icon",
            "color:orange;float:right",
            icon("ok", lib = "glyphicon")
          ),
          
          div(
            uiOutput("Isobaric_fdata_upload_UI")
          )
        ), # End collapse panel
        
        
          bsCollapsePanel(
            value = "columnids",
            subsection_header(
              "Specify Reference Column and Notation",
              "Isobaric_reference_input_icon",
              "color:orange;float:right",
              icon("ok", lib = "glyphicon")
            ),
            # conditionalPanel("typeof input.Isobaric_ref_samps !== 'undefined' && input.Isobaric_ref_samps == 'Yes'", {
            #   div(
            # uiOutput('Isobaric_fdata_cname_UI'),
            uiOutput("Isobaric_ref_group_UI"),
            uiOutput("Isobaric_ref_col_UI"),
            uiOutput("Isobaric_ref_notation_UI"),
            br(),
            div(style = "float:right", actionButton("ref_done", div("I'm done specifying values", icon("ok-sign", lib = "glyphicon"))))
            
              # )
            # })
          )
      ), # parent collapse
      uiOutput("Isobaric_ref_done_idcols_UI")
    ) # column 4
    
    ##### Sidebar by NMR #####
  } else if (tabname == "NMR") {
    sidebar <- column( # sidebarpanel
      4,
      
      # Allow normalization by-pass
      bsCollapse(
        id = "references_collapse_left",
        open = c("datselect"),
        multiple = TRUE, # parent collapse div
        
        bsCollapsePanel(
          value = "datselect",
          subsection_header(
            "NMR Normalization options",
            "NMR_reference_option_icon",
            "color:orange;float:right",
            icon("ok", lib = "glyphicon")
          ),
            uiOutput("NMR_ref_samps_UI"),
            uiOutput("NMR_reference_source_UI")
        ), # End collapse panel
        
          bsCollapsePanel(
            value = "columnids",
            subsection_header(
              "Specify Reference Identifier",
              "NMR_reference_input_icon",
              "color:orange;float:right",
              icon("ok", lib = "glyphicon")
            ),
            
              uiOutput("NMR_ref_id_UI"),
            br(),
            div(style = "float:right", actionButton("ref_done", div("I'm done specifying values", icon("ok-sign", lib = "glyphicon"))))
            
          ) # End collapse panel
        
      ), # parent collapse
      uiOutput("NMR_ref_done_idcols_UI"),
      br(),
      uiOutput("warnings_reference")
    ) # column 4
  }
  
  # Main tabpanel ####
           fluidRow( # begin fluidrow
             
             sidebar,
             
             column(
               8,
               bsCollapse(
                 id = "upload_preview_collapse",
                 open = c("summary_tables", "summary_boxplots"),
                 
                 bsCollapsePanel(
                   "Data Preview",
                   value = "summary_tables",
                   tabsetPanel(
                     id = paste0(tabname, "_ref_preview_tables"),
                     tabPanel(
                       paste0("Uploaded ", tabname, " Data File"),
                       br(),
                       withSpinner(uiOutput(paste0(tabname, "_ref_head_edata_UI")))
                     ),
                     tabPanel(
                       "Reference Data File",
                       br(),
                       withSpinner(uiOutput(paste0(tabname, "_ref_head_fdata_UI")))
                     ),
                     tabPanel(
                       paste0("Reference Normalized ", tabname, " Data File"),
                       br(),
                       withSpinner(uiOutput(paste0(tabname, "_norm_edata_UI")))
                     )
                   )
                 ),
                 
                 bsCollapsePanel(
                   "Normalized Boxplot Preview",
                   value = "summary_boxplots",
                   tabsetPanel(
                     id = paste0(tabname, "_ref_out_tabset"),
                     tabPanel(
                       id = "prior",
                       "Prior to Reference Normalization",
                       br(),
                       withSpinner(uiOutput(paste0(tabname, "_ref_upload_bp")))
                     ),
                     tabPanel(
                       id = "post",
                       "Reference Normalized",
                       br(),
                       withSpinner(uiOutput(paste0(tabname, "_ref_norm_bp")))
                     )
                   )
                 )
               ) # Collapse parent
             ) # main_column
           ) # fluidrow
  # ) # tabpanel
}
