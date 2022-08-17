upload_UI <- function() {
  navbarMenu("Upload",
    tabPanel("Upload Data",
      value = 'upload_data_tab',
      class = "collapse_page",
      fluidRow( # begin fluidrow
        column(
          4, # sidebarpanel
          bsCollapse(
            id = "upload_collapse_left", open = c("datselect"), multiple = TRUE, # parent collapse div
            # upload edata sub-collapse div
            bsCollapsePanel(div(
              "Specify data type and upload data file",
              hidden(div(id = "ok_datselect", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
            ),
            value = "datselect",
            div(
              id = "js_datatype",
              class = "inline-wrapper-1",
              pickerInput(
                "datatype",
                "Specify molecule type",
                choices = c(
                  "None" = "none",
                  "Peptides" = "pep",
                  "Proteins" = "pro",
                  "Metabolites" = "metab",
                  "Lipids" = "lip",
                  "Transcripts" = "seq"
                )
              ),

              bsButton("upload_to_datareqs", "See data requirements", icon = blueq),
            ),
            conditionalPanel(
              "input.datatype=='pep'",
              radioGroupButtons("labeled_yn", "Is this labeled peptide data?",
                                choices = c("Yes" = "iso", "No" = "pep"), selected = "pep"
              )
            ),
            
            conditionalPanel(
              "input.datatype=='metab'",
              radioGroupButtons("metab_type", "Which type of instrumentation?",
                                choices = c("LC/MS or GC/MS" = "metab", "NMR" = "nmr"), selected = "FALSE"
              )
            ),
            
            conditionalPanel(
              "input.datatype=='lip'",
              radioGroupButtons("twolipids_yn", "Separate files for positive and negative ionization?",
                choices = c("Yes" = "TRUE", "No" = "FALSE")
              )
            ),
             
          hidden(div(
            id = "edata_UI_parent",
            uiOutput("edata_UI")
          ))
          ),

            # specify various data info sub-collapse div
            bsCollapsePanel(div(
              "Specify ID Columns and Data Values",
              hidden(div(id = "ok_columnids", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
            ),
            value = "columnids",
            hidden(div(
              id = "js_id_col",
              uiOutput("id_col")
            )),

            hidden(div(
              id = "js_datascale",
              uiOutput("datascale_UI"),
              uiOutput("transform")
            )),
            
            conditionalPanel(
              "input.labeled_yn == 'seq'",
              hidden(div(
                class = "inline-wrapper-1",
                id = "js_na_symbol",
                textInput("na_symbol", "What value denotes missing data?", value = NA),
                tipify(
                  div(style="color:deepskyblue", icon("question-sign", lib = "glyphicon")),
                  title = ttext_[["MISSING_DATA_REPLACE"]]
                )
              ))),
          
            # conditionalPanel(
            #   "input.labeled_yn == 'pep'",
              hidden(div(
                id = "js_normalized_yn",
                uiOutput("normalized_UI")
              )),
            # ),
            div(id = "donebutton", style = "float:right", actionButton("done_idcols", div("I'm done specifying values", icon("ok-sign", lib = "glyphicon"))))
            ),
            # upload e_meta and specify protein column sub-collapse div
            bsCollapsePanel(div(
              "Upload Biomolecule Information",
              hidden(div(id = "ok_metadata", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
            ),
            value = "meta_collapse",
            uiOutput("emeta_yn"),
            hidden(div(
              id = "js_emeta_UI",
              uiOutput("emeta_UI")
            )),
            
              uiOutput("emeta_pro_UI"),
            
            div(
              id = "js_promap",
              uiOutput("promap_UI")
            )
            )
          ), # parent collapse
          disabled(bsButton("makeobject", "Create omicsData object", style = "primary")),
          uiOutput("warnings_upload"),
          br(),
          uiOutput("uploaded_data_summary")
        ), # column 4
        column(
          8,
          bsCollapse(
            id = "upload_preview_collapse",
            bsCollapsePanel("Data Preview",
              value = "summary_tables",
              hidden(div(
                id = "toggle_table",
                div(style = "float:left;margin-top:10px;margin-right:10px;font-weight:bold", "Display dataset:"),
                radioGroupButtons("which_table", choices = c("1" = 1, "2" = 2))
              )),
              DTOutput("head_edata"),
              uiOutput("head_emeta_wrapper")
            ),
            bsCollapsePanel("Boxplot Preview",
              value = "summary_boxplots",
              uiOutput("upload_boxplots")
            ),
            open = "summary_tables"
          )
        ) # main_column
      ) # fluidrow
    ), # tabpanel
    tabPanel(
      "Data Requirements",
      value = "data_requirements",
      includeMarkdown("www/data_reqs.md")
    )
  )
}
