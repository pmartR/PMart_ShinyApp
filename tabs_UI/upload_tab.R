upload_UI <- function() {
  tabPanel("Upload Data",
    class = "collapse_page",
    fluidRow( # begin fluidrow
      column(
        4, # sidebarpanel
        bsCollapse(
          id = "upload_collapse_left", open = "datselect", multiple = TRUE, # parent collapse div
          # upload edata sub-collapse div
          bsCollapsePanel(div(
            "Specify data type and upload data file",
            hidden(div(id = "ok_datselect", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          value = "datselect",

          div(id = "js_datatype", pickerInput("datatype", "Specify molecule type",
            choices = c("None" = "none", "Peptides" = "pep", "Proteins" = "pro", "Metabolites" = "metab", "Lipids" = "lip")
          )),
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
          conditionalPanel(
            "input.datatype=='pep'",
            radioGroupButtons("proteins_yn", "Does your metadata file contain peptide to protein mappings?",
              choices = c("Yes" = "TRUE", "No" = "FALSE")
            ),
            radioGroupButtons("labeled_yn", "Is this labeled peptide data?",
              choices = c("Yes" = "TRUE", "No" = "FALSE"), selected = "FALSE"
            )
          ),
          hidden(div(
            id = "js_id_col",
            uiOutput("id_col")
          )),
          hidden(div(
            id = "js_datascale",
            uiOutput("datascale_UI"),
            uiOutput("transform")
          )),
          hidden(div(
            id = "js_na_symbol",
            textInput("na_symbol", "What value denotes missing data?", value = NA)
          )),
          conditionalPanel(
            "input.labeled_yn == 'FALSE'",
            hidden(div(
              id = "js_normalized_yn",
              uiOutput("normalized_UI")
            ))
          ),
          div(id = "donebutton", style = "float:right", actionButton("done_idcols", div("I'm done specifying values", icon("ok-sign", lib = "glyphicon"))))
          ),
          # upload e_meta and specify protein column sub-collapse div
          bsCollapsePanel(div(
            "Upload Biomolecule Information",
            hidden(div(id = "ok_metadata", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          value = "meta_collapse",
          hidden(div(
            id = "js_emeta_UI",
            uiOutput("emeta_UI")
          )),
          div(
            id = "js_promap",
            uiOutput("promap_UI")
          )
          )
        ), # parent collapse
        disabled(actionButton("makeobject", "Create omicsData object")),
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
            DTOutput("head_emeta")
          ),
          bsCollapsePanel("Boxplot Preview",
            value = "summary_boxplots",
            uiOutput("upload_boxplots")
          ),
          open = "summary_tables"
        )
      ) # main_column
    ) # fluidrow
  ) # tabpanel
}