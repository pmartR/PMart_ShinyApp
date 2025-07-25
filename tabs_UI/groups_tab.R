groups_UI <- function() {
  tabPanel("Group Samples",
    value = "group_samples_tab",
    class = "collapse_page",
    fluidRow(
      column(
        4,
        bsCollapse(
          id = "groups_collapse_left", open = "fdata_upload", multiple = TRUE, # parent collapse div
          # file upload collapse sub-div
          bsCollapsePanel(div(
            "Upload Sample Information",
            hidden(div(id = "ok_fdata_upload", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          value = "fdata_upload",
          div(id = "upload_fdata", style = "display:inline-block;width:100%", uiOutput("fdata_UI")),
          div(
            id = "js_download_fdata", class = "inline_top", style = "float:right",
            disabled(downloadButton("download_fdata", "Download file template"))
          ),
          radioGroupButtons("usevizsampnames", "Add trimmed sample names for plotting?", choices = c("Yes", "No"), selected = "No"),
          conditionalPanel(
            condition = "input.usevizsampnames == 'Yes'",
            radioGroupButtons("customsampnames_opts", "Trim by:",
              choices = c("First x characters" = "first_n", "Range of characters" = "range", "Split by a character" = "split")
            ),
            uiOutput("customsampnames"),
            uiOutput("preview_trim")
          )
          ),
          # ID column collapse sub-div
          bsCollapsePanel(
            div(
              "Specify Main Effects and Covariates",
              tipify(
                span(style = "color:rgb(0,191,255)", icon("question-sign", lib = "glyphicon")), 
                title = ttext_[["MAIN_EFFECTS_INFO"]]
              ),
              hidden(div(id = "ok_fdata_idcols", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
            ),
            value = "fdata_columns",
            hidden(div(id = "js_fdata_id_col", uiOutput("fdata_id_col_UI"))),
            uiOutput("group_col1"),
            uiOutput("group_col2"),
            uiOutput("cv_col1"),
            uiOutput("cv_col2"),
            uiOutput("pairing_col_1"),
            uiOutput("covariates_type_picker_UI_wrapper")
          ),
          bsCollapsePanel(
            div(
              "Specify Pairing Structure",
              tipify(
                span(style = "color:rgb(0,191,255)", icon("question-sign", lib = "glyphicon")), 
                title = ttext_[["PAIRING_INFO"]]
              ),
              hidden(div(id = "ok_fdata_pair_cols", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
            ),
            value = "pair_columns",
            fluidRow(
              column(
                6,
                uiOutput("pairing_id_col"),
                uiOutput("pairing_group_col"),
                uiOutput("pairing_denom_col")
              )
            )
          )
        ),
        bsCollapsePanel(
          div(
            "Specify Batch Information",
            tipify(
              span(style = "color:rgb(0,191,255)", icon("question-sign", lib = "glyphicon")), 
              title = ttext_[["BATCH_ID_INFO"]]
            ),
            hidden(div(id = "ok_fdata_batch_idcols", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          value = "batch_columns",
          fluidRow(
            column(
              6,
              uiOutput("batch_id"),
              uiOutput("batch_correction_id")
            )
          )
        ),# parent collapse
        disabled(bsButton("group_designation", "Apply grouping", style = "primary")),
        disabled(bsButton("group_reset", "Reset grouping", style = "primary")),
        br(),
        br(),
        uiOutput("warnings_groups"),
        uiOutput("grouped_data_summary")
      ), # column 4
      column(
        8,
        bsCollapse(
          id = "groups_collapse_right",
          bsCollapsePanel("Grouping File Preview",
            value = "fdata_preview",
            DTOutput("fdata_table")
          ),
          bsCollapsePanel("Group Plots",
            value = "fdata_plots",
            uiOutput("group_barplots")
          )
        )
      )
    )
  )
}
