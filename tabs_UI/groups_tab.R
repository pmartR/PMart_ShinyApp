groups_UI <- function() {
  tabPanel("Group Samples",
    class = "collapse_page",
    fluidRow(
      column(
        4,
        bsCollapse(
          id = "groups_collapse_left", open = "fdata_upload", multiple = TRUE, # parent collapse div
          # file upload collapse sub-div
          bsCollapsePanel(div(
            "Upload Groups File",
            hidden(div(id = "ok_fdata_upload", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          value = "fdata_upload",
          div(id = "upload_fdata", style = "display:inline-block", uiOutput("fdata_UI")),
          div(
            id = "js_download_fdata", class = "inline_top", style = "float:right",
            disabled(downloadButton("download_fdata", "Download file template"))
          ),
          radioGroupButtons("usevizsampnames", "Add trimmed sample names for plotting?", choices = c("Yes", "No"), selected = "No"),
          conditionalPanel(
            condition = "input.usevizsampnames == 'Yes'",
            radioGroupButtons("customsampnames_opts", "Trim By:",
              choices = c("First x characters" = "first_n", "Range of characters" = "range", "Split by a character" = "split")
            ),
            uiOutput("customsampnames_split")
          )
          ),
          # ID column collapse sub-div
          bsCollapsePanel(div(
            "Specify Main Effects and Covariates",
            tipify(
              span(style = "color:rgb(0,191,255)", icon("question-sign", lib = "glyphicon")), 
              title = ttext_[["MAIN_EFFECTS_INFO"]]
            ),
            hidden(div(id = "ok_fdata_idcols", style = "color:orange;float:right", icon("ok", lib = "glyphicon")))
          ),
          value = "fdata_columns",
          hidden(div(id = "js_fdata_id_col", uiOutput("fdata_id_col"))),
          fluidRow(
            column(
              6,
              uiOutput("group_col1"),
              uiOutput("group_col2"),
              uiOutput("cv_col1"),
              uiOutput("cv_col2")
            ),
            column(
              6,
              uiOutput("group_col1_2"),
              uiOutput("group_col2_2"),
              uiOutput("cv_col1_2"),
              uiOutput("cv_col2_2")
            )
          )
          )
        ), # parent collapse
        disabled(bsButton("group_designation", "Apply Grouping", style = "primary")),
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
            hidden(div(
              id = "toggle_fdata",
              div(style = "float:left;margin-top:10px;margin-right:10px;font-weight:bold", "Display dataset:"),
              radioGroupButtons("which_fdata", choices = c("1" = 1, "2" = 2))
            )),
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
