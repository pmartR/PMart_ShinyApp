list(
  output$edata_UI <- renderUI({
    if (two_lipids()) {
      tagList(
        HTML("<p style = 'font-weight:bold'>Upload CSV Data Files for Positive and Negative Lipids</p>"),
        splitLayout(
          cellArgs = list(style = "overflow-x:hidden"),
          div(
            id = "js_file_edata",
            fileInput("file_edata",
              label = NULL,
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),
          div(
            id = "js_file_edata_2",
            fileInput("file_edata_2",
              label = NULL,
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          )
        )
      )
    }
    else {
      
      if (MAP_ACTIVE) {

        div(
          id = "js_file_edata",
          textInput("file_edata", "Uploaded CSV Expression Data file", 
                    value = MapConnect$Project$Data$e_data_filename %>% strsplit("/") %>% unlist() %>% tail(1))
        )
        
      } else {
        
        div(
          id = "js_file_edata",
          fileInput("file_edata", "Upload CSV Expression Data file",
                    multiple = FALSE,
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    )
          )
        )
        
      }
    
    }
  }),

  # mass id column identifiers
  output$id_col <- renderUI({
    if (two_lipids()) {
      choices_1 <- colnames(e_data())
      choices_2 <- colnames(e_data_2())
      tagList(
        HTML("<p style = 'font-weight:bold'>What columns in each dataset specify unique biomolecules?</p>"),
        fluidRow(
          column(6, pickerInput("id_col", choices = choices_1)),
          column(6, pickerInput("id_col_2", choices = choices_2))
        )
      )
    }
    else {
      choices <- colnames(e_data())
      pickerInput("id_col", "What column identifies unique molecules?", choices = choices)
    }
  }),

  # select data scale
  output$datascale_UI <- renderUI({
    
    ## If seqData, only allow counts
    
    ch <- list("Raw intensity" = "abundance", "Log base 2" = "log2", 
               "Log base 10" = "log10", "Natural log" = "log", 
               "Counts" = "counts")
    
    if(input$datatype == "seq"){
      selected <- "counts"
      disabled <- !(ch %in% "counts")
      cond <- "input.data_scale == 'counts'"
      text <- ttext_[["ABUNDANCE_NAS_TO_ZEROS"]]
    } else {
      selected <- "abundance"
      disabled <- ch %in% "counts"
      cond <- "input.data_scale == 'abundance'"
      text <- ttext_[["ABUNDANCE_ZEROS_TO_NA"]]
    }
    
    div(class = "inline-wrapper-1",
      pickerInput("data_scale", "On what scale is your data?",
        choices = ch,
        selected = selected,
        choicesOpt = list(disabled = disabled)
      ),
      conditionalPanel(
        cond,
        tipify(
          blueexcl,
          title = gsub("\n", " ", text)
        )
      )
    )
  }),

  # transform to what scale?
  output$transform_UI <- renderUI({
    
    req(input$datatype != "seq")
    
    ## Disable for seqdata
    choices <- list("Raw intensity" = "abundance", "Log base 2" = "log2", 
                    "Log base 10" = "log10", "Natural log" = "log")
    pickerInput("transform", "Transform data to:", choices = c("Select one", choices))
  }),

  # normalized or not?
  output$normalized_UI <- renderUI({
    
    req(input$datatype != "seq")
    radioGroupButtons("normalized_yn", "Has your data been normalized?",
      choices = c("Yes" = 1, "No" = 0),
      selected = 0
    )
  }),

  # do they have an emeta file to upload?
  output$emeta_yn_UI <- renderUI({
    req(input$datatype, input$datatype != "none")
    radioGroupButtons("emeta_yn",
      "Do you have a file containing extra biomolecule information?",
      choices = c("Yes" = T, "No" = F)
    )
  }),

  # select which column contains protein ID
  # if not protein data or there is no e_meta, then simply pass a random column or NULL
  output$promap_UI <- renderUI({
    req(length(colnames(revals$e_meta))> 0)
    if(!(input$id_col %in% colnames(revals$e_meta))){
      return(
        HTML(messageBox(type = "error", 
                        paste0("Unique biomolecule column '", input$id_col, 
                            "' also required in biomolecule file.",
                            " Current biomolecule file contains the following columns: ",
                            toString(colnames(revals$e_meta)))))
      )
    } else if (input$datatype == "pep" & isTRUE(as.logical(input$proteins_yn))) {
      choices <- colnames(revals$e_meta)[-which(colnames(revals$e_meta) == input$id_col)]
      pickerInput("protein_column", "Which column in your biomolecule file contains protein identifiers?",
        choices = c("Select a column", choices), selected = "Select a column"
      )
    }
    else {
      return(NULL)
    }
  }),
  
  # pro question in emeta upload sub-panel
  output$emeta_pro_UI <- renderUI({
    req(!is.null(input$file_emeta) && input$emeta_yn == "TRUE" && input$datatype == 'pep')
    radioGroupButtons(
      "proteins_yn",
      "Does your biomolecule information file contain peptide to protein mappings?",
      choices = c("Yes" = "TRUE", "No" = "FALSE")
    )
  }),

  # emeta upload sub-panel
  output$emeta_UI <- renderUI({
    
    if (MAP_ACTIVE) {
      fname_tmp = MapConnect$Project$Data$e_meta_filename
      fname_tmp = if(!isTruthy(fname_tmp)) {
        "No e_meta file found - indicate no e_meta."
      } else {
        fname_tmp %>% 
          strsplit("/") %>% unlist() %>% tail(1)
      }
      
      title_upload_div <- disabled(div(
        id = "js_file_emeta",
        textInput("file_emeta", "Uploaded CSV Biomolecule Information file", 
                  value = fname_tmp)
      ))
      
    } else {
      
      title_upload_div <- div(
        id = "js_file_emeta",
        fileInput("file_emeta", "Upload CSV Biomolecule Information file",
                  multiple = FALSE,
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"
                  )
        )
      )
      
    }

    if (two_lipids()) {
      tagList(
        HTML("<p style = 'font-weight:bold'>Upload CSV Biomolecule Information Files for Positive and Negative Lipids</p>"),
        splitLayout(
          cellArgs = list(style = "overflow-x:hidden"),
          div(
            id = "js_file_emeta",
            fileInput("file_emeta",
              label = NULL,
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),
          div(
            id = "js_file_emeta_2",
            fileInput("file_emeta_2",
              label = NULL,
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          )
        )
      )
    }
    else if (isTRUE(as.logical(input$emeta_yn))) {
      title_upload_div
    }
    else {
      hidden(title_upload_div)
    }
  }),

  # boxplots collapse panel UI elements
  output$omicsData_upload_boxplot <- renderPlotly({
    req(!is.null(objects$uploaded_omicsData))
    
    plot_args = list(
      objects$omicsData,
      "interactive" = TRUE
    )
    
    ## If Data type is seq, add transformation
    if(input$datatype == "seq") {
      plot_args[["transformation"]] = "lcpm"
    }
    
    p <- do.call(plot, plot_args) 
    plots$last_plot <- p
    return(p)
  }),

  output$omicsData_2_upload_boxplot <- renderPlotly({
    req(!is.null(objects$uploaded_omicsData_2))
    p <- plot(objects$uploaded_omicsData_2, bw_theme = TRUE, interactive = T) #+ theme(axis.text.x = element_blank())
    plots$last_plot_2 <- p
    return(p)
  }),
  #

  # uploaded data summaries
  output$omicsData_upload_summary <- renderDT(revals$upload_summary, rownames = T, options = list(dom = "t")),
  output$omicsData_upload_summary_2 <- renderDT(revals$upload_summary_2, rownames = T, options = list(dom = "t")),

  # conditionally display one or two elements for...

  # ...boxplots...
  output$upload_boxplots <- renderUI({
    if (two_lipids()) {
      tagList(
        div(id = "upload_boxplots_1", 
            style = "border-style:solid;border-width:1px;", 
            withSpinner(plotlyOutput("omicsData_upload_boxplot"))
            ),
        div(id = "upload_boxplots_2", 
            style = "border-style:solid;border-width:1px;", 
            withSpinner(plotlyOutput("omicsData_2_upload_boxplot"))
            )
      )
    }
    else {
      div(id = "upload_boxplots_1", 
          style = "border-style:solid;border-width:1px;", 
          withSpinner(plotlyOutput("omicsData_upload_boxplot"))
          )
    }
  }),
  # ...summary tables
  output$uploaded_data_summary <- renderUI({
    req(!is.null(revals$upload_summary))
    wellPanel(
      tagList(
        tags$b("Data Summary"),
        if (two_lipids()) {
          req(!is.null(revals$upload_summary_2))
          splitLayout(
            div(id = "upload_summary_1", DTOutput("omicsData_upload_summary")),
            div(id = "upload_summary_2", DTOutput("omicsData_upload_summary_2"))
          )
        }
        else {
          div(id = "upload_summary_1", DTOutput("omicsData_upload_summary"))
        }
      )
    )
  }),
  #

  # collect warnings
  output$warnings_upload <- renderUI({
    HTML(paste(revals$warnings_upload, collapse = ""))
  }),

  # e_data display
  output$head_edata <- DT::renderDT(
    {
      if (two_lipids() & isTRUE(input$which_table == 2)) {
        tmp <- e_data_2()
      }
      else {
        tmp <- e_data()
      }

      tmp <- tmp %>% mutate_each(as.character)
      tmp
    },
    options = list(scrollX = TRUE)
  ),

  # e_meta display
  output$head_emeta <- DT::renderDT(
    {
      if (two_lipids() & isTRUE(input$which_table == 2)) {
        req(!is.null(revals$e_meta_2))
        tmp <- revals$e_meta
      }
      else {
        req(!is.null(revals$e_meta))
        tmp <- revals$e_meta
      }

      tmp <- tmp %>% mutate_each(as.character)
      tmp
    },
    options = list(scrollX = TRUE)
  ),

  # head_emeta wrapper to remove whitespace
  output$head_emeta_wrapper <- renderUI({
    req(isTruthy(as.logical(input$emeta_yn)) | input$datatype == "pep")
    DTOutput("head_emeta")
  })
  
  
)
