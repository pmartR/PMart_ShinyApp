list(
  # ui with conditional for fdata
  output$fdata_UI <- renderUI({
    if (two_lipids()) {
      tagList(
        HTML("<p style = 'font-weight:bold'>Upload Sample Assignment CSV Files for Positive and Negative Datasets</p>"),
        fluidRow(
          column(
            6,
            div(
              id = "js_file_fdata", style = "overflow-x:hidden",
              fileInput("file_fdata",
                label = NULL,
                multiple = FALSE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              )
            )
          ),
          column(
            6,
            div(
              id = "js_file_fdata_2", style = "overflow-x:hidden",
              fileInput("file_fdata_2",
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
      )
    }
    else {
      
      # Add different UI for MAP
      if (MAP) {
        
        # Set FileName to an empty string
        FileName <- ""
        
        # If a project object was uploaded, use that name 
        if (is.null(MapConnect$Project) == FALSE) {
          FileName <- MapConnect$Project$Data$f_data_filename %>% strsplit("/") %>% unlist() %>% tail(1)
        }
        
        
        div(
          id = "js_file_fdata",
          textInput("file_fdata", "Upload CSV Data File for Sample Assignment", 
                    value = FileName)
        )
        
      } else {
        div(
          id = "js_file_fdata",
          fileInput("file_fdata", "Upload CSV Data File for Sample Assignment",
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

  # id column(s) for new fdata if they choose to do grouping
  output$fdata_id_col <- renderUI({
    
    matching_col_n <- map_int(1:ncol(f_data()), function(col) {
      sum(f_data()[[col]] %in% colnames(objects$omicsData$e_data))
    })
    
    matching_col <- colnames(f_data())[which(matching_col_n == max(matching_col_n))[1]]
    
    if (two_lipids()) {
      
      matching_col_n2 <- map_int(1:ncol(f_data()), function(col) {
        sum(f_data()[[col]] %in% colnames(objects$omicsData$e_data))
      })
      
      matching_col2 <- colnames(f_data())[which(matching_col_n2 == max(matching_col_n2))[1]]
      
      tagList(
        splitLayout(cellArgs = list(style = "text-align:center"), "Dataset 1", "Dataset 2"),
        br(style = "padding:2px"),
        HTML("<p style = 'font-weight:bold'>Which columns in these files specify the respective sample names?</p>"),
        fluidRow(
          column(
            6,
            pickerInput("fdata_id_col", NULL,
              choices = colnames(f_data()),
              selected = matching_col
            )
          ),
          column(
            6,
            pickerInput("fdata_id_col_2", NULL,
              choices = colnames(f_data_2()),
              selected = matching_col2
            )
          )
        )
      )
    }
    else {
      
      pickerInput("fdata_id_col", "Which column in your grouping file indicates sample names?",
        choices = colnames(f_data()),
        selected = matching_col
      )
    }
  }),

  # display fdata
  output$fdata_table <- renderDT(
    {
      if (two_lipids() & input$which_fdata == 2) {
        tmp <- f_data_2()
      }
      else {
        tmp <- f_data()
      }

      tmp
    },
    options = list(scrollX = TRUE)
  ),

  ## Group barplots
  # first create two outputs.....
  # 1.
  output$group_barplots_1 <- renderPlotly({
    req(!is.null(attributes(objects$omicsData)$group_DF))

    # custom barplot
    p <- as.data.frame(get_group_table(objects$omicsData)) %>%
     plot_ly(
       x = ~group,
       y = ~Freq,
       color = ~group,
       type = "bar"
     ) %>% layout(yaxis = list(title = "Number of samples"),
                  xaxis = list(title = "Group"))
    
    # # custom barplot
    # p <- attributes(objects$uploaded_omicsData)$group_DF %>%
    #   group_by(Group) %>%
    #   mutate(n = n()) %>%
    #   ggplot(aes(x = Group, y = n)) +
    #   geom_bar(aes(fill = Group), position = "dodge", stat = "identity") +
    #   geom_text(aes(label = n), position = position_dodge(width = 1), vjust = -0.25) +
    #   ylab("Number of samples") +
    #   theme_bw()

    plots$last_plot <- p

    p
  }),

  # 2.
  output$group_barplots_2 <- renderPlotly({
    req(!is.null(attributes(objects$omicsData_2)$group_DF))
    
    # custom barplot
    p <- as.data.frame(get_group_table(objects$omicsData_2)) %>%
      plot_ly(
        x = ~group,
        y = ~Freq,
        color = ~group,
        type = "bar"
      ) %>% layout(yaxis = list(title = "Number of samples"),
                   xaxis = list(title = "Group"))
    
    # p <- attributes(objects$uploaded_omicsData_2)$group_DF %>%
    #   group_by(Group) %>%
    #   mutate(n = n()) %>%
    #   ggplot(aes(x = Group, y = n)) +
    #   geom_bar(aes(fill = Group), position = "dodge", stat = "identity") +
    #   geom_text(aes(label = n), position = position_dodge(width = 1), vjust = -0.25) +
    #   ylab("Number of samples") +
    #   theme_bw()

    plots$last_plot_2 <- p

    p
  }),

  # then conditionally display one or two plots
  output$group_barplots <- renderUI({
    if (two_lipids()) {
      tagList(
        div(id = "sample_barplots_1", 
            style = "border-style:solid;border-width:1px;", 
            withSpinner(plotlyOutput("group_barplots_1"))
            ),
        div(id = "sample_barplots_2", 
            style = "border-style:solid;border-width:1px;", 
            withSpinner(plotlyOutput("group_barplots_2"))
            )
      )
    }
    else {
      div(id = "sample_barplots_1", 
          style = "border-style:solid;border-width:1px;", 
          withSpinner(plotOutput("group_barplots_1"))
          )
    }
  }),
  ##

  ## options for creating custom sample names
  output$customsampnames <- renderUI({
    req(sample_names())
    min_char <- min(sapply(sample_names(), nchar))

    if (input$customsampnames_opts == "first_n") {
      numericInput("first_n", "Number of characters to keep:", value = min_char, step = 1)
    }
    else if (input$customsampnames_opts == "range") {
      splitLayout(
        numericInput("range_low", "From", value = 1, step = 1),
        numericInput("range_high", "To", value = min_char, step = 1)
      )
    }
    else if (input$customsampnames_opts == "split") {
      max_elements <- sapply(sample_names(), strsplit, input$delimiter) %>%
        sapply(length) %>%
        max()
      tagList(
        textInput("delimiter", "Split on character:", value = input$delimiter),
        pickerInput("split_el", "Which elements to keep after splitting?", choices = 1:max_elements, multiple = T)
      )
    }
  }),

  output$customsampnames_2 <- renderUI({
    req(sample_names_2())
    min_char <- min(sapply(sample_names_2(), nchar))

    if (input$customsampnames_opts == "first_n") {
      numericInput("first_n_2", "Number of characters to keep:", value = min_char, step = 1)
    }
    else if (input$customsampnames_opts == "range") {
      splitLayout(
        numericInput("range_low_2", "From", value = 1, step = 1),
        numericInput("range_high_2", "To", value = min_char, step = 1)
      )
    }
    else if (input$customsampnames_opts == "split") {
      max_elements <- sapply(sample_names_2(), strsplit, input$delimiter_2) %>%
        sapply(length) %>%
        max()
      tagList(
        textInput("delimiter_2", "Split on character:", value = input$delimiter_2),
        pickerInput("split_el_2", "Which element to keep after splitting?", choices = 1:max_elements, multiple = T)
      )
    }
  }),

  output$customsampnames_split <- renderUI({
    if (two_lipids()) {
      fluidRow(
        column(
          6,
          tags$p("Dataset 1", style = "text-align:center"),
          uiOutput("customsampnames")
        ),
        column(
          6,
          tags$p("Dataset 2", style = "text-align:center"),
          uiOutput("customsampnames_2")
        )
      )
    }
    else {
      uiOutput("customsampnames")
    }
  }),
  ##

  ### Why are the group values left out here? Redundant with graphs sure
  # grouped data summaries
  output$omicsData_groups_summary <- renderDT(revals$groups_summary[1:6, ], rownames = T, options = list(dom = "t")),
  output$omicsData_groups_summary_2 <- renderDT(revals$groups_summary_2[1:6, ], rownames = T, options = list(dom = "t")),

  output$grouped_data_summary <- renderUI({
    req(!is.null(revals$groups_summary), cancelOutput = TRUE)
    wellPanel(
      tagList(
        tags$b("Grouped Data Summary"),
        if (two_lipids()) {
          req(!is.null(revals$groups_summary_2), cancelOutput = TRUE)
          splitLayout(
            div(id = "groups_summary_1", DTOutput("omicsData_groups_summary")),
            div(id = "groups_summary_2", DTOutput("omicsData_groups_summary_2"))
          )
        }
        else {
          div(id = "groups_summary_1", DTOutput("omicsData_groups_summary"))
        }
      )
    )
  }),
  
  outputOptions(output, "grouped_data_summary", suspendWhenHidden = FALSE),
  #

  # group tab warnings
  output$warnings_groups <- renderUI({
    HTML(paste(revals$warnings_groups, collapse = ""))
  }),

  #### UI blocks for main effects and covariates
  output$group_col1 <- renderUI({
    req(!is.null(f_data()))
    pickerInput("gcol1", "Select first main effect",
      choices = c(
        "None",
        setdiff(colnames(f_data()), c(input$fdata_id_col, input$gcol2, input$cvcol1, input$cvcol2))
      ),
      selected = revals$gcol1,
      options = pickerOptions(dropupAuto = FALSE)
    )
  }),

  output$group_col2 <- renderUI({
    req(!is.null(f_data()))
    pickerInput("gcol2", "Select second main effect",
      choices = c(
        "None",
        setdiff(colnames(f_data()), c(input$fdata_id_col, input$gcol1, input$cvcol1, input$cvcol2))
      ),
      selected = revals$gcol2
    )
  }),

  output$cv_col1 <- renderUI({
    req(!is.null(f_data()))
    pickerInput("cvcol1", "Select first covariate",
      choices = c(
        "None",
        setdiff(colnames(f_data()), c(input$fdata_id_col, input$gcol1, input$gcol2, input$cvcol2))
      ),
      selected = revals$cvcol1
    )
  }),

  output$cv_col2 <- renderUI({
    req(!is.null(f_data()))
    pickerInput("cvcol2", "Select second covariate",
      choices = c(
        "None",
        setdiff(colnames(f_data()), c(input$fdata_id_col, input$gcol1, input$gcol2, input$cvcol1))
      ),
      selected = revals$cvcol2
    )
  }),

  #### Second set of UI blocks for main effects and covariates for lipid data
  output$group_col1_2 <- renderUI({
    req(!is.null(f_data_2()))
    pickerInput("gcol1_2", "Select first main effect",
      choices = c(
        "None",
        setdiff(colnames(f_data_2()), c(input$fdata_id_col_2, input$gcol2_2, input$cvcol1_2, input$cvcol2_2))
      ),
      selected = revals$gcol1_2
    )
  }),

  output$group_col2_2 <- renderUI({
    req(!is.null(f_data_2()))
    pickerInput("gcol2_2", "Select second main effect",
      choices = c(
        "None",
        setdiff(colnames(f_data_2()), c(input$fdata_id_col_2, input$gcol1_2, input$cvcol1_2, input$cvcol2_2))
      ),
      selected = revals$gcol2_2
    )
  }),

  output$cv_col1_2 <- renderUI({
    req(!is.null(f_data_2()))
    pickerInput("cvcol1_2", "Select first covariate",
      choices = c(
        "None",
        setdiff(colnames(f_data_2()), c(input$fdata_id_col_2, input$gcol1_2, input$gcol2_2, input$cvcol2_2))
      ),
      selected = revals$cvcol1_2
    )
  }),

  output$cv_col2_2 <- renderUI({
    req(!is.null(f_data_2()))
    pickerInput("cvcol2_2", "Select second covariate",
      choices = c(
        "None",
        setdiff(colnames(f_data_2()), c(input$fdata_id_col_2, input$gcol1_2, input$gcol2_2, input$cvcol1_2))
      ),
      selected = revals$cvcol2_2
    )
  })
)
