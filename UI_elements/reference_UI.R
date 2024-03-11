# ## Reference tab UI

#### Resetting feature for reference normalization

assign_ref_uploads <- function(tabname) {

  ### Inputs ###
  # Sidebars for NMR vs. Isobaric
  if (tabname == "NMR") {
    output[["NMR_ref_samps_UI"]] <- renderUI({
      
      if(attr(objects$omicsData, "nmr_info")$norm_info$is_normalized){
      return(
        disabled(radioGroupButtons("NMR_ref_samps",
                          label = "Apply reference normalization to data?",
                          choices = c("Yes", "No"),
                          selected = ifelse(is.null(isolate(input$NMR_ref_samps)), 
                                            "No", 
                                            isolate(input$NMR_ref_samps)))
        )
      )
    }
      
        return(
          radioGroupButtons("NMR_ref_samps",
                            label = "Apply reference normalization to data?",
                            choices = c("Yes", "No"),
                            selected = ifelse(is.null(isolate(input$NMR_ref_samps)), 
                                              character(0), 
                                              isolate(input$NMR_ref_samps))
          )
        )
    })

    output[["NMR_reference_source_UI"]] <- renderUI({
      if (!is.null(input$NMR_ref_samps) && input$NMR_ref_samps == "Yes") {
        return(
          radioButtons(
            "NMR_reference_source",
            label = "Select reference source:",
            choices = c(
              "Row in Expression Data (e.g. metabolite)" = "e_data",
              "Column in Sample Information (e.g. sample concentration)" = "f_data"
            ),
            selected = ifelse(is.null(isolate(input$NMR_reference_source)), 
                              character(0), 
                              isolate(input$NMR_reference_source))
          )
        )
      } else {
        return(
          hidden(
            radioButtons(
              "NMR_reference_source",
              label = "Select reference source:",
              choices = c(
                "Row in Expression Data (e.g. metabolite)" = "e_data",
                "Column in Sample Information (e.g. sample concentration)" = "f_data"
              ),
              selected = character(0)
            )
          )
        )
      }
    })

    ## Specify Values
    output[["NMR_ref_id_UI"]] <- renderUI({
      if (is.null(input$NMR_ref_samps) || input$NMR_ref_samps == "No") {
        return(
          disabled(pickerInput(
            "NMR_picker_reference",
            choices = c(
              "Only applicable with reference normalization",
              "placeholder"
            )
          ))
        )
      } else if (is.null(input$NMR_reference_source)) {
        return(
          disabled(pickerInput(
            "NMR_picker_reference",
            choices = c(
              "Please select a reference source",
              "placeholder"
            )
          ))
        )
      } else if (input$NMR_reference_source == "f_data" &&
                 is.null(objects$omicsData$f_data)) {
        return(
          disabled(pickerInput(
            "NMR_picker_reference",
            choices = c(
              "Please upload Group file",
              "placeholder"
            )
          ))
        )
      } else if (input$NMR_reference_source == "f_data") {
  
        df <- f_data()
        # Only colplete, numeric columns are valid
        disabled <- unlist(apply(df, 2, function(col) any(is.na(as.numeric(col)))))

        out <- pickerInput(
          "NMR_picker_reference",
          div(
            "Select reference column:",
            div(
              style = "color:red;display:inline-block",
              tipify(
                icon("question-sign", lib = "glyphicon"),
                title = ttext_[["REFERENCE_DISABLED_COL"]]
              )
            )
          ),
          choices = colnames(df),
          options = pickerOptions(maxOptions = 1),
          choicesOpt = list(disabled = disabled),
          selected = isolate(input$NMR_picker_reference),
          multiple = TRUE
        )

        return(
          out
        )
      } else if(!is.null(isolate(input$NMR_picker_reference)) && 
           get_nmr_norm(objects$omicsData) &&
           input$NMR_ref_samps == "Yes"){
          
          out <- disabled(pickerInput(
            "NMR_picker_reference",
            div(
              "Select reference standard identifier:",
              div(
                style = "color:deepskyblue;display:inline-block",
                tipify(
                  icon("question-sign", lib = "glyphicon"),
                  title = ttext_[["REFERENCE_DISABLED_ROW"]]
                )
              )
            ),
            choices = c(isolate(input$NMR_picker_reference), "placeholder"),
            options = pickerOptions(maxOptions = 1),
            selected = isolate(input$NMR_picker_reference),
            multiple = TRUE
          ))
          return(out)
          
        } else  {
          
        # Only complete references are valid; post-transformation in case negatives exist
        rmv_col <- which(colnames(objects$omicsData$e_data) == get_edata_cname(objects$omicsData))
        choices <- objects$omicsData$e_data[[rmv_col]]
        plot_data <- objects$omicsData$e_data[-rmv_col]

        disabled <- apply(is.na(plot_data), 1, any)

        out <- pickerInput(
          "NMR_picker_reference",
          div(
            "Select reference standard identifier:",
            div(
              style = "color:deepskyblue;display:inline-block",
              tipify(
                icon("question-sign", lib = "glyphicon"),
                title = ttext_[["REFERENCE_DISABLED_ROW"]]
              )
            )
          ),
          choices = as.character(choices),
          options = pickerOptions(maxOptions = 1),
          choicesOpt = list(disabled = disabled),
          selected = isolate(input$NMR_picker_reference),
          multiple = TRUE
        )

        return(
          out
        )
      }
    })

    output[["NMR_ref_done_idcols_UI"]] <- renderUI({
      
      cond_disable_apply <- is.null(input$NMR_picker_reference) ||
        get_nmr_norm(objects$omicsData) ||
        input$NMR_picker_reference %in% c(
          "Only applicable with reference normalization",
          "Please select a reference source",
          "Please upload Group file"
        )
      
      cond_disable_reset <- !get_nmr_norm(objects$omicsData)

      apply_button <- bsButton(
        "NMR_ref_done_idcols",
        style = "primary",
        div(
          "Apply reference normalization",
          icon("ok-sign", lib = "glyphicon")
        )
      )
      
      reset_button <- bsButton("ref_reset", 
                               "Reset reference normalization", 
                               style = "primary")
      
      if(cond_disable_apply) apply_button <- disabled(apply_button)
      if(cond_disable_reset) reset_button <- disabled(reset_button)
      
      return(
        div(class = "inline-wrapper-1 inline-btn-margin",
            apply_button,
            reset_button,
            hidden(
              div(
                "Normalizing, please wait...",
                id = "ref_norm_busy", class = "fadein-out",
                style = "color:deepskyblue;font-weight:bold;margin-bottom:5px"
              )
            )
        )
      )
    })
  } else {
    ## Upload
    output[["Isobaric_fdata_upload_UI"]] <- renderUI({

      if(get_data_scale(objects$omicsData) == "abundance"){
        out <- list(
          disabled(radioGroupButtons(
            paste0(tabname, "_ref_samps"),
            "Does intensity data contain reference samples?",
            choices = c("Yes", "No"),
            selected = "No"
          )),
          br(),
          strong("Data must be on the log-scale for reference normalization.")
        )
      } else {
        out <- list(
          radioGroupButtons(
            paste0(tabname, "_ref_samps"),
            "Does intensity data contain reference samples?",
            choices = c("Yes", "No"),
            selected = character(0)
          )
        )
      }
      
      do.call(tagList, out)
      
      
    })
    
    output[["Isobaric_ref_group_UI"]] <- renderUI({
      
      label_div <- div(
        class="inline-wrapper-1", 
        "Select reference group column",
        tipify(blueq, title = ttext_[["REF_GROUP_INFO"]])
      )
      
      if (is.null(is.null(input$Isobaric_ref_samps) || input$Isobaric_ref_samps == "No")) {
        return(
          disabled(pickerInput(paste0(tabname, "_ref_group"),
                               label = label_div,
                               choices = c("Only applicable with reference normalization", "placeholder")
          ))
        )
      } else {
        return(
          pickerInput(paste0(tabname, "_ref_group"),
                      label = label_div,
                      choices = colnames(f_data()),
                      multiple = TRUE,
                      selected = input[[paste0(tabname, "_ref_group")]],
                      options = pickerOptions(maxOptions = 1),
                      choicesOpt = list(
                        disabled = colnames(f_data()) %in% c(
                          objects$omicsData$f_data_cname,
                          input[[paste0(tabname, "_ref_col")]]
                        )
                      )
          )
        )
      }
    })
    output[["Isobaric_ref_col_UI"]] <- renderUI({
      label_div <- div(
        class="inline-wrapper-1", 
        "Select column designating reference samples",
        tipify(blueq, title = ttext_[["REF_SAMP_INDICATOR_COL_INFO"]])
      )
      
      if (is.null(input$Isobaric_ref_samps) || input$Isobaric_ref_samps == "No") {
        return(
          disabled(pickerInput(paste0(tabname, "_ref_col"),
                               label = label_div,
                               choices = c("Only applicable with reference normalization", "placeholder")
          ))
        )
      } else {
        return(
          pickerInput(paste0(tabname, "_ref_col"),
                      label = label_div,
                      choices = colnames(f_data()),
                      multiple = TRUE,
                      selected = input[[paste0(tabname, "_ref_col")]],
                      options = pickerOptions(maxOptions = 1),
                      choicesOpt = list(
                        disabled = colnames(f_data()) %in% c(
                          objects$omicsData$f_data_cname,
                          input[[paste0(tabname, "_ref_group")]]
                        )
                      )
          )
        )
      }
    })
    output[["Isobaric_ref_notation_UI"]] <- renderUI({
      label_div <- div(
        class="inline-wrapper-1", 
        "Specify reference sample indicator",
        tipify(blueq, title = ttext_[["REF_SAMP_INDICATOR_SYMBOL_INFO"]])
      )
      
      if (is.null(input$Isobaric_ref_samps) || input$Isobaric_ref_samps == "No" ||
                 is.null(input[[paste0(tabname, "_ref_col")]])
      ) {
        return(
          disabled(pickerInput(paste0(tabname, "_ref_notation"),
                               label = label_div,
                               choices = c("Reference column required", "placeholder")
          ))
        )
      } else {
        return(
          pickerInput(paste0(tabname, "_ref_notation"),
                      label = label_div,
                      choices = unique(
                        as.character(
                          f_data()[[input[[paste0(tabname, "_ref_col")]]]]
                        )
                      ), # Loads from selected column
                      multiple = TRUE,
                      selected = input[[paste0(tabname, "_ref_notation")]],
                      options = pickerOptions(maxOptions = 1)
          )
        )
      }
    })
    output[["Isobaric_ref_done_idcols_UI"]] <- renderUI({

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
      
      cond_disable_reset <- is.null(pmartR::get_isobaric_norm(objects$omicsData)) || !pmartR::get_isobaric_norm(objects$omicsData)
      
      apply_button <- bsButton(
        paste0(tabname, "_ref_done_idcols"),
        style = "primary",
        div(
          "Apply reference normalization",
          icon("ok-sign", lib = "glyphicon")
        )
      )
      
      reset_button <- bsButton("ref_reset", "Reset reference normalization", style = "primary")
        
      if(cond_disable_apply) apply_button <- disabled(apply_button)
      if(cond_disable_reset) reset_button <- disabled(reset_button)
      
      return(div(class = "inline-wrapper-1 inline-btn-margin", apply_button, reset_button))
      
    })
  }

  ### Outputs ###

  ## Data preview
  # Fdata
  output[[paste0(tabname, "_ref_head_fdata_UI")]] <- renderUI({

    if (is.null(f_data())
        ) {
      return(textOutput(paste0(tabname, "_no_fdata")))
    } else {
      return(DTOutput(paste0(tabname, "_head_fdata")))
    }
  })

  output[[paste0(tabname, "_head_fdata")]] <- renderDT(
    {
      

      f_data()
    },
    options = list(dom = "tip", scrollX = TRUE),
    selection = "none"
  )

  output[[paste0(tabname, "_no_fdata")]] <- renderText("No Sample Information loaded")

  # Edata
  output[[paste0(tabname, "_ref_head_edata_UI")]] <- renderUI({
    

    if (is.null(objects$omicsData$e_data)) {
      return(textOutput(paste0(tabname, "_ref_no_edata")))
    } else {
      return(DTOutput(paste0(tabname, "_ref_head_edata")))
    }
  })

  output[[paste0(tabname, "_ref_head_edata")]] <- renderDT(
    {
      
      if(inherits(objects$omicsData, "nmrData")){
        cond <- attr(objects$omicsData, "nmr_info")$norm_info$is_normalized
      } else cond <- get_isobaric_norm(objects$omicsData)
      
      req(is.null(cond) || !cond , cancelOutput = T)
      
      objects$omicsData$e_data
    },
    options = list(dom = "tip", scrollX = TRUE),
    selection = "none"
  )

  output[[paste0(tabname, "_ref_no_edata")]] <- renderText("No Data File loaded")

  # Norm Edata
  output[[paste0(tabname, "_norm_edata_UI")]] <- renderUI({
    

    if(inherits(objects$omicsData, "nmrData")){
      cond <- attr(objects$omicsData, "nmr_info")$norm_info$is_normalized
    } else cond <- get_isobaric_norm(objects$omicsData)
    
    if (is.null(cond) || !cond ) {
      return(textOutput(paste0(tabname, "_no_norm")))
    } else {
      return(DTOutput(paste0(tabname, "_norm_edata")))
    }
  })

  output[[paste0(tabname, "_norm_edata")]] <- renderDT(
    {
      

      objects$omicsData$e_data
    },
    options = list(dom = "tip", scrollX = TRUE),
    selection = "none"
  )

  output[[paste0(tabname, "_no_norm")]] <- renderText("Normalization has not been applied")

  ## Box plots

  # Prior to normalization
  output[[paste0(tabname, "_ref_upload_bp")]] <- renderUI({
    
    if (is.null(objects$omicsData$e_data)) {
      out <- list(
        textOutput(paste0(tabname, "_ref_no_bp_prior"))
      )
    } else {
      out <- list(
        # withSpinner( ### Messes up keeping prior plot form whatever reason
          plotlyOutput(paste0(tabname, "_ref_upload_boxplots_prior"))
          # )
      )
    }

    do.call(tagList, out)
  })

  output[[paste0(tabname, "_ref_no_bp_prior")]] <- renderText("No data loaded")

  # Slightly chubs cause it renders differently based on reference standard selection
  output[[paste0(tabname, "_ref_upload_boxplots_prior")]] <- renderPlotly({
    
    if(tabname == "NMR"){
      cond <- get_nmr_norm(objects$omicsData)
    } else cond <- get_isobaric_norm(objects$omicsData)
    
    req(is.null(cond) || !cond , cancelOutput = T)
    e_data <- objects$omicsData$e_data
    e_data_cname <- get_edata_cname(objects$omicsData)
    plot_data <- melt(e_data, id = e_data_cname, na.rm = TRUE)
    
    transform <- get_data_scale( objects$omicsData)

    title <- paste0("Transformed Upload: ", tabname, " Data, ", transform)

    p <- plot_ly(
      data = plot_data,
      x = plot_data$variable,
      y = plot_data$value,
      colors = I("deepskyblue1"),
      type = "box",
      name = paste0(tabname, " Data")
    ) %>%
      layout(
        title = title,
        xaxis = list(title = "Samples"),
        yaxis = list(title = "Values")
      )

    ## Generate plots w/ references
    if (tabname == "NMR" &&
        !is.null(input$NMR_ref_samps) &&
        length(input$NMR_reference_source) > 0 &&
        input$NMR_ref_samps == "Yes" &&
        !is.null(input$NMR_picker_reference) &&
        input$NMR_picker_reference != "Please upload Group file"
    ) {
      if (input$NMR_reference_source == "f_data") {
        
        expected_cols <- colnames(objects$omicsData$e_data)[
          !(colnames(objects$omicsData$e_data) %in%
              get_edata_cname(objects$omicsData))
        ]


        fdata_col_index <- map_int(1:ncol(objects$omicsData$f_data), function(col){
          sum(objects$omicsData$f_data[[col]] %in% colnames(objects$omicsData$e_data))
        })
        
        fdata_cname <- colnames(objects$omicsData$f_data)[which(fdata_col_index == max(fdata_col_index))]
        
        # sampleid <- get_fdata_cname(objects$omicsData)

        req(!is.null(input$NMR_picker_reference) && !is.null(fdata_cname))
        reference_vals <- objects$omicsData$f_data[c(input$NMR_picker_reference, fdata_cname)]

        # Set range of extra points to be between quantiles
        axis_1_75 <- as.numeric(quantile(plot_data$value, .75, na.rm = T))
        axis_1_25 <- as.numeric(quantile(plot_data$value, .25, na.rm = T))
        axis_1_range <- range(plot_data$value, na.rm = T)
        diff_low <- (axis_1_25 - axis_1_range[1]) / diff(axis_1_range)
        diff_high <- (axis_1_range[2] - axis_1_75) / diff(axis_1_range)
        diff_mid <- diff(c(axis_1_25, axis_1_75)) / diff(axis_1_range)

        axis_2_var_range <- range(reference_vals[[1]], na.rm = T)
        axis_2_range_diff <- diff(axis_2_var_range) * 1 / diff_mid # should equal diff mid in percent of total plot
        size_y2 <- c(
          axis_2_var_range[1] - diff_low * axis_2_range_diff,
          diff_high * axis_2_range_diff + axis_2_var_range[2]
        )

        # range_per <- (median(plot_data$value) - range(plot_data$value)[1])/(range(plot_data$value)[2] - range(plot_data$value)[1])
        #
        # size_y2 <- median(reference_vals[[1]]) + median(reference_vals[[1]])*c(range_per*-1, (1-range_per)*.95)
        #
        ay <- list(
          tickfont = list(color = "black"),
          overlaying = "y",
          side = "right",
          title = input$NMR_picker_reference,
          range = size_y2
        )

        p <- p %>% add_markers(
          data = reference_vals,
          y = as.numeric(reference_vals[[1]]),
          x = reference_vals[[2]],
          name = input$NMR_picker_reference,
          yaxis = "y2"
        ) %>%
          # add_lines(y = median(plot_data$value),  ## For aprox. median line up
          #           x = plot_data$variable) %>%
          # add_lines(y = median(reference_vals[[1]]),
          #           x = reference_vals[[2]],
          #           name = "median conc",
          #           yaxis = "y2") %>%
          layout(
            yaxis2 = ay,
            legend = list( # orientation = "h",   # show entries horizontally
              # yanchor = "center",  # use center of legend as anchor
              # y = 1.025,
              xanchor = "center", # use center of legend as anchor
              x = 1.25
            )
          )

        updateTabsetPanel(session, paste0(tabname, "_ref_out_tabset"), selected = "prior")
      } else {
        reference_vals <- plot_data[plot_data[[1]] == input$NMR_picker_reference, ]

        p <- p %>%
          add_markers(
            data = reference_vals,
            y = as.numeric(reference_vals$value),
            x = reference_vals$variable,
            name = input$NMR_picker_reference
          ) %>%
          layout(legend = list(
            xanchor = "center", # use center of legend as anchor
            x = 1.25
          ))
      }
    } else if (tabname == "Isobaric" &&
               !is.null(input$Isobaric_ref_col) &&
               !is.null(input$Isobaric_ref_group) &&
               !is.null(input$Isobaric_ref_notation)
    ) {
      
      
      ## inputs
      selection <- input$datatype
      edata <- e_data()
      edata_cname <- input$id_col
      emeta <- revals$e_meta
      emeta_cname <- input$protein_column
      fdata <- f_data()
      data_scale <- input$data_scale
      transform <- input$transform
      norm_info <- as.logical(as.integer(input$normalized_yn))
      na_replace <- input$na_symbol
      fdata_cname <- get_fdata_cname(objects$omicsData)
      
      temp <- as.isobaricpepData(
        e_data = edata, e_meta = emeta, f_data = fdata,
        edata_cname = edata_cname, emeta_cname = emeta_cname, fdata_cname = fdata_cname,
        data_scale = data_scale, norm_info = list(is_normalized = norm_info)
      ) %>%
        edata_replace(na_replace, NA)
      
      if(!("none" %in% c(data_scale, transform)) && data_scale != transform){
        temp <- edata_transform(temp, data_scale = transform)
      }
      
      e_data <- temp$e_data
      e_data_cname <- get_edata_cname(temp)
      plot_data <- melt(e_data, id = e_data_cname, na.rm = TRUE)
      
      title <- paste0("Transformed Upload: ", tabname, " Data, ", transform)
      
      fdata_col_index <- map_int(1:ncol(temp$f_data), function(col){
        sum(f_data()[[col]] %in% colnames(temp$e_data))
      })
      
      fdata_cname <- colnames(f_data())[which(fdata_col_index == max(fdata_col_index))]
      
      plot_data <- left_join(
        plot_data, f_data(),
        c("variable" = fdata_cname)
      )

      plot_data <- plot_data[order(plot_data[[input$Isobaric_ref_col]]), ]
      plot_data <- plot_data[order(plot_data[[input$Isobaric_ref_group]]), ]
      plot_data$variable <- factor(plot_data$variable, levels = unique(plot_data$variable))

      non_ref <- plot_data[[input$Isobaric_ref_col]] != input$Isobaric_ref_notation
      plot_data[[input$Isobaric_ref_col]][non_ref] <- ""
      plot_data[[input$Isobaric_ref_col]][!non_ref] <- "Reference"

      fillcolr <- paste(plot_data[[input$Isobaric_ref_group]], plot_data[[input$Isobaric_ref_col]])

      p <- plot_ly(
        data = plot_data,
        x = plot_data$variable,
        y = plot_data$value,
        fillcolor = gsub(".+ Reference$", "Reference", fillcolr),
        type = "box"
      ) %>%
        layout(
          title = title,
          xaxis = list(title = "Samples"),
          yaxis = list(title = "Values")
        )
    }

    isolate(plots[[tabname]][[paste0(tabname, "_Reference_upload_boxplots_prior")]] <- p)


    return(p)
  })

  # Reference Normalized
  output[[paste0(tabname, "_ref_norm_bp")]] <- renderUI({
    

    if(inherits(objects$omicsData, "nmrData")){
      cond <- attr(objects$omicsData, "nmr_info")$norm_info$is_normalized
    } else cond <- get_isobaric_norm(objects$omicsData)
    
    
    if (is.null(cond) || !cond) {
      out <- list(
        textOutput(paste0(tabname, "_ref_no_bp_post"))
      )
    } else {
      out <- list(
        withSpinner(plotlyOutput(paste0(tabname, "_ref_upload_boxplots_post")))
      )
    }

    do.call(tagList, out)
  })

  output[[paste0(tabname, "_ref_no_bp_post")]] <- renderText("Normalization has not been applied")

  
  output$warnings_reference <- renderUI({
    HTML(paste(revals$warnings_reference, collapse = ""))
  })
  
  output[[paste0(tabname, "_ref_upload_boxplots_post")]] <- renderPlotly({
    

    if(inherits(objects$omicsData, "nmrData")){
      cond <- get_nmr_norm(objects$omicsData)
    } else{
      cond <- get_isobaric_norm(objects$omicsData)
    }
    
    req(!is.null(cond) && cond)
    
    e_data <- objects$omicsData$e_data
    e_data_cname <- get_edata_cname(objects$omicsData)
    plot_data <- melt(e_data, id = e_data_cname, na.rm = TRUE)

    title <- paste0("Reference Normalized (Post-Transformed) Upload: ", tabname, " Data")

    p <- plot_ly(
      data = plot_data,
      x = plot_data$variable,
      y = plot_data$value,
      colors = I("deepskyblue1"),
      type = "box"
    ) %>%
      layout(
        title = title,
        xaxis = list(title = "Samples"),
        yaxis = list(title = "Values")
      )

    updateTabsetPanel(session, paste0(tabname, "_ref_out_tabset"), selected = "post")
    isolate(plots[[tabname]][[paste0(tabname, "_Reference_upload_boxplots_post")]] <- p)


    return(p)
  })
}
