# toggle display of spans panel if they are in pep/pro land and have chosen to use spans
observeEvent(c(input$top_page, input$spans_or_manual), {
  req(input$top_page == "normalization_tab")

  is_peppro <- inherits(objects$omicsData, c("pepData", "proData"))
  using_spans <- input$spans_or_manual == "spans" & is_peppro

  # show appropriate buttons/panels depending on whether spans is being (or can be) used
  toggle("spans_or_manual", anim = T, condition = is_peppro)
  toggle(selector = 'div[value="use_spans"]', condition = using_spans)
  toggle("inspect_norm", condition = !using_spans)
  toggle("use_selected_spans", condition = using_spans)
  toggle("apply_normalization", condition = using_spans)
  
  if(get_data_norm(objects$omicsData)){
    updateButton(session,"apply_normalization", label = "Update normalization")
  } else {
    updateButton(session,"apply_normalization", label = "Apply normalization")
  }

  if (using_spans) {
    updateCollapse(session, "spans_submenu", open = "use_spans")
  } else {
    updateCollapse(session, "spans_submenu", open = "choose_params")
  }
  #
})

# open dialog which allows user to specify spans parameters
observeEvent(input$spans_params, {
  los <- if (length(revals$spans_params[["los"]]) > 0) paste(revals$spans_params[["los"]], collapse = ";") else "0.05;0.1;0.2;0.3"
  ppp <- if (length(revals$spans_params[["ppp"]]) > 0) paste(revals$spans_params[["ppp"]], collapse = ";") else "0.1;0.25;0.50;0.75"
  rip <- if (length(revals$spans_params[["rip"]]) > 0) paste(revals$spans_params[["rip"]], collapse = ";") else "0.1;0.15;0.2;0.25"

  # create inputs which are disabled if the corresponding subset function is not selected in the sidebar
  cond_los <- "los" %in% input$spans_which_subset_fn
  cond_ppp_rip <- "ppp_rip" %in% input$spans_which_subset_fn
  cond_ppp <- ("ppp" %in% input$spans_which_subset_fn) & cond_ppp_rip
  cond_rip <- ("rip" %in% input$spans_which_subset_fn) & cond_ppp_rip

  tag_los <- if (cond_los) textInput("spans_los", "Top order statistics Percentage (los)", value = los) else div(class = "grey_text", disabled(textInput("spans_los", "Top order statistics Percentage (los)", value = los)))
  tag_ppp <- if (cond_ppp) textInput("spans_ppp", "Percentage present (ppp)", value = ppp) else div(class = "grey_text", disabled(textInput("spans_ppp", "Percentage present (ppp)", value = ppp)))
  tag_rip <- if (cond_rip) textInput("spans_rip", "Rank invariance p-value (rip)", value = rip) else div(class = "grey_text", disabled(textInput("spans_rip", "Rank invariance p-value (rip)", value = rip)))
  tag_ppp_rip <- if (cond_ppp_rip) uiOutput("spans_ppp_rip") else div(class = "grey_text", uiOutput("spans_ppp_rip"))

  showModal(
    modalDialog(
      tagList(
        tag_los,
        tag_ppp,
        tag_rip,
        tag_ppp_rip
      ),
      size = "l"
    )
  )
})

# dynamically render ppp_rip values
output$spans_ppp_rip <- renderUI({
  ppp_rip_text <- sprintf(
    "<b>ppp_rip values (based off element-wise combination of ppp and rip):<b/><br>%s",
    revals$spans_params[["ppp_rip"]] %>% lapply(function(el) {
      sprintf("(%s, %s)", el[1], el[2])
    }) %>% paste(collapse = " | ")
  )
  HTML(ppp_rip_text)
})

# assemble spans parameter list
observe({
  if (any(is.null(input$spans_los), is.null(input$spans_ppp), is.null(input$spans_rip))) {
    revals$spans_params <- NULL
    return(NULL)
  }

  los <- strsplit(input$spans_los, ";")[[1]] %>% as.numeric()
  ppp <- strsplit(input$spans_ppp, ";")[[1]] %>% as.numeric()
  rip <- strsplit(input$spans_rip, ";")[[1]] %>% as.numeric()

  # ppp_rip will be the sequential pairs from ppp and rip.  If one is longer, the 'extra elements will not be combined'
  inds <- 1:min(length(ppp), length(rip))
  ppp_rip <- lapply(inds, function(i) c(ppp[i], rip[i])) %>% purrr::map(as.numeric)

  params <- list("los" = los, "ppp" = ppp, "rip" = rip, "ppp_rip" = ppp_rip)
  params <- params[intersect(input$spans_which_subset_fn, c("los", "ppp", "rip", "ppp_rip"))]

  isolate(revals$spans_params <- params)
})

# warn of bad or empty parameters
observe({
  req(input$top_page == "normalization_tab")
  isolate(revals$warnings_normalize$empty_params <<- NULL)
  isolate(revals$warnings_normalize$bad_params <<- NULL)

  subset_fn <- intersect(c("los", "ppp", "rip", "ppp_rip"), input$spans_which_subset_fn)

  if (!is.null(revals$spans_params)) {
    has_elements <- sapply(subset_fn, function(el) {
      length(revals$spans_params[[el]]) > 0
    })

    are_valid <- sapply(subset_fn, function(el) {
      revals$spans_params[[el]] %>%
        unlist() %>%
        sapply(function(x) {
          isTRUE(x >= 0 & x <= 1)
        }) %>%
        {
          length(.) > 0 & all(.)
        }
    })

    if (sum(!has_elements) > 0) {
      warn_empty <- sprintf("The following subset functions did not have parameters specified:  %s", paste(subset_fn[which(!has_elements)], collapse = ", "))
      isolate(revals$warnings_normalize$empty_params <<- sprintf("<p style = 'color:red'>%s</p>", warn_empty))
    }

    if (sum(!are_valid) > 0) {
      warn_invalid_params <- sprintf("The following elements had invalid parameters specified:  %s", paste0(subset_fn[which(!are_valid)], collapse = ", "))
      isolate(revals$warnings_normalize$bad_params <<- sprintf("<p style = 'color:red'>%s</p>", warn_invalid_params))
    }
  }
})

# when spans is done, lead the user to the next collapse panel and show the plot/table
observeEvent(objects$spans_res, {
  updateCollapse(session, "spans_submenu", open = "choose_params")
  updateCollapse(session, "normalization_mainpanel", open = "spans_mainpanel")
})

# perform spans procedure.
# If params are not specified, revals$spans_params will be null and spans_procedure will run with default values.
observeEvent(input$execute_spans, {
  req(inherits(objects$omicsData, c("pepData", "proData")))

  revals$warnings_normalize$bad_spans <- NULL

  shinyjs::show("spans_busy", anim = T)
  shinyjs::disable("execute_spans")
  on.exit({
    shinyjs::enable("execute_spans")
    shinyjs::hide("spans_busy", anim = T)
  })

  objects$spans_res <- tryCatch(
    {
      spans_procedure(objects$omicsData, subset_fn = input$spans_which_subset_fn, norm_fn = input$spans_which_norm_fn, params = revals$spans_params)
    },
    error = function(e) {
      msg <- paste0("Something went wrong computing spans.  \n System error:  ", e)
      message(msg)
      revals$warnings_normalize$bad_spans <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      NULL
    }
  )
})

# disable normalization
observeEvent(c(input$subset_fn, input$norm_fn, input$los, input$ppp, input$rip), {
  objects$norm_objects$global_norm <- NULL
})


# disable or enable 'use_selected_spans' if a table row is selected
observeEvent(c(input$spans_table_rows_selected, objects$spans_res), {
  cond <- all(!is.null(objects$spans_res), length(input$spans_table_rows_selected) > 0)

  toggleState("use_selected_spans", condition = cond)
})

# update the parameter selections with those of the selected row
observeEvent(input$use_selected_spans, {
  ### updating tons of inputs
  req(!is.null(objects$spans_res))

  ind <- input$spans_table_rows_selected
  row <- objects$spans_res[ind, ]

  subset_fn <- row$subset_method
  norm_fn <- row$normalization_method
  params <- strsplit(row$parameters, ";")[[1]]

  updatePickerInput(session, "subset_fn", selected = subset_fn)
  updatePickerInput(session, "norm_fn", selected = norm_fn)

  if (subset_fn == "los") {
    updateNumericInput(session, "los", value = as.numeric(params))
  }
  else if (subset_fn == "ppp") {
    updateNumericInput(session, "ppp", value = as.numeric(params))
  }
  else if (subset_fn == "rip") {
    updateNumericInput(session, "rip", value = as.numeric(params))
  }
  else if (subset_fn == "ppp_rip") {
    updateNumericInput(session, "ppp", value = as.numeric(params[1]))
    updateNumericInput(session, "rip", value = as.numeric(params[1]))
  }
})

# disable non-relevant inputs depending on the subset function used
observeEvent(input$subset_fn, {
  param_ids <- c("los", "ppp", "rip")

  choices <- list(
    "los" = "los",
    "ppp" = "ppp",
    "rip" = "rip",
    "ppp_rip" = c("ppp", "rip"),
    "all" = "__NONE__",
    "complete" = "__NONE__"
  )

  # Toggle axes and coloring options depending on plot type
  lapply(param_ids, function(inputid) {
    toggleState(inputid, condition = inputid %in% choices[[input$subset_fn]])
    toggleCssClass(class = "grey_text", condition = !(inputid %in% choices[[input$subset_fn]]), selector = sprintf("label[for='%s']", inputid))
  })
})



# apply normalize_global with current params to objects$omicsData
observeEvent(c(input$apply_normalization, input$apply_normalization_modal), {
  req(input$top_page == "normalization_tab", any(c(input$apply_normalization, input$apply_normalization_modal) > 0))
  ####
  removeModal()

  tryCatch(
    {
      # construct parameters
      if (input$subset_fn %in% c("all", "complete")) {
        params <- NULL
      }
      else if (input$subset_fn == "los") {
        params <- list(los = input$los)
      }
      else if (input$subset_fn == "ppp") {
        params <- list(ppp = input$ppp)
      }
      else if (input$subset_fn == "rip") {
        params <- list(rip = input$rip)
      }
      else if (input$subset_fn == "ppp_rip") {
        params <- list(ppp_rip = list(ppp = input$ppp, rip = input$rip))
      }

      objects$omicsData <- normalize_global(objects$omicsData,
        subset_fn = input$subset_fn,
        norm_fn = input$norm_fn,
        backtransform = as.logical(input$backtransform),
        params = params,
        apply_norm = TRUE
      )

      if (!is.null(objects$omicsData_2)) {
        objects$omicsData_2 <- normalize_global(objects$omicsData_2,
          subset_fn = input$subset_fn,
          norm_fn = input$norm_fn,
          backtransform = as.logical(input$backtransform),
          params = params,
          apply_norm = TRUE
        )
      }

      # success modal if all is well
      showModal(
        modalDialog(
          {
            fluidRow(
              column(10,
                align = "center", offset = 1,
                tags$h4(sprintf(
                  "Your data has been normalized using subset function: (%s) with parameters: (%s) and normalization function: (%s)",
                  input$subset_fn, paste(unlist(ifelse(is.null(params), "None", params)), collapse = " | "), input$norm_fn
                )),
                hr(),
                actionButton("normalization_dismiss", "Review results", width = "75%"),
                uiOutput("goto_stats")
                )
            )
          },
          footer = NULL
        )
      )
      # be nice and open the plot panel for them
      updateCollapse(session, "normalization_mainpanel", open = "normdata_mainpanel")
      
      enable("reset_normalization")
    },
    error = function(e) {
      msg <- paste0("Something went wrong normalizing your data.  \n System error:  ", e)
      message(msg)
      revals$warnings_normalize$bad_norm_obj1 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
    }
  )
})

# reset normalization; really only needed where no normalization desired
observeEvent(input$reset_normalization, {
  disable("reset_normalization")
  makeobject()
  refnorm()
  makegroup()
  apply_filt()
})

# dismiss and move to next tabs
observeEvent(input$normalization_dismiss, removeModal())
observeEvent(input$goto_statistics, {
  updateTabsetPanel(session, "top_page", selected = "statistics_tab")
  removeModal()
})
observeEvent(input$goto_pepstats, {
  updateTabsetPanel(session, "top_page", selected = "peptide_statistics_tab")
  removeModal()
})

# get a p_value for the KW test on the normalization parameters chosen (taken from spans_procedure)
# make various plots summarizing the proposed normalization method
observeEvent(input$inspect_norm, {
  req(!is.null(objects$omicsData))

  # disable button while working...
  disable("inspect_norm")
  show("analyze_norm_busy")
  on.exit({
    enable("inspect_norm")
    hide("analyze_norm_busy")
  })
  
  # initialize parameters
  params <- switch(
    input$subset_fn,
     all = NULL,
     complete = NULL,
     los = list(los = input$los),
     ppp = list(ppp = input$ppp),
     rip = list(rip = input$rip),
     ppp_rip = list(ppp_rip = list(ppp = input$ppp, rip = input$rip))
    )

  # # initialize parameters
  # if (input$subset_fn %in% c("all", "complete")) {
  #   params <- NULL
  # }
  # else if (input$subset_fn == "los") {
  #   params <- list(los = input$los)
  # }
  # else if (input$subset_fn == "ppp") {
  #   params <- list(ppp = input$ppp)
  # }
  # else if (input$subset_fn == "rip") {
  #   params <- list(rip = input$rip)
  # }
  # else if (input$subset_fn == "ppp_rip") {
  #   params <- list(ppp_rip = list(ppp = input$ppp, rip = input$rip))
  # }

  # inspect_norm() returns a 4 element list of p_location, p_scale, loc_boxplot, scale_boxplot, norm_modal_ba_plots
  res_1 <- tryCatch(
    {
      inspect_norm(objects$omicsData, subset_fn = input$subset_fn, norm_fn = input$norm_fn, params = params)
    },
    error = function(e) {
      msg <- paste0("Something went wrong inspecting your omicsData object.  \n System error:  ", e)
      message(msg)
      revals$warnings_normalize$bad_inspection_obj1 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      NULL
    }
  )

  if (!is.null(objects$omicsData_2)) {
    res_2 <- tryCatch(
      {
        inspect_norm(objects$omicsData_2, subset_fn = input$subset_fn, norm_fn = input$norm_fn, params = params)
      },
      error = function(e) {
        msg <- paste0("Something went wrong inspecting your second omicsData object.  \n System error:  ", e)
        message(msg)
        revals$warnings_normalize$bad_inspection_obj2 <<- sprintf("<p style = 'color:red'>%s</p>", msg)
        NULL
      }
    )

    extra_text <- " (Dataset 1)"

    location_msg_2 <- tags$b(tags$h4(sprintf("P-value from Kruskal-Wallis test on location parameters (Dataset 2):  %s", round(res_2$p_location, 3))))
    scale_msg_2 <- if (!is.null(res_2$p_scale)) tags$b(tags$h4(sprintf("P-value from Kruskal-Wallis test on scale parameters (Dataset 2):  %s", round(res_2$p_scale, 3)))) else NULL
  }
  else {
    location_msg_2 <- scale_msg_2 <- NULL
    res_2 <- NULL
    extra_text <- ""
  }

  if (!is.null(res_1)) {
    # store plots
    plots$loc_boxplot <- res_1$loc_boxplot
    plots$scale_boxplot <- res_1$scale_boxplot
    plots$loc_boxplot_2 <- res_2$loc_boxplot
    plots$scale_boxplot_2 <- res_2$scale_boxplot
    plots$norm_modal_ba_plots <- res_1$norm_modal_ba_plots
    plots$norm_modal_ba_plots_2 <- res_2$norm_modal_ba_plots

    # tags displaying p-values
    location_msg <- tags$b(tags$h4(sprintf("P-value from Kruskal-Wallis test on location parameters%s:  %s", extra_text, round(res_1$p_location, 3))))
    scale_msg <- if (!is.null(res_1$p_scale)) tags$b(tags$h4(sprintf("P-value from Kruskal-Wallis test on scale parameters%s:  %s", extra_text, round(res_1$p_scale, 3)))) else NULL

    text <- ifelse(get_data_norm(objects$omicsData), "Update", "Apply")
    
    # conditional message/button name depending on if we have a low p-value
    if (any(c(res_1$p_location, res_1$p_scale, res_2$p_location, res_2$p_scale) < 0.05)) {
      proceed_msg <- tagList(tags$b(style = "color:red", "Low p-values suggest your normalization factors are related to a variable of interest.  This may skew your results, consider choosing another method."), hr())
      button_name <- paste(text, " normalization anyway", collapse ="")
    }
    else {
      button_name <- paste(text, " normalization", collapse = "")
      proceed_msg <- NULL
    }

    # display the modal which will warns of low p-values and gives option to apply normalization
    showModal(
      modalDialog(
        location_msg,
        scale_msg,
        location_msg_2,
        scale_msg_2,
        hr(),
        proceed_msg,
        radioGroupButtons("norm_modal_plot_select", "Show me plots of:",
          choices = c("Before and after normalization" = "ba", "Boxplots of normalization factors by group" = "fac")
        ),
        uiOutput("norm_modal_mainplot"),
        footer = tagList(
          div(
            style = "float:left",
            bsButton("apply_normalization_modal", button_name, style = "primary")
          ),
          modalButton("Choose another normalization")
        ),
        size = "l"
      )
    )
  }
})
