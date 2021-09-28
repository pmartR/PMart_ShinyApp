#'@details UI panel for ggplot axes options with optional prepended elements.
#'@param pagename The name of the page or the string to prepend to each input name
#'@param ... Extra shiny elements to prepend to the axes options UI.
style_UI <- function(pagename, ...) {
  tagList(
    ...,
    splitLayout(textInput(paste0(pagename, "_xlab"), "X-axis label"),
      textInput(paste0(pagename, "_ylab"), "Y-axis label"),
      numericInput(paste0(pagename, "_x_fontsize"), "X-axis font size", value = 11),
      numericInput(paste0(pagename, "_y_fontsize"), "Y-axis font size", value = 11),
      cellWidths = rep("25%", 4)
    ),
    splitLayout(numericInput(paste0(pagename, "_xangle"), "X-axis tick angle", value = NULL),
      numericInput(paste0(pagename, "_yangle"), "Y-axis tick angle", value = NULL),
      numericInput(paste0(pagename, "_x_ticksize"), "X-axis tick size", value = NULL),
      numericInput(paste0(pagename, "_y_ticksize"), "Y-axis tick size", value = NULL),
      cellWidths = rep("25%", 4)
    ),
    splitLayout(textInput(paste0(pagename, "_title"), "Title"),
      numericInput(paste0(pagename, "_title_fontsize"), "Title font size", value = 14),
      cellWidths = c("25%", "25%")
    )
  )
}

#'@details A helper to make an inline set of colorpicker inputs
#'@param cpicker_args list of lists, each sub-element being a list of arguments
#'passed to one instance of colourpicker::pickerInput
#'@param ... Extra UI elements to put at the front of the inline div
inline_cpickers <- function(cpicker_args, ...) {
  cpickers = purrr::map(cpicker_args, function(x) do.call(colourpicker::colourInput, x))
  do.call(
    div,
    c(
      list(...),
      list(class = "inline-wrapper-1"),
      cpickers
    )
  )
}

#'@details Create one button or two inline buttons that will be used to update
#'the axes styling of a one or two ggplots.
#'
#'@param pagename A prefix for input ID's, which is usually the name of the page
#'that shiny is on
#'@param two_lipids Boolean indicating if there are two lipid datasets active 
#'and thus two plots to be updated.
#'@param two_plots Boolean indicating if there are two plots for some other
#'reason than that there are two datasets.
#'@param flip_button Whether to include a checkboxgroupbutton to flip the axes
apply_style_UI <- function(pagename, two_lipids, two_plots, flip_button = FALSE) {
  if (flip_button) {
    fbtn <- div(checkboxGroupButtons(paste0(pagename, "_flip_axes"), "",
      choices = list("Flip Axes" = TRUE),
      checkIcon = list("yes" = icon("refresh", lib = "glyphicon"), "no" = icon("refresh", lib = "glyphicon"))
    ),
    style = "display:inline-block"
    )
  }
  else {
    fbtn <- NULL
  }

  if (two_lipids) {
    tagList(
      tags$b("Update style to the plots for the:"),
      div(
        bsButton(paste0(pagename, "_apply_style_plot_1"), "first dataset"),
        bsButton(paste0(pagename, "_apply_style_plot_2"), "second dataset"),
        fbtn
      )
    )
  } else if (!two_plots) {
    div(bsButton(paste0(pagename, "_apply_style_plot_1"), "Update plot style"), fbtn)
  } else if (two_plots) {
    tagList(
      tags$b("Update style to the plots for the:"),
      div(
        bsButton(paste0(pagename, "_apply_style_plot_1"), "top/left plot"),
        bsButton(paste0(pagename, "_apply_style_plot_2"), "bottom/right plot"),
        fbtn
      )
    )
  }
}

#'@details Toggle(add/remove) a tooltip on an element given some condition
#'
#'@param session The shiny session
#'@param id input id to select, if specified, will prepend '#' to form the jquery
#'@param condition boolean to indicate whether to show (T) or hide (F) the tooltip
#'@param tooltip_text string to show in the tooltip
#'@param selector jquery selector if selecting input id is not specific enough.
#'Will override the selector formed from specifying 'id'. 
#'@param position the display position of the tooltip relative to the element.
toggleTooltip <- function(session, id = NULL, condition = T, tooltip_text = "", selector = NULL, position="bottom") {
  if (!is.null(selector)) {
    jquery = selector
  } else {
    jquery = paste0("#", id)
  }
  if (condition) {
    # addTooltip(session, id, tooltip_text) # tooltip('destroy') is awful
    shinyjs::runjs(
      sprintf("$('%s').attr('data-original-title', '%s')", jquery, tooltip_text)
    )
    shinyjs::runjs(
      sprintf("$('%s').tooltip({placement:'%s'})", jquery, position)
    )
  }
  else {
    shinyjs::runjs(
      sprintf("$('%s').attr('data-original-title', null)", jquery)
    )
  }
}

#'@description Convenience function to assign a class to a tab in a shiny 
#'navbar.
#'
#'@param name The name of the navbar tab, given by the \code{value} argument in
#'\code{tabPanel}
#'@param class CSS class to apply to the tab.  Defaults to 'disabled'.
#'@param condition Logical indicating whether to enable or disable the tab.
#'TRUE = enabled, FALSE = disabled
toggleTab <- function(name, condition, class="disabled") {
  if(condition) {
    js$enableTab(name, class)
  } else js$disableTab(name, class)
}

#'@description Helper to create a text div with an icon next to it, possibly
#'hidden and with css styling applied.
subsection_header <- function(titletext, id, style, icon, hidden = T, tooltip_text = NULL) {
  if (!is.null(tooltip_text)) {
    icon <- tipify(icon, tooltip_text)
  }
  if (hidden == T) {
    div(titletext, hidden(div(id = id, style = style, icon)))
  }
  else {
    div(titletext, div(id = id, style = style, icon))
  }
}

#'@description Creates a vector of shiny inputs as strings, usually to be passed
#'to a column of a display dataframe.
buttonInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}
