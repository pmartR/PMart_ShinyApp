# UI panel for ggplot axes options
style_UI <- function(pagename) {
  tagList(
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
