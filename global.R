enableBookmarking(store = "server")

library(readr)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(stringr)
library(DT)
library(pmartR)
library(dplyr)
library(purrr)
library(shinyjs)
library(shinycssloaders)
library(plotly)
library(shinyalert)

######## GLOBAL VALUES ##########
# static objects
filter_names <- read.csv("./filter_names.csv", stringsAsFactors = F)
dt_checkmark <- '<span class="glyphicon glyphicon-ok" style="color:deepskyblue"></span>'
dt_minus <- '<span class="glyphicon glyphicon-minus"></span>'

#'@details text displayed in tooltips
ttext_ <- list(
  "MAIN_EFFECTS_INFO" = "Main effects are the variables of interest, those values across which you want to make comparisons.  Covariates are those values that are not of experimental interest, but must be controlled for in the statistics.",
  "RMD_PROP_MISSING_WARNING" = "We advise against using proportion missing as a metric in lipidomics/metabolomics data, as they often have a very low proportion missing.",
  "RMD_CUSTOM_FILTER_INFO" = "If you want to filter out one or more samples based on inspection of the rMd metrics and not a p-value cutoff, note their names and use a custom sample filter from the sample filters section to remove them.",
  "ROLLUP_DISABLE_INFO" = "Your data has already been rolled up to the protein level, if you would like to revert to the peptide level, go back to the filter page and re-apply your filters (you may need to re-do other steps that happened after the filter tab.)",
  "TABDISABLE_NOT_PEP" = "Tab disabled because either you are not analyzing peptide data or your object does not exist.",
  "TABDISABLE_NOT_PEP_NO_EMETA" = "Tab disabled because either you are not analyzing peptide data, you did not include a biomolecule information file, or your object does not exist",
  "TABDISABLE_PEP_NO_EMETA" = "Tab disabled because you uploaded peptide data with no biomolecule information file.  Peptide-level statistics are still available."
) 

#'@details info text NOT displayed in tooltips.  Usually in warnings UI elements
infotext_ <- list(
  "LOG_TRANSFORM_ZEROS" = "You have selected to log transform abundance values, 
  and %s to indicate missing values.  However there are zeros in your data
  which would cause some values to be transformed to -infinity.  Either specify
  that zeros indicate missing values or replace them with NA's or the missing value
  indicator.",
  "MISSING_DATA_REPLACE" = "The value specified here will be replaced by NA in
  the data file.  If missing values are already NA in your data, then you do not 
  have to specify this field.",
  "RESET_FILTERS_WARNING" = "Re-applying filters will delete any statistics you 
  have computed.  If your data was normalized in the normalize tab, you will 
  have to re-normalize it.  Rolled-up protein data will revert to the peptide
  level."
) 

global_input_choices = list(
  "RMD_FILTER_CHOICES" = list("Median Absolute Distance"="MAD", 
                              "Kurtosis", 
                              "Skewness", 
                              "Correlation", 
                              "Proportion Missing" = "Proportion_Missing")
)

NULLSELECT_ = "__nullselect__"

# global observers, modal UI elements, helper functions
source("UI_helper_functions.R", local = TRUE)

# source all resources
for (res_folder in c("tabs_UI", "utils")) {
  for (f in Sys.glob(sprintf("./%s/*.R", res_folder))) {
    source(f, local = TRUE)
  }
}
