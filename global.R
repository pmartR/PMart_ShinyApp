enableBookmarking(store = "server")

library(readr)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(pmartR)
library(dplyr)
library(purrr)
library(shinyjs)

######## GLOBAL VALUES ##########
# static objects
filter_names <- read.csv("./filter_names.csv", stringsAsFactors = F)
dt_checkmark <- '<span class="glyphicon glyphicon-ok" style="color:deepskyblue"></span>'
dt_minus <- '<span class="glyphicon glyphicon-minus"></span>'

ttext_ <- list(
  "MAIN_EFFECTS_INFO" = "Main effects are the variables of interest, those values across which you want to make comparisons.  Covariates are those values that are not of experimental interest, but must be controlled for in the analysis.",
  "RMD_PROP_MISSING_WARNING" = "We advise against using proportion missing as a metric in lipidomics/metabolomics data, as they often have a very low proportion missing."
) 

global_input_choices = list(
  "RMD_FILTER_CHOICES" = list("Median Absolute Distance"="MAD", 
                              "Kurtosis", 
                              "Skewness", 
                              "Correlation", 
                              "Proportion Missing" = "Proportion_Missing")
)

# global observers, modal UI elements, helper functions
source("UI_helper_functions.R", local = TRUE)
source("helper_functions.R", local = TRUE)

# source all resources
for (res_folder in c("tabs_UI")) {
  for (f in Sys.glob(sprintf("./%s/*.R", res_folder))) {
    source(f, local = TRUE)
  }
}
