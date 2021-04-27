enableBookmarking(store = 'server')

library(readr)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(trelliscopejs)
library(pmartR)
library(dplyr)
library(purrr)

######## GLOBAL VALUES ##########
# static objects
filter_names <- read.csv("./filter_names.csv", stringsAsFactors = F)
dt_checkmark <- '<span class="glyphicon glyphicon-ok" style="color:deepskyblue"></span>'
dt_minus <- '<span class="glyphicon glyphicon-minus"></span>'
main_effects_text = 'Main effects are the variables of interest, those values across which you want to make comparisons.  Covariates are those values that are not of experimental interest, but must be controlled for in the analysis.'

