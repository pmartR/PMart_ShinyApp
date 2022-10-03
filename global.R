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
library(colourpicker)
library(shinycssloaders)
library(plotly)
library(shinyalert)
library(reshape2)

######## GLOBAL VALUES ##########

# Pull app version from global variable
MAP_ACTIVE <- ifelse(Sys.getenv("MAP_VERSION") == "1", TRUE, FALSE)

# static objects
FILTER_NAMES <- read.csv("./filter_names.csv", stringsAsFactors = F, check.names = F)
dt_checkmark <- '<span class="glyphicon glyphicon-ok" style="color:deepskyblue"></span>'
dt_minus <- '<span class="glyphicon glyphicon-minus"></span>'
blueq = icon("question-sign", lib="glyphicon", style = "color:deepskyblue;")
blueexcl = icon("exclamation-sign", lib="glyphicon", style = "color:deepskyblue;")

#'@details text displayed in tooltips
ttext_ <- list(
  ABUNDANCE_ZEROS_TO_NA = "Zeros will be automatically transformed to NA in raw abundance data.",
  ABUNDANCE_NAS_TO_ZEROS = "NAs will be automatically transformed to zeros in transcript count data.",
  COVARIATE_TYPE_INFO = "Suppose your covariate has values [1,1,2,2,3,3].  By default, it is treated as a factor variable, but you may want to specify it is numeric.",
  CV_FILTER_INFO = "Remove biomolecules based on minimum value of coefficient of variation, calculated per-group as the standard deviation divided by the mean, and then pooled across groups per Ahmed (1995).",
  IMD_INTERACTIVE_MANY_POINTS = "If your data has many biomolecules, creating an interactive plot may take a while.",
  MAIN_EFFECTS_INFO = "Main effects are the variables of interest, those values across which you want to make comparisons.  Covariates are those values that are not of experimental interest, but must be controlled for in the statistics.",
  MISSING_DATA_REPLACE = "The value specified here will be replaced by NA in the data file.  If missing values are already NA in your data or the missing values are zeros in your abundance data, then you do not have to specify this field.",
  MOLECULE_FILTER_INFO = "Remove biomolecules that do not have at least some number of nonmissing values",
  PAIRING_INFO = "If your samples are paired, three pieces of information are needed:  The column specifying which pairs of samples go together, the column specifying which group or side of the pairing each sample is in, and which group is to be subtracted from the other.",
  PROTEOMICS_FILTER_INFO = "Remove proteins (and consequently peptides) that do not have a minimum number of peptides mapping to them.  Optionally, also remove peptides that map to more than one protein.",
  RMD_CUSTOM_FILTER_INFO = "If you want to filter out one or more samples based on inspection of the rMd metrics and not a p-value cutoff, note their names and use a custom sample filter from the sample filters section to remove them.",
  RMD_FILTER_INFO = "Identify and filter outlier samples based on their robust Mahalanobis distance, calculated based on up to 5 metrics described in Webb-Robertson et al. (2011)",
  RMD_PROP_MISSING_WARNING = "We advise against using proportion missing as a metric in lipidomics/metabolomics data, as they often have a very low proportion missing.",
  RNA_FILT_LIB_INFO = "Filter samples based on a minimum specified library size, defined as the sum of gene counts in a sample.",
  RNA_FILT_MIN_NONZERO_INFO = "Filter samples based on a specified minimum number of nonzero gene counts",
  ROLLUP_DISABLE_INFO = "Your data has already been rolled up to the protein level, if you would like to revert to the peptide level, go back to the filter page and re-apply your filters (you may need to re-do other steps that happened after the filter tab.)",
  TABDISABLE_NOT_PEP = "Tab disabled because either you are not analyzing peptide data or your object does not exist.",
  TABDISABLE_NOT_PEP_NO_EMETA = "Tab disabled because either you are not analyzing peptide data, you did not include a biomolecule information file, or your object does not exist",
  TABDISABLE_NOT_REF = "Tab disabled because reference normalization is only available for NMR data or labeled peptide data",
  TABDISABLE_SEQDATA = "Tab disabled because normalization is not supported for transcript data, or your object does not exist.",
  TOTAL_COUNT_FILT_INFO = "RNA-seq filter which removes transcripts that have total count across all samples less than a specified threshold.",
  REFERENCE_DISABLED_ROW = "Disabled entries contain missing values in some samples. These might be due to NAs generated in log transformation or replacement of values less than or equal to zero.",
  REFERENCE_DISABLED_COL = "Disabled entries are non-numeric.",
  TABDISABLE_PEP_NOT_ROLLED_UP = "This tab is used to perform statistics on peptide data rolled up to the protein level.  Roll up your peptide data in the Protein Rollup tab before using this tab.  To perform stats on the peptide-level data, go to the Peptide Statistics tab.",
  TABDISABLE_PEP_NO_EMETA = "Tab disabled because you uploaded peptide data with no biomolecule information file.  Peptide-level statistics are still available."
) 

#'@details info text NOT displayed in tooltips.  Usually in warnings UI elements
infotext_ <- list(
  "LOG_TRANSFORM_ZEROS" = "You have selected to log transform abundance values, 
  and %s to indicate missing values.  However there are zeros in your data
  which would cause some values to be transformed to -infinity.  Either specify
  that zeros indicate missing values or replace them with NA's or the missing value
  indicator.",
  "RESET_FILTERS_WARNING" = "Re-applying filters will delete any statistics you 
  have computed.  If your data was normalized in the normalize tab, you will 
  have to re-normalize it.  Rolled-up protein data will revert to the peptide
  level."
) 

global_input_choices = list(
  RMD_FILTER_CHOICES = list("Median Absolute Distance"="MAD", 
                              "Kurtosis", 
                              "Skewness", 
                              "Correlation", 
                              "Proportion Missing" = "Proportion_Missing"),
  MISSINGVAL_COLORS = c(
    "YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges", "Greys",
    "Greens", "GnBu", "BuPu", "BuGn", "Blues", "Set3", "Set2", "Set1", "Pastel2", "Pastel1", "Paired", "Dark2", "Accent",
    "Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"
  )
)

#'@details input id's that need to be mutually exclusive in the groups tab ...
GROUPS_ME_IDS <- c(
  "fdata_id_col",
  "gcol1",
  "gcol2", 
  "cvcol1", 
  "cvcol2",
  "pair_id_col",
  "pair_group_col",
  "pair_denom_col"
)

#'@details ... and for the optional second dataset as well
GROUPS_ME_IDS_2 <- c(
  "fdata_id_col_2",
  "gcol1_2",
  "gcol2_2", 
  "cvcol1_2", 
  "cvcol2_2",
  "pair_id_col_2",
  "pair_group_col_2",
  "pair_denom_col_2"
)

NULLSELECT_ = "__nullselect__"

#'@details the data-value (html) attribute of all dropdowns
TAB_IDS <- list(
  "upload_and_datareqs",
  "group_samples_tab",
  "reference_tab",
  "data_summary_tab",
  "filter_tab",
  "normalization_tab",
  "peptide_statistics_tab",
  "protein_rollup_tab",
  "statistics_tab",
  "download_tab"
)

# global observers, modal UI elements, helper functions
source("UI_helper_functions.R", local = TRUE)

# source all resources
for (res_folder in c("tabs_UI", "utils")) {
  for (f in Sys.glob(sprintf("./%s/*.R", res_folder))) {
    source(f, local = TRUE)
  }
}
