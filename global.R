enableBookmarking(store = "server")

library(readr)
library(shiny)
library(shinyBS)
library(shinybusy)
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
MAP_URL <- ifelse(Sys.getenv("MAP_URL") == "", "https://map.emsl.pnnl.gov/app/map", Sys.getenv("MAP_URL"))

# static objects
FILTER_NAMES <- read.csv("./filter_names.csv", stringsAsFactors = F, check.names = F)
dt_checkmark <- '<span class="glyphicon glyphicon-ok" style="color:deepskyblue"></span>'
dt_minus <- '<span class="glyphicon glyphicon-minus"></span>'
blueq = icon("question-sign", lib="glyphicon", style = "color:deepskyblue;")
blueexcl = icon("exclamation-sign", lib="glyphicon", style = "color:deepskyblue;")
blue_info = icon("info-sign", lib="glyphicon", style = "color:deepskyblue")

#'@details text displayed in tooltips
ttext_ <- list(
  ABUNDANCE_ZEROS_TO_NA = "Zeros will be automatically transformed to NA in raw abundance data.",
  ABUNDANCE_NAS_TO_ZEROS = "NAs will be automatically transformed to zeros in transcript count data.",
  COVARIATE_TYPE_INFO = "Suppose your covariate has values [1,1,2,2,3,3].  By default, it is treated as a factor variable, but you may want to specify it is numeric.",
  CV_FILTER_INFO = "Remove biomolecules based on minimum value of coefficient of variation, calculated per-group as the standard deviation divided by the mean, and then pooled across groups per Ahmed (1995).",
  DOWNLOAD_OPTS_DISABLED = "Currently selected parameters are the same as the ones currently stored for this plot",
  EXPLAIN_QC_BOXPLOTS = "Boxplots of abundance values for each sample.  Orderable/colorable by group or other sample information",
  EXPLAIN_QC_BARPLOTS = "Barplots of missing values for each sample.  Orderable/colorable by group or other sample information",
  EXPLAIN_QC_SCATTERPLOTS = "Scatterplots of biomolecules and their level of missingness against their median abundance.",
  EXPLAIN_QC_PCA = "Principal components analysis of abundance values.  Probabilistic Principle Components Analysis is used, which does not require imputation of missing values. The R^2 value on the axes labels correspond to the percent variance of all the data explained by the corresponding principal component.  Colorable by group or other sample information",
  IMDANOVA_FILTER_INFO = "Remove biomolecules that do not have a minimum number of non-missing values in each group to do statistical comparisons.",
  IMDANOVA_BOTH_NA = "Must choose at least one minimum observed threshold (gtest or anova)",
  IMDANOVA_NONE_FILTERED = "The chosen minimum values for the iMd filter do not remove any biomolecules, please revise.",
  IMDANOVA_VALUES_OUT_OF_RANGE = "The specified minimum number of observations per group must be less than or equal to the minimum group size: %s.  Minimum g-test must be >= 3.  Minimum ANOVA must be >=2.",
  IMD_INTERACTIVE_MANY_POINTS = "If your data has many biomolecules, creating an interactive plot may take a while.",
  MAIN_EFFECTS_INFO = "Main effects are the variables of interest, those values across which you want to make comparisons.  Covariates are those values that are not of experimental interest, but must be controlled for in the statistics.",
  MIDPOINT_EXPORT_DISABLED = "Either your omicsData has not been created, or you have not finished the Normalization tab (or Statistics tab for seqData)",
  MISSING_DATA_REPLACE = "The value specified here will be replaced by NA in the data file.  If missing values are already NA in your data or the missing values are zeros in your abundance data, then you do not have to specify this field.",
  MOLECULE_FILTER_INFO = "Remove biomolecules that do not have at least some number of nonmissing values",
  PAIRING_INFO = "If your samples are paired, three pieces of information are needed:  The column specifying which pairs of samples go together, the column specifying which group or side of the pairing each sample is in, and which group is to be subtracted from the other.",
  PROTEOMICS_FILTER_INFO = "Remove proteins (and consequently peptides) that do not have a minimum number of peptides mapping to them.  Optionally, also remove peptides that map to more than one protein (redundant peptides).  If redundant peptides are removed, they will be removed before determining whether proteins have the minimum peptide count.",
  REF_GROUP_INFO = "Reference normalization expects samples to belong to one of several several sets of samples (e.g. plexes or plates), each having a single &quot;reference&quot; sample (e.g. a sample on the plex that is a pool of the study samples on that same plex).  Indicate the columns in the sample information file that specify these sets of samples.",
  REF_SAMP_INDICATOR_COL_INFO = "Select which column in the sample information file indicates whether or not a sample is the reference sample for its corresponding sample set. For example, this column might contain only 1s and 0s, with 1s indicating a reference sample, and would have only a single 1 for each sample set (e.g. plex or plate).",
  REF_SAMP_INDICATOR_SYMBOL_INFO = "Select which value indicates a reference sample.  In the column you selected as indicating reference samples, what value in that column indicates a reference sample.  For example, you might specify that &quot;1&quot; represents a reference sample.",
  RMD_CUSTOM_FILTER_INFO = "If you want to filter out one or more samples based on inspection of the rMd metrics and not a p-value cutoff, note their names and use a custom sample filter from the sample filters section to remove them.",
  RMD_FILTER_INFO = "Identify and filter outlier samples based on their robust Mahalanobis distance, calculated based on up to 5 metrics described in Webb-Robertson et al. (2011)",
  RMD_PROP_MISSING_WARNING = "We advise against using proportion missing as a metric in lipidomics/metabolomics data, as they often have a very low proportion missing.",
  RNA_FILT_LIB_INFO = "Filter samples based on a minimum specified library size, defined as the sum of gene counts in a sample.  The RNA-seq filters are provided in case there are any samples that exhibit egregious behavior that have not already been removed. These filters are not designed for the quality control of the identification portion of the bioinformatics pipeline.",
  RNA_FILT_MIN_NONZERO_INFO = "Filter samples based on a specified minimum number of nonzero gene counts.  The RNA-seq filters are provided in case there are any samples that exhibit egregious behavior that have not already been removed. These filters are not designed for the quality control of the identification portion of the bioinformatics pipeline.",
  ROLLUP_DISABLE_INFO = "Your data has already been rolled up to the protein level, if you would like to revert to the peptide level, go back to the filter page and re-apply your filters (you may need to re-do other steps that happened after the filter tab.)",
  SEQDATA_PVAL_ADJUST = "Adjust p-values to control for multiple tests resulting from many biomolecules (as oppose to multiple comparisons within a single biomolecule).",
  TABDISABLE_NOT_PEP = "Tab disabled because either you are not analyzing peptide data or your object does not exist.",
  TABDISABLE_NOT_PEP_NO_EMETA = "Tab disabled because either you are not analyzing peptide data, you did not include a biomolecule information file, or your object does not exist",
  TABDISABLE_NOT_REF = "Tab disabled because reference normalization is only available for NMR data or labeled peptide data",
  TABDISABLE_SEQDATA = "Tab disabled because normalization is not supported for transcript data, or your object does not exist.",
  TOTAL_COUNT_FILT_INFO = "RNA-seq filter which removes transcripts that have total count across all samples less than a specified threshold.",
  REFERENCE_DISABLED_ROW = "Disabled entries contain missing values in some samples. These might be due to NAs generated in log transformation or replacement of values less than or equal to zero.",
  REFERENCE_DISABLED_COL = "Disabled entries are non-numeric.",
  SAMPLE_FILTER_CAUTION = "Removal of sample outliers should be done with caution, and ideally removal should be informed by input from the experimental scientist and/or supporting evidence from more filters than just the rMd Filter",
  TABDISABLE_PEP_NOT_ROLLED_UP = "This tab is used to perform statistics on peptide data rolled up to the protein level.  Roll up your peptide data in the Protein Roll Up tab before using this tab.  To perform stats on the peptide-level data, go to the Peptide Statistics tab.",
  TABDISABLE_PEP_NO_EMETA = "Tab disabled because you uploaded peptide data with no biomolecule information file.  Peptide-level statistics are still available.",
  WHAT_IS_MC = "This option defines p-value adjustment for multiple comparisons within a single biomolecule.  For example, if you have three groups A, B, and C and you want to test comparisons A vs B, A vs C, and B vs C, you will run adjustment on the resulting three p-values for all biomolecules.",
  WHAT_IS_FDR = "This option defines p-value adjustment for multiple tests arising from many biomolecules.  For example, if you have 100 biomolecules and some number of comparisons, you will run adjustment on the resulting 100 p-values for each comparison."
)

#'@details info text NOT displayed in tooltips.  Usually in warnings UI elements
infotext_ <- list(
  LOG_TRANSFORM_ZEROS = "You have selected to log transform abundance values, 
  and %s to indicate missing values.  However there are zeros in your data
  which would cause some values to be transformed to -infinity.  Either specify
  that zeros indicate missing values or replace them with NA's or the missing value
  indicator.",
  DATA_NOT_LOG = "Your data is not on the log scale and was not transformed to the log scale, some downstream methods may be disabled, consider recreating your data with a log-transform specified or that your data is already on the log scale.",
  "RESET_FILTERS_WARNING" = "Re-applying filters will delete any statistics you 
  have computed.  If your data was normalized in the normalize tab, you will 
  have to re-normalize it.  Rolled up protein data will revert to the peptide
  level.",
  GROUPS_IN_FDATA = "The column 'Group' was found in your sample identification file, but is reserved for PMart.  It has been rename to '%s' (lower case g)",
  REFNORM_COLUMN_INFO_1 = "In the first image below, you would select 'TMT_Plex_Number' as the reference group column (1st dropdown), 'Treatment_Group' as the column containing the indicator for reference samples (2nd dropdown), and 'ReferencePool' as the value indicating reference samples (last dropdown).  ",
  REFNORM_COLUMN_INFO_2 = "In the second example, you would select 'TMT_Plex_Number' as the reference group column (1st dropdown), 'Is_Reference_Pool' as the column containing the indicator for reference samples (2nd dropdown), and 'Yes' as the value indicating reference samples (last dropdown).  ",
  STATS_OBJECTS_COMBINED = "Your objects have been combined into a single dataset.  If navigating to previous tabs, the first object shown in plots is now the combined data."
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

# source all resources
for (res_folder in c("tabs_UI", "utils")) {
  for (f in Sys.glob(sprintf("./%s/*.R", res_folder))) {
    source(f, local = TRUE)
  }
}
