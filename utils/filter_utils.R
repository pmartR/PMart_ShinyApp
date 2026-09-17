#' @details Return filters of a certain type in an object and possibly the sub
#' elements of each filter.
get_filters <- function(omicsData, filter_type, element = NULL) {
  attributes(omicsData)$filters %>% 
    lapply(function(filt) {
      if(isTRUE(filt$type == filter_type)) {
        if(!is.null(element)) filt[[element]] else filt
      } else {
        NULL
      }
    })
}

cv_filter_status <- function(omicsData) {
  group_df <- attr(omicsData, "group_DF")
  if (!is.null(group_df) && "Group" %in% names(group_df)) {
    group_sizes <- table(group_df$Group)
    if (length(group_sizes) > 0 && all(group_sizes <= 1)) {
      cvfilt <- tryCatch(cv_filter(omicsData, use_groups = FALSE), error = function(e) NULL)
      if (!is.null(cvfilt) && any(is.finite(cvfilt$CV))) {
        return(list(
          available = TRUE,
          reason = "all_singletons",
          message = NULL,
          note = "Non-group CV was calculated because all groups contain a single sample.",
          filter = cvfilt
        ))
      }

      return(list(
        available = FALSE,
        reason = "all_singletons",
        message = "CV filtering is unavailable because all groups contain a single sample and non-group CVs could not be calculated.",
        note = NULL
      ))
    }
  }

  cvfilt <- tryCatch(cv_filter(omicsData), error = function(e) NULL)
  if (is.null(cvfilt)) {
    return(list(
      available = FALSE,
      reason = "cv_error",
      message = "CV filtering is unavailable because CVs could not be calculated for the non-singleton groups.",
      note = NULL
    ))
  }

  if (!any(is.finite(cvfilt$CV))) {
    return(list(
      available = FALSE,
      reason = "no_valid_cv",
      message = "CV filtering is unavailable because the non-singleton groups did not produce any finite CV values.",
      note = NULL
    ))
  }

  list(available = TRUE, reason = NULL, message = NULL, note = NULL, filter = cvfilt)
}
