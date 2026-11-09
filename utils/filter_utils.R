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

#' @details Check whether a moleculeFilt object satisfies the requirement of the
#' selected batch correction method. Checks the filter object itself rather than
#' attr(omicsData, "filters"), since pmartR records nothing there when the
#' molecule filter removes no biomolecules.
has_batch_correction_filter <- function(molfilt, batch_correction_id) {
  if (is.null(batch_correction_id)) return(TRUE)
  if (length(batch_correction_id) != 1L || is.na(batch_correction_id)) return(FALSE)
  if (!batch_correction_id %in% c("ComBat", "EigenMS")) return(TRUE)

  required_attribute <- switch(
    as.character(batch_correction_id),
    ComBat = "use_batch",
    EigenMS = "use_groups"
  )

  inherits(molfilt, "moleculeFilt") && isTRUE(attr(molfilt, required_attribute))
}

cv_filter_all_singletons <- function(omicsData) {
  group_df <- attr(omicsData, "group_DF")
  if (is.null(group_df) || !"Group" %in% names(group_df)) return(FALSE)

  group_sizes <- table(group_df$Group)
  length(group_sizes) > 0 && all(!is.na(group_sizes) & group_sizes == 1)
}

cv_filter_use_groups <- function(omicsData, requested = TRUE) {
  requested <- if (is.null(requested)) TRUE else isTRUE(as.logical(requested))
  !cv_filter_all_singletons(omicsData) && requested
}
