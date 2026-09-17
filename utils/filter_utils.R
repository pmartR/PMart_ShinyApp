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
