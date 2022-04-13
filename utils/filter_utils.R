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