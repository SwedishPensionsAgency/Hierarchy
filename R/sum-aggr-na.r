#' Aggregate sum function
#' 
#' ...
#' 
#' @param ... arguments passed to the sum function
#' 
#' 
#' @export
sum_aggr_na <- function(...) {
    if (length(x) == 1 && is.na(x)) {
        sum(..., na.rm = FALSE)
    } else {
        sum(..., na.rm = TRUE)
    }
}
