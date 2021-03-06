#' Aggregate sum function
#' 
#' ...
#' 
#' @param x data
#' 
#' 
#' @export
sum_aggr_na <- function(x) {
    if (length(x) == 1 && is.na(x)) {
        sum(x, na.rm = FALSE)
    } else {
        sum(x, na.rm = TRUE)
    }
}
