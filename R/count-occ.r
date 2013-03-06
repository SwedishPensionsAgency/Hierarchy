#' Count occurrences
#' 
#' Count the occurrences of a character in a string
#' 
#' @param str string
#' @param chr character 
#' 
#' @examples 
#' count_occ("1.1.1.2.2", ".")
#' 
#' @export
count_occ <- function(str, chr) {
    sapply(gregexpr(sprintf("[^%s*]", chr), str), length) - 1
}
