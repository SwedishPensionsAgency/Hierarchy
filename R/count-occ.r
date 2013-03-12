#' Count occurrences
#' 
#' Count the occurrences of a character in a string
#' 
#' @param str string
#' @param chr character 
#' 
#' @examples 
#' count_occ("1.1.1.2.12")
#' 
#' @export
count_occ <- function(str, chr = "\\.") {
    unlist(lapply(strsplit(as.character(str), chr), length)) - 1
}
