#' Path enum to levels
#' 
#' Convert a path enumeration id vector to a vector containing hierarchical levels
#' 
#' @param ids path enum vector
#' @param sep id seperator (e.g. ".")
#' 
#' @examples 
#' # id_to_levels(notes$Id)
#' 
#' @export
id_to_levels <- function(ids, sep  = "\\.") {
    count_occ(ids, sep) - min(count_occ(ids, sep)) + 1
}
