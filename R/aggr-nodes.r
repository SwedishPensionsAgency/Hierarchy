#' Aggregate by
#' 
#' A wrapper function to the path enumeration class to aggregate nodes.
#' The hierarchical data set must have a path enumerated column.
#' 
#' @param data data frame
#' @param path column with path enumeration ids
#' @param metrics metric columns
#' @param ids node id (e.g. "1.2.1.3")
#' @param by column to aggregate by
#' @param fun aggregate function (e.g. sum)
#' @param ... arguments passed to descendants_ids()
#' @param to_levels if to convert ids to levels
#' 
#' @examples 
#' # aggr_by(melt(notes), ids = "1.1.1.2.2", end = 2, include = TRUE)
#' 
#' @export
aggr_by <- function(data, path = colnames(data)[1], 
                    metrics = "value", 
                    ids = "1", 
                    by = "variable", ..., 
                    fun = function(x) sum(x, na.rm = TRUE,
                    to_levels = FALSE
                    )) {
    res <- ddply(data, by, function(x) {
        a <- Hierarchy:::path_enum$new(data = x, path = path, metrics = metrics)
        a$aggregate(a$descendants_ids(ids, ...), fun)
    })

    if (to_levels) res[[path]] <- count_occ(res[[path]]

    return(res)
}
