#' Aggregate by variable
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
#' # aggr_by_var(melt(notes), ids = "1.1.1.2.2", end = 2, include = TRUE)
#' 
#' @export
aggr_by_var <- function(data, path = colnames(data)[1],
                        ids = "1",
                        metrics = "value",
                        by = "variable",
                        fun = function(x) sum(x, na.rm = TRUE),
                        to_levels = FALSE,
                        ...) {
    res <- ddply(data, by, function(x) {
        a <- Hierarchy:::path_enum$new(data = x, path = path, metrics = metrics)
        a$aggregate(a$descendants_ids(ids, ...), fun)
    })

    if (to_levels) res[[path]] <- id_to_levels(res[[path]])

    return(res)
}


# Aggregate by child

aggr_by_child <- function(data,
                          ids = "1",
                          path = colnames(data)[1], 
                          metrics = "value", 
                          include = FALSE,
                          fun = function(x) sum(x, na.rm = TRUE), 
                          to_levels = FALSE,
                          label = colnames(data)[1],
                          ...) {

  a <- Hierarchy:::path_enum$new(data = data, path = path, metrics = metrics)
  ch <- a$children_ids(ids)

  res <- do.call("rbind", lapply(ch, function(x) {

    ds <- a$descendants_ids(x, include = include, ...)

    if (is.null(ds)) ds <- x

    df <- a$aggregate(ds, fun)
    
    if (include) df[df$label == a$node(x)[1, ][[label]], ][1, ][[label]] <- "(all)"

    df$child <- a$node(x)[1, ][[label]]

    # Replace part of id with *
    .sep <- "\\."  # TODO: Improve
    escaped_ids <- gsub("\\.", "\\\\.", ids)  # TODO: Improve
    df[['Id']] <- gsub(sprintf("(^%1$s%2$s)(\\w+)(.*$)", escaped_ids, .sep),"\\1*\\3", df[['Id']])
    
    if (to_levels) df[[path]] <- id_to_levels(df[[path]])

    return(df)
  }))
}
