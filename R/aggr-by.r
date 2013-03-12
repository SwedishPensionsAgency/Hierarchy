#' Aggregate nodes
#' 
#' A wrapper function to the path enumeration class, 
#' that is used to aggregate nodes by dimensions and/or children. 
#' The hierarchical data set must have a path enumerated column.
#' 
#' @param data data frame
#' @param path column with path enumeration ids
#' @param labels label columns (description of the variable).
#' @param dims dimension columns
#' @param metrics metric columns
#' @param ids node id (e.g. "1.2.1.3")
#' @param include if the node itself should be returned as a row
#' @param fun function to be used in metric calculations (e.g. sum)
#' @param id_format format of ids to "levels" or "stars"; default = "none".
#' @param by_child if all calculations should be done on the children of the id node.
#' Return column "root" will contain the (first) label of the children.
#' @param ... arguments passed to the descendants_ids() function; start = where to start in the subtree, end = where to end in the subtree.
#' 
#' @examples
#' \dontrun{
#' aggr_by(melt(notes), ids = "1.1.1.2.2.1.3.1", path = "Id", labels = "Label", dims = c("variable"), metrics = c("value"), include = FALSE, end = 1, by_child = FALSE, id_format = "stars")
#' aggr_by(melt(notes), ids = "1.1.1.2.2.1.3.1", path = "Id", labels = "Label", dims = c("variable"), metrics = c("value"), include = TRUE, end = 1, by_child = TRUE, id_format = "stars")
#' aggr_by(melt(notes), ids = "1.1.1.2.2.1.3.1", path = "Id", dims = c("variable"), metrics = c("value"), include = TRUE, end = 2, labels = "Label", by_child = TRUE, id_format = "levels")
#' }
#' 
#' @export
aggr_by <- function(data,
                    path = colnames(data)[1],
                    labels = colnames(data)[2],
                    dims = NULL,
                    metrics = "value",
                    ids = "1",
                    include = FALSE,
                    fun = function(x) sum(x, na.rm = TRUE), 
                    id_format = "none",
                    by_child = FALSE,
                    ...) {

    # Only keep variables of interest and aggregate on those
    by <- c(path, labels, dims)
    data <- subset(data, select = c(by, metrics))
    data <- ddply(data, by, colwise(fun))

    # Define aggr function
    aggr_each <- function(x) {
      a <- Hierarchy:::path_enum$new(data = x, path = path, metrics = metrics)
      
      if (by_child) {
        ids <- a$children_ids(ids)
      }
    
      res <- do.call("rbind", lapply(ids, function(x) {
        ds <- a$descendants_ids(x, include = include, ...)
        if (is.null(ds)) ds <- x
    
        df <- a$aggregate(ds, fun)
        
        # Replace label with "(all)" (TODO: Add support for factors)
        if (id_format == "stars") df[[labels[1]]][df[[path]] == x] <- "(all)"

        df$root <- a$node(x)[1, ][[labels[1]]]  # obs: the first label in labels is used

        return(df)
      }))
    }
    
    if (is.null(dims)) {
        res <- aggr_each(data)
    } else {
    res <- ddply(data, dims, function(x) {
        aggr_each(x)
    })
    }

    # Replace part of id with *
    if (id_format == "levels") {
        res[[path]] <- id_to_levels(res[[path]])
    } else if (id_format == "stars") {
        .sep <- "\\."  # TODO: Improve
        escaped_ids <- gsub("\\.", "\\\\.", ids)  # TODO: Improve
        res[[path]] <- gsub(sprintf("(^%1$s%2$s)(\\w+)(.*$)", escaped_ids, .sep),"\\1*\\3", res[[path]])
    }
        
    return(res)
}
