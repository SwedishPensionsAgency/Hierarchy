#' Aggregate by variable and/or children
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
#' # aggr_by_(melt(notes), ids = "1.1.1.2.2", end = 2, include = TRUE)
#' 
#' @export
aggr_by <- function(data,
                          ids = "1",
                          path = colnames(data)[1], 
                          metrics = "value",
                          dims = "variable",
                          include = FALSE,
                          fun = function(x) sum(x, na.rm = TRUE), 
                          to_levels = FALSE,
                          label = colnames(data)[1],
                          by_child = FALSE,
                          ...) {

    # Only keep variables of interest and aggregate on those
    by <- c(path, label, dims)
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
        #if (include) df[df$label == a$node(x)[1, ][[label]], ][1, ][[label]] <- "(all)"
    
        df$root <- a$node(x)[1, ][[label]]
    
        #if (to_levels) df[[path]] <- id_to_levels(df[[path]])
    
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
    .sep <- "\\."  # TODO: Improve
    escaped_ids <- gsub("\\.", "\\\\.", ids)  # TODO: Improve
    res[[path]] <- gsub(sprintf("(^%1$s%2$s)(\\w+)(.*$)", escaped_ids, .sep),"\\1*\\3", res[[path]])
        
    if (to_levels) res[[path]] <- id_to_levels(res[[path]])
    
    return(res)
}

### Example
# require(Hierarchy)
# require(plyr)
# require(reshape2)
# x <- read.table("data/notes.tab", sep = "\t", header = TRUE)
# x <- melt(x)
# test <- aggr_by(x, ids = "1.1.1.2.2.1.3.1", path = "Id", label = "Label", dims = c("variable"), metrics = c("value"), include = FALSE, end = 1, by_child = FALSE)
# test <- aggr_by(x, ids = "1.1.1.2.2.1.3.1", path = "Id", label = "Label", dims = c("variable"), metrics = c("value"), include = TRUE, end = 1, by_child = TRUE)
# test <- aggr_by(x, ids = "1.1.1.2.2.1.3.1", path = "Id", dims = c("variable"), metrics = c("value"), include = TRUE, end = 2, label = "Label", by_child = TRUE, to_levels = TRUE)
# head(test)
