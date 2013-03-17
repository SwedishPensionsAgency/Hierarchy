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
#' @param to_levels if to convert ids to levels
#' @param by_child if all calculations should be done on the children of the id node.
#' Return column "root" will contain the (first) label of the children.
#' @param grand_label label to be used for grand calculations (e.g. total sum). Default: "(all)"
#' @param cast_col dcast column
#' @param formula dcast forumula
#' @param margins dcast margins; NULL means no margins
#' @param sort_cols set to FALSE if columns shouldnt be reordered (only used with dcast)
#' @param ... arguments passed to the descendants_ids() function; start = where to start in the subtree, end = where to end in the subtree.
#' 
#' @examples
#' \dontrun{
#' x <- melt(notes)
#' aggr_by(x, ids = "1.1.1.2.2.1.3.1", path = "Id", labels = "Label", dims = c("variable"), metrics = c("value"), include = FALSE, end = 1, by_child = FALSE, id_format = "stars")
#' aggr_by(x, ids = "1.1.1.2.2.1.3.1", path = "Id", labels = "Label", dims = c("variable"), metrics = c("value"), include = TRUE, end = 1, by_child = TRUE, id_format = "stars")
#' aggr_by(x, ids = "1.1.1.2.2.1.3.1", path = "Id", dims = c("variable"), metrics = c("value"), include = TRUE, end = 2, labels = "Label", by_child = TRUE, id_format = "levels")
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
                    fun = sum_aggr_na, 
                    to_levels = FALSE,
                    by_child = FALSE,
                    grand_label = "(all)",
                    cast_col = ifelse(by_child, "root", dims[1]),
                    formula = paste(path, "+", paste(labels, collapse = " + "), "~", cast_col),
                    margins = cast_col,
                    sort_cols = FALSE,
                    ...) {

    # Only select/keep variables of interest
    data <- subset(data, select = c(path, labels, dims, metrics))

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
        
        # Replace label with "(all)"
        if (include) {
            if (is.factor(df[[labels[1]]])) {
                df[[labels[1]]] <- as.character(df[[labels[1]]])  # factor -> character due to issue #11
            }
            df[[labels[1]]][df[[path]] == x] <- grand_label
        }

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
    
    # If by_child then convert ids to stars; to match rows
    if (by_child) {
        escaped_ids <- gsub("\\.", "\\\\.", ids)  # TODO: Improve
        res[[path]] <- gsub(sprintf("(^%1$s%2$s)(\\w+)(.*$)", escaped_ids, "\\."),"\\1*\\3", res[[path]])
    }
    
    # If formula is not null, then use cast
    if (!is.null(formula)) {

        # Keep column sorting key (is there a way to not order/sort columns in dcast?)
        column_sort_key <- as.character(unique(res[[cast_col]]))
        
        # Cast data
        res <- dcast(res, formula, value.var = metrics, margins = margins, fill = NA_real_)
        
        # If one wants to keep original column order
        if (!sort_cols) {
            if (is.null(margins)) {
                res <- res[ , c(path, labels, column_sort_key)]
            } else {
                res <- res[ , c(path, labels, column_sort_key, "(all)")]
            }
        }
        
        # Rename column grand_labels
        colnames(res)[colnames(res) == "(all)"] <- grand_label
    }
    
    # If to convert to hierarchical levels
    if (to_levels) {
        res[[path]] <- id_to_levels(res[[path]])
    }
    
    return(res)
}
