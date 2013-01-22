setClass(
    Class = "Hierarchy",
    representation = representation(
        data = "data.frame", 
        id = "character", 
        labels = "character", 
        dimensions = "character", 
        metrics = "character"),
    validity = function(object){
        # Todo: Add validation tests
        return(TRUE)
    }
)

#' Class initializor
#' 
#' Method that runs when a new hierarchical object is initialized.
#' 
setMethod (
    f = "initialize",
    signature = "Hierarchy",
    definition = function(.Object, data, id, labels = NULL, dimensions = NULL, metrics = NULL) {

        .Object@data <- data
        .Object@id <- id
        
        if (!is.null(labels)) .Object@labels <- labels
        if (!is.null(dimensions)) .Object@dimensions <- dimensions
        if (!is.null(metrics)) .Object@metrics <- metrics
        
        validObject(.Object)
        return(.Object)
    }
)

#' Create new Hierarchy object
#' 
#' Method to create a new hierarchical object.
#' 
#' @param data Data frame or data table
#' @param id Name of id column
#' @param labels Name of label columns
#' @param dimensions Name of dimension columns (e.g. year, month)
#' @param metrics Name of metric columns
#' 
#' @import data.table
#' @export
#' 
Hierarchy <- function(...) {
    new(Class = "Hierarchy", ...)
}

#' Subset "["-method (overloading)
#' 
setMethod(
    f = "[",
    signature = "Hierarchy",
    definition = function(x, i){
        subs(x, i)
        # TODO: add "dims" argument; which subsets on chosen dimensions
    }
)

#' Aggregate
#' 
#' ...
#' 
#' @export
setGeneric("aggr", function(object, sum_all = "logical"){ standardGeneric("aggr") })
setMethod(
    f = "aggr", 
    signature = "Hierarchy",
    definition = function(object, sum_all = FALSE) {
        if (sum_all) {
            x <- sapply(object@metrics, function(x) sum(object@data[[x]], na.rm = TRUE))
        } else {
            ids <- get_id(object)
            x <- do.call("rbind", lapply(ids, function(x) aggr(object[x], sum_all = TRUE)))
            rownames(x) <- ids
        }
        return(x)
        # TODO: add "levels" argument; similar to cast
    }
)

#' Subset
#' 
#' ...
#' 
#' @export
#' 
setGeneric("subs", function(object, id = "character"){ standardGeneric("subs") })
setMethod(
    f = "subs",
    signature = "Hierarchy",
    definition = function(object, id) {
        pattern <- sprintf("^%s", gsub("\\*", "\\\\w", id))
        object@data <- object@data[grepl(pattern, get_id(object)), ]
        return(object)
    }
)

#' Get id's
#' 
#' ...
#' 
#' @export
setGeneric("get_id", function(object){ standardGeneric("get_id") })
setMethod(
    f = "get_id", 
    signature = "Hierarchy",
    definition = function(object) {
        object@data[[object@id]]
    }
)
