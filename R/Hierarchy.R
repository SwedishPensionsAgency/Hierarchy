setClass(
    Class = "Hierarchy",
    representation = representation(
        data = "data.table", 
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
        
        if (!is.data.table(data)) {
            .Object@data <- as.data.table(data)
        } else {
            .Object@data <- data
        }
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
    }
)

#' Aggregate
#' 
#' ...
#' 
#' @export
setGeneric("aggr", function(object){ standardGeneric("aggr") })
setMethod(
    f = "aggr", 
    signature = "Hierarchy",
    definition = function(object) {
        sapply(object@metrics, function(x) sum(object@data[[x]], na.rm = TRUE))
        
        # TODO: 
        #   - Add "levels" argument
        #   - 
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
        object@data <- object@data[grepl(pattern, object@data$id)]
        return(object)
    }
)