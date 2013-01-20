setClass(
    Class = "Hierarchy",
    # dims => list / vector / data.frame??
    representation = representation(
        data = "data.frame", 
        id = "character", 
        labels = "character", 
        dimensions = "character", 
        value = "numeric"),
    validity = function(object){
        # TODO
        # - must be of equal length
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
    definition = function(.Object, hid, dims = NULL, value = NULL) {
        .Object@hid <- hid
        .Object@dims <- dims
        .Object@value <- value
        validObject(.Object)
        return(.Object)
    }
)

#' Create new Hierarchy object
#' 
#' Method to create a new hierarchical data set/structure.
#' 
#' @param hid Hierarchical id vector
#' @param name Name vector
#' @param dims Dimension vector (e.g. year, month)
#' @param value Metric value
#' 
#' @examples db <- cdb()
#' @export
#' 
cdb <- function(...) {
    new(Class = "cdb", ...)
}