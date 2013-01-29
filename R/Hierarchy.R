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
    definition = function(x, i, j = FALSE){
        # j = nested sum
        
        
        # TODO: Rewrite aggr so that I don't need all those fixes
        if (j) {
            data <- aggr(x, i)
            id <- rownames(data)
            data <- cbind(id, data)
            rownames(data) <- NULL
            x@data <- as.data.frame(data)
        } else {
            x <- subs(x, i)
        }
        return(x)

        # x <- aggr(...)
        # if is.vector(x) { x <- as.data.frame(t(x)) }
        # TODO: add "dims" argument; which subsets on chosen dimensions
    }
)

#' Aggregate
#' 
#' ...
#' 
#' @export
setGeneric("aggr", function(object, id = "character", sum_all = "logical"){ standardGeneric("aggr") })
setMethod(
    f = "aggr", 
    signature = "Hierarchy",
    definition = function(object, id = "", sum_all = FALSE) {
        if (sum_all) {
            x <- sapply(object@metrics, function(x) sum(object@data[[x]], na.rm = TRUE))
        } else {
            ids <- get_id(object)
            x <- do.call("rbind", lapply(ids, function(x) aggr(subs(object, x), sum_all = TRUE)))
            rownames(x) <- ids
            x <- data.frame(x)
            pattern <- sprintf("^%s(\\.|$)", gsub("\\.", "\\\\.", id))
            x <- x[grepl(pattern, rownames(x)), ]
        }
        
        return(x)
        
        # TODO: add "levels" argument; similar to cast
        # TODO: return as Hierarchy object instead
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
        pattern <- sprintf("^%s(\\.|$)", gsub("\\.", "\\\\.", id))
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

#' Convert Hierarchy object to a json
#' 
#' ...
#' 
#' @export
#' @import RJSONIO
#' 
setGeneric("as.json", function(object){ standardGeneric("as.json") })
setMethod(
    f = "as.json",
    signature = "Hierarchy",
    definition = function(object) {
        RJSONIO::toJSON(as.list(object))
    }
)

#' Convert Hierarchy object to a list
#' 
#' ...
#' 
#' @export
#' 
setGeneric("as.list", function(object){ standardGeneric("as.list") })
setMethod(
    f = "as.list",
    signature = "Hierarchy",
    definition = function(object) {
        lst_fun(object@data, object@id)
    }
)

#' Get the number hierarchical dimensions (occurences of "." + 1)
#' 
#' ...
#' @param x id vector
#' @param delimiter hierarchical delimiter
ch_dim <- function(x, delimiter = ".") {
    x <- as.character(x)
    chr.count <- function(x) length(which(unlist(strsplit(x, NULL)) == delimiter))
    if (length(x) > 1) {
        sapply(x, chr.count) + 1
    } else {
        chr.count(x) + 1
    }
}

#' Convert a hierarchical data.frame to a nested list
#' 
#' ...
#'
#' @param ch data frame
#' @param id_col name of id column
#' @param num start dimension
#' @param stp end dimension
#' 
lst_fun <- function(ch, id_col = "id", num = min(d), stp = max(d)) {
    
    # Convert data.frame to character
    ch <- data.frame(lapply(ch, as.character), stringsAsFactors=FALSE)
    
    # Get number of hierarchical dimensions
    d <- ch_dim(ch[[id_col]])
    
    # Convert to list
    lapply(ch[d == num,][[id_col]], function(x) {
        tt <- ch[grepl(sprintf("^%s.", x), ch[[id_col]]),]
        current <- ch[ch[[id_col]] == x,]
        if (stp != num && nrow(tt) > 0) { 
            c(current, list(children = lst_fun(tt, id_col, num + 1, stp)))
        } else { current }
    })
}
