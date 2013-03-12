#' Path enumeration class
path_enum <- setRefClass(
    "path.enumeration",
    fields = list(.data = "data.frame", .path = "character", .metrics = "character", .sep = "character"),
    methods = list(
        initialize = function(data, path = colnames(data)[1], metrics = NULL, sep = "\\.") {
            .path <<- path  # path has to be assigned before data
            .sep <<- sep
            .data <<- data
            .metrics <<- metrics
        },
        
        # Get data and/or subset data (replaces data in path.enum object)
        data = function(..., x = .data){
                if (is.vector(x)) {
                    .data <<- base:::subset(x = node(x), ...)
                } else {
                    .data <<- base:::subset(x = x, ...)
                }
        },
        
        # Get and filter data by match
        match = function(path, re) {
            fun <- function(x) grep(sprintf(re, x, .sep), data()[[.path]])
            match_paths <- unlist(lapply(path, fun))
            match_paths <- unique(match_paths)
            data()[match_paths, ][[.path]]
        },
        
        # Check if path id exists in data
        path_exists = function(path) all(sapply(path, function(x) x %in% data()[[.path]])),
        
        # Validate path
        validate = function(path) if (!path_exists(path)) stop("path does not exist"),

        # Parent methods
        # TODO: Add ancestors function, in the same way as descendants <-> children
        parent_id = function(path) {
            validate(path)
            x <- gsub(sprintf("(^|%s)\\w*$", .sep), "", path)
            x <- if (all(x == "")) NULL else sort(unique(x[x != ""]))
            return(x)
        },
        parent = function(...) data()[data()[[.path]] %in% parent_id(...), ],
        has_parent = function(path) unlist(sapply(path, function(x) !is.null(parent_id(x)))),

        # Descendants methods (use "start" and "end" to define how deep it should go)
        descendants_ids = function(path, end = max(count_occ(data()[[.path]], .sep)), start = 1, include = FALSE) {
            validate(path)
            x <- match(path, paste("^%1$s.(\\d*.){", start, ",", end, "}$", sep = ""))
            x <- if (length(x) > 0) as.character(sort(x)) else NULL
            
            if (include) x <- c(path, x)
            
            x <- unique(x)  # temporary fix to bug (TODO: improve)
            
            return(x)
        },
        descendants = function(...) data()[data()[[.path]] %in% descendants_ids(...), ],
        has_descendants = function(path) unlist(sapply(path, function(x) !is.null(descendants_ids(x)))),
        
        # Children methods
        children_ids = function(path) descendants_ids(path, end = 1),
        children = function(...) data()[data()[[.path]] %in% children_ids(...), ],
        has_children = function(path) unlist(sapply(path, function(x) !is.null(children_ids(x)))),
        
        # End nodes (the last children of gimven nodes)
        
        # TODO : VECTORIZE VECTORIZE VECTORIZE !
        #> a$endnodes_ids(x)
        #[1] "1.1.1.1" "1.1.1.2" "1.1.1.3" "1.2.1.1" "1.2.1.2"
        #Warning message:
        #    In if (has_descendants(path)) { :
        #    the condition has length > 1 and only the first element will be used
        
        endnodes_ids = function(path) {
            if (has_descendants(path)) {
                x <- descendants_ids(path)
                x <- x[sapply(x, function(i) { !has_children(i) })]
            } else {
                x <- path
            }
            return(x)
        },
        endnodes = function(...) data()[data()[[.path]] %in% endnodes_ids(...), ],
        
        # Aggregate endnodes of given x paths
        aggregate = function(x = node_ids(), fun = function(x) sum(x, na.rm = TRUE), metrics = .metrics) {
            if (length(metrics) > 1) {
                y <- t(sapply(x, function(x) apply(subset(endnodes(x), select = metrics), 2, fun)))
            } else {
                y <- data.frame(sapply(x, function(x) apply(subset(endnodes(x), select = metrics), 2, fun)))
                colnames(y) <- metrics
            }
            rownames(y) <- NULL
            
            z <- cbind(subset(node(x), select = colnames(node(x))[!colnames(node(x)) %in% metrics]), y)
            z <- z[ , colnames(data())]
            return(z)
        },
        
        # Node
        node_ids = function() data()[[.path]],
        node = function(path) data()[node_ids() %in% path, ],
        
        # Get tree as JSON character string
        to_json = function(path) {

            node_df <- node(path)
            intent <- paste(rep("  ", count_occ(path, .sep)), collapse = "")
            
            res <- paste(
                sprintf("\n%1$s{\n", intent),
                paste(sapply(colnames(node_df), function(x, na = na) {
                        if(is.numeric(node_df[[x]])) {
                            qte <- ""
                            node_df[[x]][is.na(node_df[[x]])] <- 0  # replace NA with 0
                        } else {
                            qte <- "\""
                        }
                        
                        sprintf("%1$s\"%2$s\" : %3$s%4$s%3$s", intent, x, qte, node_df[[x]])
                    }), collapse = ",\n"),
                if(has_children(path)) sprintf(",\n%1$s\"children\" : [%2$s\n%1$s]", intent, paste(sapply(children_ids(path), to_json), collapse = ",")) else "",
                sprintf("\n%1$s}", intent),
                sep = "")
            
            return(res)
        }
    )
)