# Path Enumeration

path_enum <- setRefClass(
    "path.enumeration",
    fields = list(.data = "character", .path = "character", .sep = "character"),
    methods = list(
        initialize = function(data, path = colnames(data)[1], sep = "\\.") {
            .path <<- path  # path has to be assigned before data
            .sep <<- sep
            .data <<- deparse(substitute(data))
        },
        
        # Get all data
        data = function() eval(parse(text = .data)),

        # Get and filter data by match
        #match = function(path, re) data()[grep(sprintf(re, path, .sep), data()[[.path]]), ][[.path]],  # TODO: Vectorize
        match = function(path, re) {
            fun <- function(x) grep(sprintf("^%1$s%2$s\\w*$", x, .sep), data()[[.path]])
            match_paths <- unlist(lapply(path, fun))
            match_paths <- unique(match_paths)
            data()[match_paths, ][[.path]]
        },
        
        # Check if path id exists in data
        path_exists = function(path) path %in% data()[[.path]],
        
        # Validate path
        validate = function(path) if (!path_exists(path)) stop("path does not exist"),
        
        # Find the position of the last seperator in path
        last_sep_position = function(path) max(gregexpr(.sep, path)[[1]]),
        
        # Parent methods
        parent_id = function(path) {
            validate(path)
            x <- gsub(sprintf("(^|%s)\\w*$", .sep), "", path)
            x <- if (nchar(x) > 0) as.character(x) else NULL
            return(x)
        },
        parent = function(...) data()[data()[[.path]] %in% parent_id(...), ],
        has_parent = function(path) !is.null(parent_id(path)),
        
        # Descendants methods
        descendants_ids = function(path) {
            validate(path)
            x <- match(path, "^%1$s%2$s\\w*(%2$s|$)")
            x <- if (length(x) > 0) as.character(x) else NULL
            return(x)
        },
        descendants = function(...) data()[data()[[.path]] %in% descendants_ids(...), ],
        has_descendants = function(path) !is.null(descendants_ids(path)),
        
        # Children methods
        children_ids = function(path) {
            browser()
            validate(path)
            x <- match(path, "^%1$s%2$s\\w*$") 
            x <- if (length(x) > 0) as.character(x) else NULL
            return(x)
        },
        children = function(...) data()[data()[[.path]] %in% children_ids(...), ],
        has_children = function(path) !is.null(children_ids(path)),
        
        # End nodes (the last children of given nodes)
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
        endnodes_aggregate = function(x, metrics, fun) {
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
        node = function(path) data()[data()[[.path]] %in% path, ]
        

        
    )
)