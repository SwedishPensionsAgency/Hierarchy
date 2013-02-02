# Path Enumeration

Hierarchy <- setRefClass(
    "Hierarchy",
    fields = list(.data = "character", .path = "character", .sep = "character"),
    methods = list(
        initialize = function(x, y = colnames(x)[1]) {
            
            .path <<- y  # have to be assigned before x
            .sep <<- "\\."
            
            .data <<- deparse(substitute(x))
            

        },
        
        data = function() eval(parse(text = .data)),  # get data

        match = function(path, re) {
            data()[grep(sprintf(re, path, .sep), data()[[.path]])][[.path]]
        },
        
        # Find the position of the last seperator in path
        last_sep_position = function(path) {
            max(gregexpr(.sep, path)[[1]])
        },
        
        parent_id = function(path) {
            sep_nchar <- 1  # currently only allows one character in seperator (excl. "\\")
            substr(path, 0, last_sep_position(path) - sep_nchar)
        },
        
        parent = function(...) {
            data()[data()[[.path]] %in% parent_id(...)]
        },
        
        descendants_ids = function(path) {
            match(path, "^%1$s%2$s\\w*(%2$s|$)")
        },
        
        descendants = function(...) {
            data()[data()[[.path]] %in% descendants_ids(...)]
        },
        
        children_ids = function(path) {
            match(path, "^%1$s%2$s\\w*$")
        },
        
        children = function(...) {
            data()[data()[[.path]] %in% children_ids(...)]
        }
        
    )
)