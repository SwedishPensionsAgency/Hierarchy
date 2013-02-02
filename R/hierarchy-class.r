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