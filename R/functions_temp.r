library(RJSONIO)

ch <- c("a", "a.a", "a.a.a", "a.a.a.a", "a.b", "b", "b.a")

# Get hierarchical dimensions (occurences of ".")
ch_dim <- function(x, delimiter = ".") {
    chr.count <- function(x) length(which(unlist(strsplit(x, NULL)) == delimiter))
    if (length(x) > 1) {
        sapply(x, chr.count) + 1
    } else {
        chr.count(x) + 1
    }
}

lst_fun <- function(ch, num = NULL, stp = NULL) {
    
    d <- ch_dim(ch)
    if (is.null(num)) num <- min(d)
    if (is.null(stp)) stp <- max(d)
    
    lapply(ch[d == num], function(x) {
        tt <- ch[grepl(sprintf("^%s.", x), ch)]
        if (stp != num && length(tt) > 0) { 
            list(name = x, children = lst_fun(tt, num + 1, stp)) 
        } else { list(name = x) }
    })
}

s <- toJSON(lst_fun(ch))
