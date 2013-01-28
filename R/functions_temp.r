library(RJSONIO)

# Get hierarchical dimensions (occurences of ".")
ch_dim <- function(x, delimiter = ".") {
    x <- as.character(x)
    chr.count <- function(x) length(which(unlist(strsplit(x, NULL)) == delimiter))
    if (length(x) > 1) {
        sapply(x, chr.count) + 1
    } else {
        chr.count(x) + 1
    }
}

lst_fun <- function(ch, id_col = "id", num = NULL, stp = NULL) {
    
    # Convert data.frame to character
    ch <- data.frame(lapply(ch, as.character), stringsAsFactors=FALSE)
    
    d <- ch_dim(ch[[id_col]])
    if (is.null(num)) num <- min(d)
    if (is.null(stp)) stp <- max(d)
    
    lapply(ch[d == num,][[id_col]], function(x) {
        tt <- ch[grepl(sprintf("^%s.", x), ch[[id_col]]),]
        current <- ch[ch[[id_col]] == x,]
        if (stp != num && nrow(tt) > 0) { 
            c(current, list(children = lst_fun(tt, id_col, num + 1, stp)))
        } else { current }
    })
}

ch <- data.frame(
    id = c('1', '1.1', '1.1.1', '1.2'), 
    value = c(1054343, 5, 543354543, 5))#, stringsAsFactors = FALSE)

s <- toJSON(lst_fun(ch, "id"))

#ch_dim(ch$id)
