
# Function to get the number hierarchical dimensions (occurences of "." + 1)
ch_dim <- function(x, delimiter = ".") {
    x <- as.character(x)
    chr.count <- function(x) length(which(unlist(strsplit(x, NULL)) == delimiter))
    if (length(x) > 1) {
        sapply(x, chr.count) + 1
    } else {
        chr.count(x) + 1
    }
}

# Function to convert a hierarchical data.frame to a nested list
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

# Convert data.frame to list
lst <- lst_fun(df, "id")

# Convert list to json
s <- RJSONIO::toJSON(lst)

