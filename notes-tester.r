require(reshape2)
require(plyr)
require(data.table)

.year <- 2011
.lang <- "sv"
.grand_label <- "Summa"

import_csv <- function(file, lang) {
    x <- read.csv2(file, stringsAsFactors = FALSE)
    x <- as.data.table(melt(x))
    x$year <- substring(x$variable, 2, 5)
    x$label <- x[[paste("Label", lang, sep = "_")]]
    x$notes <- x[[paste("Notes", lang, sep = "_")]]
    return(x)
}

custom_aggr_by <- function(data,
                           path = "Id", 
                           labels = c("label", "notes"), 
                           dims = c("year"), 
                           metrics = c("value"),
                           to_levels = TRUE,
                           grand_label = .grand_label,
                           ...) {
    
    aggr_by(data = data, 
            path = path,
            labels = labels,
            dims = dims,
            metrics = metrics,
            to_levels = to_levels,
            grand_label = grand_label,
            ...)
}

by_child_table <- function(data, id, fun = sum_aggr_na) {
    this_year <- custom_aggr_by(data[year == .year], ids = id, by_child = TRUE, to_levels = FALSE, fun = fun)
    colnames(this_year)[ncol(this_year)] <- paste(.grand_label, .year)
    sort_key <- 1:nrow(this_year)
    
    prev_year <- custom_aggr_by(data[year == .year - 1], ids = id, by_child = TRUE, to_levels = FALSE, fun = fun)
    colnames(prev_year)[ncol(prev_year)] <- paste(.grand_label, .year - 1)
    
    x <- merge(this_year, prev_year[, c(1:3, ncol(prev_year))], by.x = c("Id", "label", "notes"), all = FALSE, sort = FALSE)
    x$Id <- id_to_levels(x$Id)
    return(x)
}

### NOTER ###

# IP, resultaträkning
x <- import_csv("~/Desktop/noter.csv", .lang)
x <- custom_aggr_by(x, ids = "1.1.1.2.1.2", end = 2, margins = NULL, grand_label = "Årets resultat")

# IP, balansräkning
x <- import_csv("~/Desktop/noter.csv", .lang)
x <- custom_aggr_by(x, ids = "1.1.1", end = 3, include = FALSE, margins = NULL)

# PP, resultaträkning
x <- import_csv("~/Desktop/noter.csv", .lang)
x <- custom_aggr_by(x, ids = "1.2.1.2.1.2", end = 2, margins = NULL, grand_label = "Årets resultat")

# PP, balansräkning
x <- import_csv("~/Desktop/noter.csv", .lang)
x <- custom_aggr_by(x, ids = "1.2.1", end = 3, include = FALSE, margins = NULL)

# Not 2
x <- import_csv("~/Desktop/noter.csv", .lang)
x <- custom_aggr_by(x, ids = "1.1.1.2.1.2.1.2", margins = NULL, fun = function(x) sum_aggr_na(-x))

# Not 3
x <- import_csv("~/Desktop/noter.csv", .lang)
x <- by_child_table(x, "1.1.1.2.1.2.1.3.1")

# Not 4
x <- import_csv("~/Desktop/noter.csv", .lang)
x <- custom_aggr_by(x, ids = "1.1.1.2.1.2.1.4", margins = NULL, fun = function(x) sum_aggr_na(-x))

# Not 7
x <- import_csv("~/Desktop/noter.csv", .lang)
x <- custom_aggr_by(x, ids = "1.1.1.2.1.2.3.6", margins = NULL, fun = function(x) sum_aggr_na(-x))

# Not 8
x <- import_csv("~/Desktop/noter.csv", .lang)
x <- by_child_table(x, "1.1.1.2.1.2.3.7", fun = function(x) sum_aggr_na(-x))
