require(reshape2)
require(plyr)
require(data.table)

# TODO
# - How to keep NA's in tables? (e.g. note 12)

# Inställningar
.year <- 2011
.lang <- "sv"
.grand_label <- "Summa"

# Data
.notes <- function() import_csv("~/Desktop/noter.csv", .lang)

# Funktioner

## Läs in csv data
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

by_child_table <- function(data, ...) {
    this_year <- custom_aggr_by(data[year == .year], by_child = TRUE, to_levels = FALSE, ...)
    colnames(this_year)[ncol(this_year)] <- paste(.grand_label, .year)
    sort_key <- 1:nrow(this_year)
    
    prev_year <- custom_aggr_by(data[year == .year - 1], by_child = TRUE, to_levels = FALSE, ...)
    colnames(prev_year)[ncol(prev_year)] <- paste(.grand_label, .year - 1)
    
    x <- merge(this_year, prev_year[, c(1:3, ncol(prev_year))], by.x = c("Id", "label", "notes"), all = FALSE, sort = FALSE)
    x$Id <- id_to_levels(x$Id)
    return(x)
}

### NOTER ###

# IP, resultaträkning
x <- custom_aggr_by(.notes(), ids = "1.1.1.2.1.2", end = 2, margins = NULL, grand_label = "Årets resultat")

# IP, balansräkning
x <- custom_aggr_by(.notes(), ids = "1.1.1", end = 3, include = FALSE, margins = NULL)

# PP, resultaträkning
x <- custom_aggr_by(.notes(), ids = "1.2.1.2.1.2", end = 2, margins = NULL, grand_label = "Årets resultat")

# PP, balansräkning
x <- custom_aggr_by(.notes(), ids = "1.2.1", end = 3, include = FALSE, margins = NULL)

# Not 2
x <- custom_aggr_by(.notes(), ids = "1.1.1.2.1.2.1.2", margins = NULL, fun = function(x) sum_aggr_na(-x))

# Not 3
x <- by_child_table(.notes(), ids = "1.1.1.2.1.2.1.3.1")

# Not 4
x <- custom_aggr_by(.notes(), ids = "1.1.1.2.1.2.1.4", margins = NULL, fun = function(x) sum_aggr_na(-x))

# Not 7
x <- custom_aggr_by(.notes(), ids = "1.1.1.2.1.2.3.6", margins = NULL, fun = function(x) sum_aggr_na(-x))

# Not 8
x <- by_child_table(.notes(), ids = "1.1.1.2.1.2.3.7", fun = function(x) sum_aggr_na(-x))

# Not 9
x <- by_child_table(.notes(), ids = "1.1.1.2.1.2.3.8", fun = function(x) sum_aggr_na(-x))

# Not 10
uppkomna <- custom_aggr_by(.notes(), ids = "1.1.1.2.1.2.3.9", margins = NULL)
fordelade <- custom_aggr_by(.notes(), ids = "1.1.1.2.1.2.3.10", margins = NULL, fun = function(x) sum_aggr_na(-x))
x <- rbind(uppkomna, fordelade)

# Not 11
x <- custom_aggr_by(.notes(), ids = "1.1.1.2.1.2.3.11", margins = NULL)

# Not 12
x <- by_child_table(.notes(), ids = "1.1.1.1.1.1")

# Not 14
x <- by_child_table(.notes(), ids = "1.1.1.2.2.1", end = 1)

# Not 14 a
x <- custom_aggr_by(.notes(), ids = "1.1.1.2.2.1.1.1", margins = NULL)
