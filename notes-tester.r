require(reshape2)
require(plyr)
require(data.table)

.year <- 2011
.grand_label <- "Summa"

notes <- read.csv2("~/Desktop/noter.csv", stringsAsFactors = FALSE)
notes <- as.data.table(melt(notes))
notes$year <- substring(notes$variable, 2, 5)
notes$notes <- notes$Notes_sv
notes$label <- notes$Label_sv

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

by_child_table <- function(id, fun = sum_aggr_na) {
    this_year <- custom_aggr_by(notes[year == .year], ids = id, by_child = TRUE, to_levels = FALSE, fun = fun)
    colnames(this_year)[ncol(this_year)] <- paste(.grand_label, .year)
    sort_key <- 1:nrow(this_year)
    
    prev_year <- custom_aggr_by(notes[year == .year - 1], ids = id, by_child = TRUE, to_levels = FALSE, fun = fun)
    colnames(prev_year)[ncol(prev_year)] <- paste(.grand_label, .year - 1)
    
    x <- merge(this_year, prev_year[, c(1:3, ncol(prev_year))], by.x = c("Id", "label", "notes"), all = FALSE, sort = FALSE)
    x$Id <- id_to_levels(x$Id)
    return(x)
}

### NOTER ###

# IP, resultaträkning
custom_aggr_by(notes, ids = "1.1.1.2.1.2", end = 2, margins = NULL, grand_label = "Årets resultat")

# IP, balansräkning
custom_aggr_by(notes, ids = "1.1.1", end = 3, include = FALSE, margins = NULL)

# PP, resultaträkning
custom_aggr_by(notes, ids = "1.2.1.2.1.2", end = 2, margins = NULL, grand_label = "Årets resultat")

# PP, balansräkning
custom_aggr_by(notes, ids = "1.2.1", end = 3, include = FALSE, margins = NULL)

# Not 2
custom_aggr_by(notes, ids = "1.1.1.2.1.2.1.2", margins = NULL, fun = function(x) sum_aggr_na(-x))

# Not 3
by_child_table("1.1.1.2.1.2.1.3.1")

# Not 4
custom_aggr_by(notes, ids = "1.1.1.2.1.2.1.4", margins = NULL, fun = function(x) sum_aggr_na(-x))

# Not 7
custom_aggr_by(notes, ids = "1.1.1.2.1.2.3.6", margins = NULL, fun = function(x) sum_aggr_na(-x))

# Not 8
by_child_table("1.1.1.2.1.2.3.7", fun = function(x) sum_aggr_na(-x))
