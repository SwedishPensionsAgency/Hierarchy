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

### NOTER ###

# IP, resultaträkning
custom_aggr_by(notes, ids = "1.1.1.2.1.2", margins = NULL, grand_label = "Årets resultat")

# IP, balansräkning
custom_aggr_by(notes, ids = "1.1.1", end = 3, include = FALSE, margins = NULL)

# PP, resultaträkning
custom_aggr_by(notes, ids = "1.2.1.2.1.2", margins = NULL, grand_label = "Årets resultat")

# PP, balansräkning
custom_aggr_by(notes, ids = "1.2.1", end = 3, include = FALSE, margins = NULL)

# Not 2
custom_aggr_by(notes, ids = "1.1.1.2.1.2.1.2", margins = NULL, fun = function(x) sum_aggr_na(-x))

# Not 3
x <- merge(
    custom_aggr_by(notes[year == .year], ids = "1.1.1.2.1.2.1.3.1", by_child = TRUE), 
    custom_aggr_by(notes[year == .year - 1], ids = "1.1.1.2.1.2.1.3.1", by_child = TRUE)[, c("Id", "label", .grand_label)], 
    by = c("Id", "label"), all = TRUE)
colnames(x)[ncol(x) - 1] <- paste(.grand_label, .year) 
colnames(x)[ncol(x)] <- paste(.grand_label, .year - 1)

# Not 4
custom_aggr_by(notes, ids = "1.1.1.2.1.2.1.4", margins = NULL, fun = function(x) sum_aggr_na(-x))

