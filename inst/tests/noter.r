require(Hierarchy)
require(reshape)
require(plyr)

x <- read.csv2("data/noter.csv")
x <- melt(noter)

# Aggregate (add as wrapper function in package)
aggr_by <- function(data, path = colnames(data)[1], metrics = "value", ids = "1", by = "variable", ..., fun = function(x) sum(x, na.rm = TRUE)) {
    ddply(data, dims, function(x) {
        a <- Hierarchy:::path_enum$new(data = x, path = path, metrics = metrics)
        a$aggregate(a$descendants_ids(ids, ...), fun)
    })
}

# EXAMPLE 1
ex1 <- aggr_by(x, ids = "1.1.1.2.2", end = 2)
cast(ex1, Id + Label_en ~ variable, sum)

# EXAMPLE 2
ip <- aggr_by(x, ids = "1.1.1.2.2", end = 2)
ip$forman <- "IP"
pp <- aggr_by(x, ids = "1.2.1.2.2", end = 2)
pp$forman <- "PP"
combo <- rbind(ip, pp)
combo$forman_ar <- paste(combo$forman, combo$variable)
cast(combo, Label_en ~ forman_ar, sum)
