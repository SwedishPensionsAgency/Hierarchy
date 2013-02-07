require("reshape")
require("data.table")
require("Hierarchy")

# .onLoad

## Läs in data
x <- read.csv2("example/exempel-tabelldata-OR.csv", fileEncoding = "Windows-1252")

## Transformera
x <- melt(x, id.vars = c("ID", "NAMN_SV", "FORMAN_SV"), variable_name = "YEAR")
x$YEAR <- as.integer(substring(x$YEAR, 2, 5))

# For each table

## Selection
fun <- function(x, path, forman, year, deep) {
    x <- x[x$FORMAN_SV %in% c(forman, "(...)") & x$YEAR == year, ]
    x$FORMAN_SV <- forman
    h <- path_enum$new(data = x, 
                       path = "ID",
                       metrics = "value")
    ## Aggregation
    tbl <- h$endnodes_aggregate(h$descendants_ids(path, deep), function(x) sum(x, na.rm = TRUE))
}

tbl <- do.call("rbind", lapply(c("Premiepension", "Inkomstpension"), function(i) fun(x, "3", i, 2011, 2)))
cast(tbl, ID + NAMN_SV ~ FORMAN_SV, sum)

cast(fun(x, "1", "", 2011, 2), ID + NAMN_SV ~ FORMAN_SV, sum)
