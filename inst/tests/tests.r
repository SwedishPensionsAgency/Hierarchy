
# Test data
kpi <- structure(list(
    id = structure(1:10, .Label = c("1", "1.1", "1.1.1", "1.1.1.1", "1.1.1.2", "1.1.1.3", "1.2", "1.2.1", "1.2.1.1", "1.2.1.2"), class = "factor"), 
    name = structure(c(3L, 2L, 6L, 5L, 9L, 10L, 1L, 4L, 8L, 7L), .Label = c("Hälso- och sjukvård", "Inventarier", "KPI", "Läkemedel", "Matbord", "Möbler", "Naturläkemedel", "Sjukvårdsartiklar", "Säng", "Taklampa"), class = "factor"), 
    year = c(NA, NA, NA, 2012L, 2012L, 2012L, NA, NA, 2012L, 2012L), 
    month = c(NA, NA, NA, 12L, 12L, 12L, NA, NA, 12L, 12L), 
    weight = c(NA, NA, NA, 1.48, 2.9, 2.21, NA, NA, 0.98, 3.03), 
    consumption = c(NA, NA, NA, 40000L, 20000L, 5000L, NA, NA, 10000L, 12000L)), .Names = c("id", "name", "year", "month", "weight", "consumption"), class = "data.frame", row.names = c(NA, -10L))

a <- path_enum$new(kpi)
a$data()

x <- c("1.1", "1")

a$descendants_ids(x)
a$descendants(x)
a$has_descendants(x)

a$children_ids(x)
a$children(x)
a$has_children(x)

a$parent_id(x)
a$parent(x)
a$has_parent(x)

a$endnodes_ids(x)
a$endnodes(x)

a$endnodes_aggregate("1.1", c("weight"), function(x) sum(x, na.rm = TRUE))
a$endnodes_aggregate("1.1", c("consumption", "weight"), function(x) mean(x, na.rm = TRUE))
