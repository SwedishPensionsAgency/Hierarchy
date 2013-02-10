
# Example data
x <- structure(list(year = c(2010L, 2011L, 2012L, 2010L, 2011L, 2012L, 2010L, 2011L, 2012L), country = structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L), .Label = c("Finland", "Norway", "Sweden"), class = "factor"), value = c(2, 3, 3, 0, 4, 3, 1, 2, 2)), .Names = c("year", "country", "value"), row.names = c(NA, -9L), class = "data.frame")
x$country <- paste("", x$country)
line_plot(x, "year", "value", "country", xlab = "År", ylab = "Kronor")
