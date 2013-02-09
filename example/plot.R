
library("ggplot2")
library("directlabels")
library("reshape")

# Example data
x <- structure(list(year = c(2010L, 2011L, 2012L, 2010L, 2011L, 2012L, 2010L, 2011L, 2012L), country = structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L), .Label = c("Finland", "Norway", "Sweden"), class = "factor"), value = c(2, 3, 3, 0, 4, 3, 1, 2, 2)), .Names = c("year", "country", "value"), row.names = c(NA, -9L), class = "data.frame")

x$country <- paste("", x$country)

p <- ggplot(x, aes(x = year, y = value, colour = country)) + geom_line()
# p <- p + xlim(min(x$year), max(x$year))
p <- p + geom_dl(aes(label=country), method=list("last.bumpup", cex=1), show_guide=FALSE) 
p <- p + theme(
    legend.position="none",
    plot.margin = unit(c(0, 4, 0, 0), "cm"),
    panel.background = element_rect(fill='white', colour='white')
)

p1 <- ggplot_gtable(ggplot_build(p))
p1$layout$clip[p1$layout$name=="panel"] <- "off"
grid.draw(p1)
