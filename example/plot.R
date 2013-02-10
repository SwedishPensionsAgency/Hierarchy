
#' Customized plot theme
#' 
#' @param p ggplot object
#' @import ggplot2
my_theme <- function(right_margin = 4, ...) {
    theme(
        legend.position = "none",
        plot.margin = unit(c(0, right_margin, 0, 0), "cm"),  # todo: auto-resize margin depending on name lengths
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        ...
    )
}

#' Fix labels
#' 
#' @param p ggplot object
#' @import ggplot2
fix_labels <- function(p) {
    p <- ggplot_gtable(ggplot_build(p))
    p$layout$clip[p$layout$name=="panel"] <- "off"
    grid.draw(p)
}

#' Line plot
#' 
#' @param ... arguments passed to ggplot() function
#' @param dl_method directlabels method
#' @param cex directlabels cex argument (font size)
#' @param theme ggplot theme
#' @import ggplot2, directlabels
#' @export
line_plot <- function(..., dl_method = "last.bumpup", cex = 1, theme = my_theme()) {
    p <- ggplot(...) + geom_line() + geom_dl(method = list(dl_method, cex = cex)) + theme
    fix_labels(p)
}


# Example data
x <- structure(list(year = c(2010L, 2011L, 2012L, 2010L, 2011L, 2012L, 2010L, 2011L, 2012L), country = structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L), .Label = c("Finland", "Norway", "Sweden"), class = "factor"), value = c(2, 3, 3, 0, 4, 3, 1, 2, 2)), .Names = c("year", "country", "value"), row.names = c(NA, -9L), class = "data.frame")
x$country <- paste("", x$country)
line_plot(data = x, aes(x = year, y = value, colour = country, label = country))
