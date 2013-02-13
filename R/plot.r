#' Customized plot theme
#' 
#' @param p ggplot object
my_theme <- function(right_margin = 4, ...) {
    theme(
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = -0.25),
        legend.position = "none",
        plot.margin = unit(c(0.10, right_margin, 0.2, 0.25), "inches"),  # (top, right, bottom, left)
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted"),
        axis.line = element_line(colour = "black"),
        ...
    )
}

#' Fix labels
#' 
#' @param p ggplot object
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
#' @examples
#' require("ggplot2")
#' require("directlabels")
#' x <- structure(list(year = c(2010L, 2011L, 2012L, 2010L, 2011L, 2012L, 2010L, 2011L, 2012L), country = structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L), .Label = c("Finland", "Norway", "Sweden"), class = "factor"), value = c(2, 3, 3, 0, 4, 3, 1, 2, 2)), .Names = c("year", "country", "value"), row.names = c(NA, -9L), class = "data.frame")
#' x$country <- paste("", x$country)
#' line_plot(x, "year", "value", "country", xlab = "YEAR", ylab = "SEK")
#' @export
line_plot <- function(data, x, y, group, xlab = "x", ylab = "y", size = 2, dl_method = "last.bumpup", cex = 1, theme = my_theme(right_margin = margin_width)) {
    x <- data.frame(x = data[[x]], y = data[[y]], group = data[[group]])

    plot.new()
    p <- ggplot(data = x, aes(x = x, y = y, color = group, label = group)) +
         geom_line(size = size) +
         geom_dl(method = list(dl_method, cex = cex)) +
         coord_cartesian(xlim = c(min(x$x), max(x$x)), ylim = c(min(x$y), max(x$y))) +
         xlab(xlab) + 
         ylab(ylab)
    
    margin_width <- max(strwidth(unique(data[[group]]), cex = cex, units = "inches"))  # measure used to adjust plot right margin
    p <- p + theme
    
    fix_labels(p)
}
