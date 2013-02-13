#' Customized plot theme
#' 
#' @param p ggplot object
#' @import ggplot2
my_theme <- function(right_margin = 4, ...) {
    theme(
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = -0.25),
        legend.position = "none",
        plot.margin = unit(c(1, right_margin, 1, 1), "cm"),  # todo: auto-resize margin depending on name lengths
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
#' @import ggplot2 directlabels
#' @export
line_plot <- function(data, x, y, group, xlab = "x", ylab = "y", size = 2, dl_method = "last.bumpup", cex = 1, theme = my_theme()) {
    x <- data.frame(x = data[[x]], y = data[[y]], group = data[[group]])
    p <- ggplot(data = x, aes(x = x, y = y, color = group, label = group)) +
         geom_line(size = size) +
         geom_dl(method = list(dl_method, cex = cex)) +
         coord_cartesian(xlim = c(min(x$x), max(x$x)), ylim = c(min(x$y), max(x$y))) +
         xlab(xlab) + 
         ylab(ylab) +
         theme
    fix_labels(p)
}
