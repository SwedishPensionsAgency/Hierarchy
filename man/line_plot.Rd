\name{line_plot}
\alias{line_plot}
\title{Line plot}
\usage{
  line_plot(data, x, y, group, xlab = "x", ylab = "y",
    size = 2, dl_method = "last.bumpup", cex = 1,
    theme = my_theme(right_margin = margin_width))
}
\arguments{
  \item{...}{arguments passed to ggplot() function}

  \item{dl_method}{directlabels method}

  \item{cex}{directlabels cex argument (font size)}

  \item{theme}{ggplot theme}
}
\description{
  Line plot
}
\examples{
require("ggplot2")
require("directlabels")
x <- structure(list(year = c(2010L, 2011L, 2012L, 2010L, 2011L, 2012L, 2010L, 2011L, 2012L), country = structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L), .Label = c("Finland", "Norway", "Sweden"), class = "factor"), value = c(2, 3, 3, 0, 4, 3, 1, 2, 2)), .Names = c("year", "country", "value"), row.names = c(NA, -9L), class = "data.frame")
x$country <- paste("", x$country)
line_plot(x, "year", "value", "country", xlab = "YEAR", ylab = "SEK")
}

