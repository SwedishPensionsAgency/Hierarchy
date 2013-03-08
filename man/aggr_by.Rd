\name{aggr_by}
\alias{aggr_by}
\title{Aggregate by}
\usage{
  aggr_by(data, path = colnames(data)[1],
    metrics = "value", ids = "1", by = "variable", ...,
    fun = function(x) sum(x, na.rm = TRUE),
    to_levels = FALSE)
}
\arguments{
  \item{data}{data frame}

  \item{path}{column with path enumeration ids}

  \item{metrics}{metric columns}

  \item{ids}{node id (e.g. "1.2.1.3")}

  \item{by}{column to aggregate by}

  \item{fun}{aggregate function (e.g. sum)}

  \item{...}{arguments passed to descendants_ids()}

  \item{to_levels}{if to convert ids to levels}
}
\description{
  A wrapper function to the path enumeration class to
  aggregate nodes. The hierarchical data set must have a
  path enumerated column.
}
\examples{
# aggr_by(melt(notes), ids = "1.1.1.2.2", end = 2, include = TRUE)
}

