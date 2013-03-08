\name{aggr_by}
\alias{aggr_by}
\title{Aggregate by variable and/or children}
\usage{
  aggr_by(data, path = colnames(data)[1], label,
    dims = NULL, metrics = "value", ids = "1",
    include = FALSE,
    fun = function(x) sum(x, na.rm = TRUE),
    to_levels = FALSE, by_child = FALSE, ...)
}
\arguments{
  \item{data}{data frame}

  \item{path}{column with path enumeration ids}

  \item{label}{label column (description of the variable);
  required.}

  \item{dims}{dimension columns}

  \item{metrics}{metric columns}

  \item{ids}{node id (e.g. "1.2.1.3")}

  \item{include}{if the node itself should be returned as a
  row}

  \item{fun}{function to be used in metric calculations
  (e.g. sum)}

  \item{to_levels}{if ids should be converted to levels}

  \item{by_child}{if all calculations should be done on the
  children of the id node. Return column "root" will
  contain the label of the children.}

  \item{...}{arguments passed to the descendants_ids()
  function; start = where to start in the subtree, end =
  where to end in the subtree.}
}
\description{
  A wrapper function to the path enumeration class to
  aggregate nodes. The hierarchical data set must have a
  path enumerated column.
}
\examples{
NULL
}

