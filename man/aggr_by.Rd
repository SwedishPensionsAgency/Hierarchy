\name{aggr_by}
\alias{aggr_by}
\title{Aggregate nodes}
\usage{
  aggr_by(data, path = colnames(data)[1],
    labels = colnames(data)[2], dims = NULL,
    metrics = "value", ids = "1", include = TRUE,
    fun = sum_aggr_na, to_levels = FALSE, by_child = FALSE,
    grand_label = "(all)",
    cast_col = ifelse(by_child, "root", dims[1]),
    formula = paste(path, "+", paste(labels, collapse = " + "), "~", cast_col),
    margins = cast_col, sort_cols = FALSE, ...)
}
\arguments{
  \item{data}{data frame}

  \item{path}{column with path enumeration ids}

  \item{labels}{label columns (description of the
  variable).}

  \item{dims}{dimension columns}

  \item{metrics}{metric columns}

  \item{ids}{node id (e.g. "1.2.1.3")}

  \item{include}{if the node itself should be returned as a
  row}

  \item{fun}{function to be used in metric calculations
  (e.g. sum)}

  \item{to_levels}{if to convert ids to levels}

  \item{by_child}{if all calculations should be done on the
  children of the id node. Return column "root" will
  contain the (first) label of the children.}

  \item{grand_label}{label to be used for grand
  calculations (e.g. total sum). Default: "(all)"}

  \item{cast_col}{dcast column}

  \item{formula}{dcast forumula}

  \item{margins}{dcast margins; NULL means no margins}

  \item{sort_cols}{set to FALSE if columns shouldnt be
  reordered (only used with dcast)}

  \item{...}{arguments passed to the descendants_ids()
  function; start = where to start in the subtree, end =
  where to end in the subtree.}
}
\description{
  A wrapper function to the path enumeration class, that is
  used to aggregate nodes by dimensions and/or children.
  The hierarchical data set must have a path enumerated
  column.
}
\examples{
\dontrun{
x <- melt(notes)
aggr_by(x, ids = "1.1.1.2.2.1.3.1", path = "Id", labels = "Label", dims = c("variable"), metrics = c("value"), include = FALSE, end = 1, by_child = FALSE, id_format = "stars")
aggr_by(x, ids = "1.1.1.2.2.1.3.1", path = "Id", labels = "Label", dims = c("variable"), metrics = c("value"), include = TRUE, end = 1, by_child = TRUE, id_format = "stars")
aggr_by(x, ids = "1.1.1.2.2.1.3.1", path = "Id", dims = c("variable"), metrics = c("value"), include = TRUE, end = 2, labels = "Label", by_child = TRUE, id_format = "levels")
}
}

