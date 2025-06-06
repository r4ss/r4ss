% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_fxns.R
\name{tryCatch.W.E}
\alias{tryCatch.W.E}
\title{tryCatch both warnings (with value) and errors}
\usage{
tryCatch.W.E(expr)
}
\arguments{
\item{expr}{an \R expression to evaluate}
}
\value{
a list with 'value' and 'warning', where
'value' may be an error caught.
}
\description{
Catch \emph{and} save both errors and warnings, and in the case of
a warning, also keep the computed result.
}
\details{
Copied from
https://svn.r-project.org/R/trunk/src/library/base/demo/error.catching.R
}
\author{
Martin Maechler;
Copyright (C) 2010-2023  The R Core Team
}
