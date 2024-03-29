% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getADMBHessian.R
\name{getADMBHessian}
\alias{getADMBHessian}
\title{Read admodel.hes file}
\usage{
getADMBHessian(
  hesfile = "admodel.hes",
  File = lifecycle::deprecated(),
  FileName = lifecycle::deprecated()
)
}
\arguments{
\item{hesfile}{Name of .hes file, including the full path (can be
relative to working directory).}

\item{File}{Deprecated. Add path to \code{hesfile} input instead.}

\item{FileName}{Deprecated. Use `hesfile`` instead.}
}
\value{
A list with elements num.pars, hes, hybrid_bounded_flag, and scale.
}
\description{
This function reads in all of the information contained in the
.hes file. Some is needed for relaxing the covariance matrix, while the rest
is recorded and rewritten to file as ADMB expects.
}
\note{
Explanation of the methods (in PDF form):
\url{https://github.com/admb-project/admb-examples/blob/master/admb-tricks/covariance-calculations/ADMB_Covariance_Calculations.pdf}
}
\seealso{
\code{\link[=read.admbFit]{read.admbFit()}}, \code{\link[=NegLogInt_Fn]{NegLogInt_Fn()}}
}
\author{
Cole Monnahan
}
