% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_values.R
\name{get_values}
\alias{get_values}
\title{Function to pull values and calculate confidence intervals from model output
called from \code{r4ss::table_exec_summary()}.}
\usage{
get_values(replist, label, yrs, ci_value, single = FALSE)
}
\arguments{
\item{replist}{A list object created by \code{\link{SS_output}()}.}

\item{label}{The parameter name to calculate confidence intervals for. The name
is based on the names in the \code{replist} object.}

\item{yrs}{Vector of years to calculate confidence intervals for.}

\item{ci_value}{To calculate confidence intervals, the desired interval must
be specified. The default is 0.95.}

\item{single}{Calculate the confidence interval for a single year or parameter.
The default is FALSE.}
}
\value{
data frame with point estimate and confidence interval low and high values
}
\description{
Function to pull values and calculate confidence intervals from model output
called from \code{r4ss::table_exec_summary()}.
}
\author{
Chantel R. Wetzel, Kelli F. Johnson, Ian G. Taylor
}
