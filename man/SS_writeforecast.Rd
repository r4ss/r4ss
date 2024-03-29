% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SS_writeforecast.R
\name{SS_writeforecast}
\alias{SS_writeforecast}
\title{write forecast file}
\usage{
SS_writeforecast(
  mylist,
  dir = NULL,
  file = "forecast.ss",
  writeAll = FALSE,
  overwrite = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{mylist}{List object created by \code{\link[=SS_readforecast]{SS_readforecast()}}.}

\item{dir}{A file path to the directory of interest.
The default value is \code{dir = NULL}, which leads to using
the current working directory.}

\item{file}{Filename for new forecast file. Default="forecast.ss".}

\item{writeAll}{Should the function continue even if Forecast=0
(at which point SS stops reading, and remaining elements in list may not be
available, depending on settings used in SS_readforecast)}

\item{overwrite}{A logical value specifying if the existing file(s)
should be overwritten. The default value is \code{overwrite = FALSE}.}

\item{verbose}{A logical value specifying if output should be printed
to the screen.}
}
\description{
write Stock Synthesis forecast file from list object in R which was probably
created using \code{\link[=SS_readforecast]{SS_readforecast()}}
}
\seealso{
Other read/write functions: 
\code{\link{SS_read}()},
\code{\link{SS_readctl}()},
\code{\link{SS_readdat}()},
\code{\link{SS_readforecast}()},
\code{\link{SS_readstarter}()},
\code{\link{SS_write}()},
\code{\link{SS_writectl}()},
\code{\link{SS_writedat}()},
\code{\link{SS_writestarter}()}
}
\author{
Ian G. Taylor, Kelli F. Johnson, Kathryn L. Doering, Nathan R. Vaughan
}
\concept{read/write functions}
