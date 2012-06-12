\name{SS_writedat}
\alias{SS_writedat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  write data file
}
\description{
  write Stock Synthesis data file from list object in R which was
  probably created using \code{\link{SS_readdat}}
}
\usage{
SS_writedat(datlist, outfile, overwrite = FALSE, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datlist}{
    List object created by \code{\link{SS_readdat}}.
  }
  \item{outfile}{
    Filename for where to write new data file.
  }
  \item{overwrite}{
    Should existing files be overwritten? Default=FALSE.
  }
  \item{verbose}{
    Should there be verbose output while running the file?
  }
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{
  Ian Taylor
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{SS_makedatlist}},
 \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
 \code{\link{SS_readctl}},
 \code{\link{SS_writestarter}}, \code{\link{SS_writeforecast}},
 \code{\link{SS_writedat}}, \code{\link{SS_writectl}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ manip }% __ONLY ONE__ keyword per line
