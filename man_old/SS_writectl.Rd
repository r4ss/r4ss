\name{SS_writectl}
\alias{SS_writectl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  write control file
}
\description{
  Write Stock Synthesis control file. Like \code{\link{SS_readctl}},
  this function is not fully developed.
}
\usage{
SS_writectl(ctllist, outfile, overwrite = F, verbose = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ctllist}{
    List object created by \code{\link{SS_readctl}}.
  }
  \item{outfile}{
    Filename for where to write new control file.
  }
  \item{overwrite}{
    Should existing files be overwritten? Default=F.
  }
  \item{verbose}{
    Should there be verbose output while running the file? Default=T.
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
 \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
 \code{\link{SS_readdat}}, \code{\link{SS_readctl}},
 \code{\link{SS_writestarter}}, \code{\link{SS_writeforecast}},
 \code{\link{SS_writedat}}, \code{\link{SS_writectl}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ manip }% __ONLY ONE__ keyword per line
