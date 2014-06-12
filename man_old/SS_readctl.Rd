\name{SS_readctl}
\alias{SS_readctl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  read control file
}
\description{
  read Stock Synthesis control file into list object in R
}
\usage{
SS_readctl(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
    Filename either with full path or relative to working directory.
  }
}
\details{
  This function is not fully implemented. The logic to figure out all
  the details of a Stock Synthesis control file is very complex, so this
  function may be completed in a way that is not totally consistent with
  the other similar files. Or it may never be completed at all. The
  functions \code{\link{SS_changepars}} and \code{\link{SS_parlines}}
  offer alternatives for working with SS control files.
}
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
% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{SS_changepars}}, \code{\link{SS_parlines}},
 \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
 \code{\link{SS_readdat}},
 \code{\link{SS_writestarter}}, \code{\link{SS_writeforecast}},
 \code{\link{SS_writedat}}, \code{\link{SS_writectl}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
