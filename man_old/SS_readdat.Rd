\name{SS_readdat}
\alias{SS_readdat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  read data file
}
\description{
  read Stock Synthesis data file into list object in R
}
\usage{
SS_readdat(file, verbose = TRUE, echoall = FALSE, section = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
    Filename either with full path or relative to working directory.
  }
  \item{verbose}{
    Should there be verbose output while running the file? Default=TRUE.
  }
  \item{echoall}{
    Debugging tool (not fully implemented) of echoing blocks of
    data as it is being read. }
  \item{section}{
    Which data set to read. Only applies for a data.ss_new file created
    by Stock Synthesis. Allows the choice of either expected values
    (section=2) or bootstrap data (section=3+). Leaving default of
    section=NULL will read input data, (equivalent to section=1).
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
 \code{\link{SS_readctl}},
 \code{\link{SS_writestarter}}, \code{\link{SS_writeforecast}},
 \code{\link{SS_writedat}}, \code{\link{SS_writectl}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
