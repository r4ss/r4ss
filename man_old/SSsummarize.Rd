\name{SSsummarize}
\alias{SSsummarize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Summarize the output from multiple Stock Synthesis models.
}
\description{
  Summarize various quantities from the model output collected by
  \code{\link{SSgetoutput}} and return them in a list of tables and
  vectors.
}
\usage{
SSsummarize(biglist, keyvec = NULL, numvec = NULL,
sizeselfactor = "Lsel", ageselfactor = "Asel",
selfleet = NULL, selyr = "startyr", selgender = 1,
lowerCI = 0.025, upperCI = 0.975)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{biglist}{
    A list of lists created by \code{\link{SSgetoutput}}.
  }
  \item{keyvec}{
    Optional list of strings matching names of elements of biglist to
    subset. Default=NULL.
  }
  \item{numvec}{
    Optional list of numbers of elements from biglist to subset.
    Default=NULL.
  }
  \item{sizeselfactor}{
    A string or vector of strings indicating which elements of the
    selectivity at length output to summarize. Default=c("Lsel").
  }
  \item{ageselfactor}{
    A string or vector of strings indicating which elements of the
    selectivity at age output to summarize. Default=c("Asel").
  }
  \item{selfleet}{
    Vector of fleets for which selectivity will be summarized. NULL=all fleets. Default=NULL.
  }
  \item{selyr}{
    String or vector of years for which selectivity will be summarized.
    NOTE: NOT CURRENTLY WORKING.
    Options: NULL=all years, "startyr" = first year.
  }
  \item{selgender}{
    Vector of genders (1 and/or 2) for which selectivity will be summarized. NULL=all genders. Default=NULL.
  }
  \item{lowerCI}{
    Quantile for lower bound on calculated intervals. Default = 0.025
    for 95\% intervals.
  }
  \item{upperCI}{
    Quantile for upper bound on calculated intervals. Default = 0.975
    for 95\% intervals.
  }
}
% \details{
%   %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
%   %%  ~Describe the value returned
%   %%  If it is a LIST, use
%   %%  \item{comp1 }{Description of 'comp1'}
%   %%  \item{comp2 }{Description of 'comp2'}
%   %% ...
% }
% \references{
%   %% ~put references to the literature/web site here ~
% }
\author{
  Ian Taylor
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SSgetoutput}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ manip }
\keyword{ list }
