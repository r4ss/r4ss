\name{SSplotSummaryF}
\alias{SSplotSummaryF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot the summary F (or harvest rate).
}
\description{
  Plots the summary F (or harvest rate) as set up in the starter file
  Needs a lot of work to be generalized
}
\usage{
SSplotSummaryF(replist, yrs = "all", Ftgt = NA, ylab = "Summary Fishing Mortality")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    List created by \code{SS_output}
  }
  \item{yrs}{
    Which years to include.
  }
  \item{Ftgt}{
    Target F where horizontal line is shown.
  }
  \item{ylab}{
    Y-axis label.
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
  Allan Hicks
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SSplotTimeseries}}, ~~~
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
