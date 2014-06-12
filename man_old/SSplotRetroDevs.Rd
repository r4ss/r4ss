\name{SSplotRetroDevs}
\alias{SSplotRetroDevs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Make squid plot of retrospectives of recruitment deviations.
}
\description{
  Inspired by Jim Ianelli and named by Sean Cox, the squid plot is a way
  to examine retrospective patterns in estimation of recruitment
  deviations.
}
\usage{
SSplotRetroDevs(retroSummary, endyrvec, cohorts, ylim = c(-3, 3),
uncertainty = FALSE,
labels = c("Recruitment deviation relative to recent estimate",
"Recruitment deviation", "Age"),
main = "Retrospective analysis of recruitment deviations",
mcmcVec = FALSE, relative = FALSE,
labelyears = TRUE, legend = FALSE, leg.ncols = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{retroSummary}{
    List object created by \code{\link{SSsummarize}} that summarizes the
    results of a set of retrospective analysis models.ss
  }
  \item{endyrvec}{
    Vector of years representing the final year
    of values to show for each model.
  }
  \item{cohorts}{
    Which cohorts to show in plot.
  }
  \item{ylim}{
    Limits of y-axis.
  }
  \item{uncertainty}{
    Show uncertainty intervals around lines? (This can get a bit busy.)
  }
  \item{labels}{
    Vector of plot labels.
  }
  \item{main}{
    Title for plot.
  }
  \item{mcmcVec}{
    Either vector of TRUE/FALSE values indicating which models use MCMC.
    Or single value applied to all models.
  }
  \item{relative}{
    Show deviations relative to most recent estimate or relative to 0.
  }
  \item{labelyears}{
    Label cohorts with text at the end of each line?
  }
  \item{legend}{
    Add a legend showing which color goes with which line (as
    alternative to \code{labelyears}).
  }
  \item{leg.ncols}{
    Number of columns for the legend.
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
\references{
  Ianelli et al. (2011) Assessment of the walleye pollock stock in the
  Eastern Bering Sea.
  \url{http://www.afsc.noaa.gov/REFM/docs/2011/EBSpollock.pdf}. (Figure
  1.31, which is on an absolute, rather than log scale.)
}
\author{
  Ian Taylor
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SSsummarize}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
