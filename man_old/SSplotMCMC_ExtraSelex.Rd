\name{SSplotMCMC_ExtraSelex}
\alias{SSplotMCMC_ExtraSelex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot uncertainty around chosen selectivity ogive from MCMC.
}
\description{
  Plot uncertainty in selectivity from an MCMC output for whichever
  fleet/year was chosen in the optional extra "more stddev reporting"
}
\usage{
SSplotMCMC_ExtraSelex(post, add = FALSE, nsexes = 1, shift = 0,
fleetname = "default", col = "blue") 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{post}{
    A data frame containing either derived_posteriors.sso or a good
    subset of it. This can be an element of the list created by the the
    \code{\link{SSgetMCMC}} function.
  }
  \item{add}{
    TRUE/FALSE option to add results to an existing plot.
  }
  \item{nsexes}{
    Number of sexes in the model (should match model values but is only
    used in the title).
  }
  \item{shift}{
    Optional adjustment to the x values to avoid overlap of intervals
    when overplotting on an existing plot.
  }
  \item{fleetname}{
    Optional input to make the title better. Default will be something
    like "Fleet 1", using the numbering from the model.
  }
  \item{col}{
    Color for points and lines.
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

% \seealso{
%   \code{\link{SS_plots}}, \code{\link{SS_output}}
% }
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
\keyword{ hplot }
