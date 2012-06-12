\name{IOTCmove}
\alias{IOTCmove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Make a map of movement for a 5-area Indian Ocean model
}
\description{
  Run the \code{\link{SSplotMovementMap}} function with defaults related
  to a 5-area model for tunas in the Indian Ocean as discussed at the
  Indian Ocean Tuna Commission Working Party on Tropical Tunas in
  October, 2010. Obviously this is not useful for the majority of r4ss
  users, but it could serve as an example of how a wrapper function
  might be written for any other model.
}
\usage{
IOTCmove(replist = NULL, moveage = 5, moveseas = 1,
         legend = FALSE, title = NULL,
         areanames = c("R1","R2","R3","R4","R5"),
         ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    optional list created by \code{SS_output}
  }
  \item{moveage}{
    age for which movemement rates will be represented
  }
  \item{moveseas}{
    season for which movement rates will be represented
  }
  \item{legend}{
    add a legend to show the movement rate associated with the widest
    arrows
  }
  \item{title}{
    optional title to add at top of figure
  }
  \item{areanames}{
    vector of names for each area
  }
  \item{...}{
    Additional arguments can get passed to \code{\link{SSplotMovementMap}}
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
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
%   %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
% \examples{
%   % \dontrun{
%   %   IOTCmove(myreplist)
%   % }
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
