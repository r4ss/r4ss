\name{SS_fitbiasramp}
\Rdversion{1.1}
\alias{SS_fitbiasramp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate bias adjustment for recruitment deviates
}
\description{
Uses standard error of estimated recruitment deviates to estimate the 5
controls for the bias adjustment in Stock Synthesis
}
\usage{
SS_fitbiasramp(replist, verbose=FALSE, startvalues=NULL, method="BFGS",
  twoplots=TRUE, transform=FALSE, png=FALSE, pdf=FALSE, oldctl=NULL,
  newctl=NULL, pwidth=7, pheight=7, punits="in", ptsize=12, res=300,
  cex.main=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    Object created using \code{\link{SS_output}} }
  \item{verbose}{ Controls the amount of output to the screen.
    Default=FALSE. }
  \item{startvalues}{ A vector of 5 values for the starting points in
    the minimization. Default=NULL. }
  \item{method}{ A method to apply to the 'optim' function. See ?optim
    for options. Default="BFGS". }
  \item{twoplots}{ Make a two-panel plot showing devs as well as
    transformed uncertainty, or just the second panel in the set?
    Default=TRUE.}
  \item{transform}{ An experimental option to treat the transform the 5
    quantities to improve minimization. Doesn't work well. Default=FALSE. }
  \item{png}{ Send plot to PNG file? Default=FALSE. }
  \item{pdf}{ Send plot to PDF file? Default=FALSE. }
  \item{oldctl}{ Optional name of existing control file to modify.
    Default=NULL. }
  \item{newctl}{ Optional name of new control file to create from old
    file with estimated bias adjustment values. Default=NULL. }
  \item{pwidth}{ Default width of plots printed to files in units of \code{punits}. Default=7. }
  \item{pheight}{ Default height width of plots printed to files in units of \code{punits}. Default=7. }
  \item{punits}{ Units for \code{pwidth} and \code{pheight}. Can be "px" (pixels), "in" (inches), "cm" or "mm". Default="in". }
  \item{ptsize}{ Point size for plotted text in plots printed to files (see help("png") in R for details). Default=12. }
  \item{res}{ Resolution of plots printed to files. Default=300. }
  \item{cex.main}{ Character expansion for plot titles. }
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
\references{
  Methot, R.D. and Taylor, I.G., 2010. Modeling the variability of recruitment in fishery assessment models. In review.
}
\author{
Ian Taylor
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{SS_output}}
}
% \examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.

% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ data }
\keyword{ manip }
\keyword{ hplot }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
