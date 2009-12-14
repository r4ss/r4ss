\name{SSv3_plotpars}
\alias{SSv3_plotpars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot distributions of priors, posteriors, and estimates. }
\description{
  Make multi-figure plots of prior, posterior, and estimated asymptotic parameter distributions. MCMC not required to make function work.
}
\usage{
SSv3_plotpars(dir = "c:/path/", repfile = "Report.sso",
postfile = "posteriors.sso", showpost = T, showprior = T,
showmle = T, showinit = T, showrecdev = T, priorinit = T,
priorfinal = T, showlegend = T, fitrange = F, xaxs="i",
verbose = T, nrows = 3, ncols = 3, new = T, pdf = F,
pwidth = 7, pheight = 7, punits="in", ptsize = 12,
returntable = F, strings = c(), burn = 0, thin = 1,
ctlfile = "control.ss_new")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{ Directory where all files are located. }
  \item{repfile}{ Name of report file. Default="Report.sso".}
  \item{postfile}{ Name of MCMC posteriors file (not required).  Default="posteriors.sso".}
  \item{showpost}{ Show posterior distribution as bar graph? Default=T. }
  \item{showprior}{ Show prior distribution as black line? Default=T.}
  \item{showmle}{ Show MLE estimate and asymptotic variance estimate with blue lines? Default=T.}
  \item{showinit}{ Show initial value as red triangle? Default=T.}
  \item{showrecdev}{ Include recdevs in the plot? Default=T.}
  \item{priorinit}{ T/F for prior probability at initial value (not
    implemented).}
  \item{priorfinal}{ T/F for prior probability at final value (not
    implemented).}
  \item{showlegend}{ Show the legend? Default=T.}
  \item{fitrange}{ Fit range tightly around MLE & posterior distributions, instead of full parameter range? Default=F.}
  \item{xaxs}{ Parameter input for x-axis. See \code{?par} for more info. Default="i".}
  \item{verbose}{ Controls amount of text output (maybe). Default=T.}
  \item{nrows}{ How many rows in multi-figure plot. Default=3.}
  \item{ncols}{ How many columns in multi-figure plot. Default=3.}
  \item{new}{ Open new window for plotting? Default=T.}
  \item{pdf}{ Write to PDF file instead of R GUI? Default=F.}
  \item{pwidth}{ Default width of plots printed to files in units of \code{punits}. Default=7. }
  \item{pheight}{ Default height width of plots printed to files in units of \code{punits}. Default=7. }
  \item{punits}{ Units for \code{pwidth} and \code{pheight}. Can be "px" (pixels), "in" (inches), "cm" or "mm". Default="in". }
  \item{ptsize}{ Point size for plotted text in plots printed to files (see help("png") in R for details). Default=12. }
  \item{returntable}{ Return table of parameter info? Default=F.}
  \item{strings}{ Subset parameters included in the plot using substring from parameter names (i.e. "SR" will get "SR_R0" and "SR_steep" if they are both estimated quantities in this model). Default=c().}
  \item{burn}{ Additional burn-in applied to MCMC posteriors. Default=0.}
  \item{thin}{ Additional thinning applied to MCMC posteriors. Default=1.}
  \item{ctlfile}{ Specify control file to get min and max recdev values (otherwise assumed to be -5 and 5). Default="control.ss_new".}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%   \item{comp1 }{Description of 'comp1'}
%   \item{comp2 }{Description of 'comp2'}
%   ...
% }
% \references{ ~put references to the literature/web site here ~ }
\author{ Ian Taylor }
% \note{ ~~further notes~~ 

%  % ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
\dontrun{
pars <- SSv3_plotpars(dir='c:/SS/Simple/')

# strings can be partial match
pars <- SSv3_plotpars(dir='c:/SS/Simple/',strings=c("steep"))
}
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ dplot }
\keyword{ hplot }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
