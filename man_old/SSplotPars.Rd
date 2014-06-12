\name{SSplotPars}
\alias{SSplotPars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot distributions of priors, posteriors, and estimates. }
\description{
  Make multi-figure plots of prior, posterior, and estimated asymptotic
  parameter distributions. MCMC not required to make function work.
}
\usage{
SSplotPars(dir = "c:/path/", repfile = "Report.sso",
xlab="Parameter value",ylab="Density",
postfile = "posteriors.sso", showpost = TRUE, showprior = TRUE,
showmle = TRUE, showinit = TRUE, showrecdev = TRUE, priorinit = TRUE,
priorfinal = TRUE, showlegend = TRUE, fitrange = FALSE, xaxs="i",
xlim=NULL, ylim=NULL, verbose = TRUE, nrows = 3, ncols = 3,
new = TRUE, pdf = FALSE, pwidth = 7, pheight = 7, punits="in",
ptsize = 12, returntable = FALSE, strings = c(), exact = FALSE,
newheaders=NULL, burn = 0, thin = 1,
ctlfile = "control.ss_new")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{ Directory where all files are located. }
  \item{repfile}{ Name of report file. Default="Report.sso".}
  \item{xlab}{ Label on horizontal axis. }
  \item{ylab}{ Label on vertical axis. }
  \item{postfile}{ Name of MCMC posteriors file (not required).
    Default="posteriors.sso".}
  \item{showpost}{ Show posterior distribution as bar graph? Default=TRUE. }
  \item{showprior}{ Show prior distribution as black line? Default=TRUE.}
  \item{showmle}{ Show MLE estimate and asymptotic variance estimate
    with blue lines? Default=TRUE.}
  \item{showinit}{ Show initial value as red triangle? Default=TRUE.}
  \item{showrecdev}{ Include recdevs in the plot? Default=TRUE.}
  \item{priorinit}{ TRUE/FALSE for prior probability at initial value (not
    implemented).}
  \item{priorfinal}{ TRUE/FALSE for prior probability at final value (not
    implemented).}
  \item{showlegend}{ Show the legend? Default=TRUE.}
  \item{fitrange}{ Fit range tightly around MLE & posterior
    distributions, instead of full parameter range? Default=FALSE.}
  \item{xaxs}{ Parameter input for x-axis. See \code{?par} for more
    info. Default="i".}
  \item{xlim}{ Optional x-axis limits to be applied to all plots.
    Otherwise, limits are based on the model results. Default=NULL.}
  \item{ylim}{ Optional y-axis limits to be applied to all plots.
    Otherwise, limits are based on the model results. Default=NULL.}
  \item{verbose}{ Controls amount of text output (maybe). Default=TRUE.}
  \item{nrows}{ How many rows in multi-figure plot. Default=3.}
  \item{ncols}{ How many columns in multi-figure plot. Default=3.}
  \item{new}{ Open new window for plotting? Default=TRUE.}
  \item{pdf}{ Write to PDF file instead of R GUI? Default=FALSE.}
  \item{pwidth}{ Default width of plots printed to files in units of
    \code{punits}. Default=7. }
  \item{pheight}{ Default height width of plots printed to files in
    units of \code{punits}. Default=7. }
  \item{punits}{ Units for \code{pwidth} and \code{pheight}. Can be "px"
    (pixels), "in" (inches), "cm" or "mm". Default="in". }
  \item{ptsize}{ Point size for plotted text in plots printed to files
    (see help("png") in R for details). Default=12. }
  \item{returntable}{ Return table of parameter info? Default=FALSE.}
  \item{strings}{ Subset parameters included in the plot using substring
    from parameter names (i.e. "SR" will get "SR_R0" and "SR_steep" if
    they are both estimated quantities in this model). Default=c().}
  \item{exact}{ Should strings input match parameter names exactly?
    Otherwise substrings are allowed. Default=FALSE.}
  \item{newheaders}{ Optional vector of headers for each panel to
    replace the parameter names. Default=NULL.}
  \item{burn}{ Additional burn-in applied to MCMC posteriors. Default=0.}
  \item{thin}{ Additional thinning applied to MCMC posteriors. Default=1.}
  \item{ctlfile}{ Specify control file to get min and max recdev values
    (otherwise assumed to be -5 and 5). Default="control.ss_new".}
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
pars <- SSplotPars(dir='c:/SS/Simple/')

# strings can be partial match
pars <- SSplotPars(dir='c:/SS/Simple/',strings=c("steep"))
}
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ hplot }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
