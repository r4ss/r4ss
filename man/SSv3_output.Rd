\name{SSv3_output}
\alias{SSv3_output}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Old output reader}
\description{
  Deprecated function has since been replaced by \code{\link{SS_output}}
}
\usage{
SSv3_output(dir = "C:/myfiles/mymodels/myrun/", model = "ss3",
repfile = "Report.sso", compfile = "CompReport.sso",
covarfile = "covar.sso", ncols = 200, forecast = TRUE, warn = TRUE,
covar = TRUE, checkcor = TRUE, cormax = 0.95, cormin = 0.01,
printhighcor = 10, printlowcor = 10, verbose = TRUE,
printstats = TRUE, hidewarn = FALSE, NoCompOK = FALSE,oldcode=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{ Locates the directory of the files to be read in, double backslashes (or forwardslashes) and quotes necessary. }
  \item{model}{ Name of the executable (leaving off the .exe). Deafult="ss3"}
  \item{repfile}{ Name of the big report file (could be renamed by
    user). Default="Report.sso".}
  \item{compfile}{ Name of the composition report file. Default="CompReport.sso".}
  \item{covarfile}{ Name of the covariance output file. Default="covar.sso".}
  \item{ncols}{ The maximum number of columns in files being read in.
    If this value is too big the function runs more slowly, too small
    and errors will occur.  A warning will be output to the R command
    line if the value is too small. It should be bigger than the maximum
    age + 10 and the number of years + 10. Default=200.}
  \item{forecast}{ Read the forecast-report file? Default=TRUE. }
  \item{warn}{ Read the Warning.sso file? Default=TRUE. }
  \item{covar}{ Read covar.sso to get variance information and identify
    bad correlations? Default=TRUE. }
  \item{checkcor}{ Check for bad correlations? Default=TRUE. }
  \item{cormax}{ The specified threshold for defining high correlations.
    A quantity with any correlation above this value is identified. Default=0.95. }
  \item{cormin}{ The specified threshold for defining low correlations.
    Only quantities with all correlations below this value are
    identified (to find variables that appear too independent from the
    model results). Default=0.01.}
  \item{printhighcor}{ The maximum number of high correlations to print to the R GUI. Default=10.}
  \item{printlowcor}{ The maximum number of low correlations to print to the R GUI. Default=10.}
  \item{verbose}{ Return updates of function progress to the R GUI?
    Default=TRUE.}
  \item{printstats}{ Print summary statistics about the output to the R GUI? Default=TRUE.}
  \item{hidewarn}{ Hides some warnings output from the R GUI. Default=FALSE.}
  \item{NoCompOK}{ Allow the function to work without a CompReport file.
    Default=FALSE.}
  \item{oldcode}{ Allow the function to run in spite of the fact that
    it's really out of date. Default=FALSE.}
}
% \details{

% }
\value{
  % Many values are returned. Complete list would be quite long, but
  % should probably be created at some point in the future.
}
% \references{ ~put references to the literature/web site here ~ }
\author{ Ian Stewart, Ian Taylor }
% \note{ ~~further notes~~

%  % ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
% \dontrun{
% myreplist <- SSv3_output(dir='c:/SS/SSv3.02d/Simple/')
% }
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
% \keyword{ data }
% \keyword{ manip }
% \keyword{ list }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
