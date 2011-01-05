\name{SS_changepars}
\alias{SS_changepars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Change parameters in the control file. }
\description{
  A function to take advantage of \code{\link{SS_parlines}} that could be
  used to create a series of control files with different parameter
  values. This is used by \code{\link{SS_profile}}, but may also be useful
  for simulation work.
}
\usage{
SS_changepars(dir = "C:/myfiles/mymodels/myrun/",
ctlfile = "control.ss_new", newctlfile = "control_modified.ss",
linenums = NULL, strings = NULL, newvals = NULL, estimate = FALSE,
verbose = TRUE)  
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{ Directory with control file to change. }
  \item{ctlfile}{ Control file name. Default="control.ss_new". }
  \item{newctlfile}{ Name of new control file to be written.
    Default="control_modified.ss". }
  \item{linenums}{ Line numbers of control file to be modified. Either
    this or the Strings input are needed. Default=NULL. }
  \item{strings}{ Strings (with optional partial matching) indicating
    which parameters to be modified. This is an alternative to linenums.
    Strings correspond to the commented parameter names included in
    control.ss_new, or whatever is written as comment at the end of the
    14 number parameter lines. Default=NULL. }
  \item{newvals}{ Vector of new parameter values. Default=NULL. }
  \item{estimate}{ Vector of TRUE/FALSE for which changed parameters are
    to be estimated. Default=FALSE. }
  \item{verbose}{ More detailed output to command line. Default=TRUE. }
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
\seealso{
  \code{\link{SS_parlines}}, \code{\link{SS_profile}}
}
\examples{
\dontrun{
SS_changepars(dir='Y:/ss/SSv3.03a/Simple/',ctlfile='Control.SS_New',
              strings=c('SR_steep','SR_sigmaR'),newvals=c(.35,.6))
# [1] wrote new file to Control_Modified.SS
#    oldvals newvals oldphase newphase     comment
# 1 0.609048    0.35        4       -4  # SR_steep
# 2 0.600000    0.60       -4       -4 # SR_sigmaR
}
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ data }
\keyword{ manip }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
