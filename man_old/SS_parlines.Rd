\name{SS_parlines}
\alias{SS_parlines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Get parameter lines from Stock Synthesis control file }
\description{
  A simple function which takes as input the full path and filename of a
  control file for input to Stock Synthesis. Ideally, a Control.SS_New
  file will be used, so that it represents what SS thinks the inputs
  are, and not what the user thinks the inputs are.

  It returns a table which should contain one line for each
  parameter in the model. Currently, only the first 7 values are
  returned, because all parameters have those values. In the future,
  extended parameter lines could be returned.

  Parameter lines are identified as those which have 7 or 14 numeric
  elements followed by a non-numeric element. It's possible that this
  system could break down under certain circumstances
}
\usage{
  SS_parlines(ctlfile = "C:/myfiles/mymodels/myrun/control.ss_new",
  dir=NULL, verbose = T, active = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ctlfile}{ File name of control file including path. }
  \item{dir}{ Alternative input of path, where file is assumed to be
  "control.ss_new". Default=NULL.}
  \item{verbose}{ T/F switch for amount of detail produced by function.
  Default=T.}
  \item{active}{ Should only active parameters (those with positive
  phase) be output? Default=F. }
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
  \code{\link{SS_changepars}}
}
\examples{
\dontrun{
x <- SS_parlines(ctlfile='Y:/ss/SSv3.03a/Simple/Control.SS_New')
head(x)
#       LO    HI     INIT PRIOR PR_type   SD PHASE              Label Line_num
# 42  0.05  0.15  0.10000  0.10       0  0.8    -3  NatM_p_1_Fem_GP_1       42
# 43  0.05  0.15  0.10000  0.10       0  0.8    -3  NatM_p_2_Fem_GP_1       43
# 44  1.00 45.00 32.28100 36.00       0 10.0     2 L_at_Amin_Fem_GP_1       44
# 45 40.00 90.00 71.34260 70.00       0 10.0     4 L_at_Amax_Fem_GP_1       45
# 46  0.05  0.25  0.15199  0.15       0  0.8     4 VonBert_K_Fem_GP_1       46
# 47  0.05  0.25  0.10000  0.10       0  0.8    -3  CV_young_Fem_GP_1       47
}
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ data }
\keyword{ manip }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
