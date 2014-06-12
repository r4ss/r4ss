\name{SS_recdevs}
\alias{SS_recdevs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Insert a vector of recruitment deviations into the control
file. }
\description{
A function to insert a vector of recruitment deviations into the control
file for simulation studies. This can also be achieved by using the .par
file, but Ian Taylor prefers this approach for no good reason.}
\usage{
SS_recdevs(fyr, lyr, ctl = NULL, recdevs = NULL,
rescale = TRUE, scaleyrs = NULL, dir = "working_directory",
ctlfile = "control.ss_new", newctlfile = "control_modified.ss",
verbose = TRUE, writectl = TRUE, returnctl = FALSE, newmaxbias = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fyr}{First year of the recdev vector.}
  \item{lyr}{Last year of the recdev vector.}
  \item{ctl}{Either NULL to read anew or an already read control file.
    Default=NULL.}
  \item{recdevs}{Either NULL to generate anew or an already generated
    vector of recdevs. Default=NULL.}
  \item{rescale}{Should the recdevs be rescaled to have mean = 0 and
    std. deviation = sigmaR? Default=TRUE.}
  \item{scaleyrs}{Vector of years over which rescaling (if chosen)
    should occur.}
  \item{dir}{Directory where files are located. Default is to use the
    working directory in use by R. Default="working_directory".}
  \item{ctlfile}{Name of control file to modify.
    Default="control.ss_new".}
  \item{newctlfile}{Name of new file to output modified control file.
    Default="control_modified.ss".}
  \item{verbose}{Verbose output to R command line? Default=TRUE.}
  \item{writectl}{Write new file? Default=TRUE.}
  \item{returnctl}{Return contents ctl file as an object in the R
    workspace. Default=FALSE.}
  \item{newmaxbias}{Replace the maximum bias adjustment fraction with
    any non-NULL value. Default=NULL.}
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
% \examples{}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ data }
\keyword{ manip }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
