\name{SS_splitdat}
\alias{SS_splitdat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Split apart bootstrap data to make input file. }
\description{
  A function to split apart bootstrap data files created in data.ss_new.
  To get bootstraps, the input "N bootstrap file to produce" in
  starter.ss needs to be 3 or greater. 
}
\usage{
SS_splitdat(inpath = "working_directory",
outpath = "working_directory", inname = "data.ss_new",
outpattern = "BootData", number = FALSE,
verbose = TRUE, fillblank = TRUE, MLE = TRUE,
inputs = FALSE, notes = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inpath}{ Directory containing the input file. By default the working directory given by getwd() is used. Default="working_directory".}
  \item{outpath}{ Directory into which the output file will be written. Default="working_directory".}
  \item{inname}{ File name of input data file to be split. Default="Data.SS_New".}
  \item{outpattern}{ File name of output data file. Default="BootData".}
  \item{number}{ Append bootstrap number to the file name chosen in \code{outpattern}? Default=F.}
  \item{verbose}{ Provide richer command line info of function progress? Default=TRUE.}
  \item{fillblank}{ Replace blank lines with "#". Helps with running on linux. Default=TRUE.}
  \item{MLE}{ Grab the maximum likelihood values from the second block
    in Data.SS_New (instead of bootstrap values or copies of inputs)? Default=TRUE.}
  \item{inputs}{ Grab the copy of the input values values from the first
    block in Data.SS_New (instead of MLE or bootstrap values)? Default=F.}
  \item{notes}{ Notes to the top of the new file (comment indicator "#C" will be added). Default="".}
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
