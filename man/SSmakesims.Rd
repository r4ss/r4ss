\name{SSmakesims}
\alias{SSmakesims}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make files and set up folders for simulations
}
\description{
  This is less general than other functions in this package, but is put
  here for Ian Taylor's convenience and may be useful for others,
  especially if they adapt it to their own needs
}
\usage{
SSmakesims(copymasters=TRUE, makecases=FALSE, olddir=NULL,
newdir=NULL, make4=FALSE,
exe="ss3.exe", steep=FALSE, M=FALSE, CV=FALSE, trend=FALSE,
morphratio=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{copymasters}{ Switch for whether to copy master data and
    control files to new directory. Default=TRUE. }
  \item{makecases}{ Switch for whether to create ctl files for
    individual cases. Default=FALSE. }
  \item{olddir}{ Optional old directory from which to copy files.
    Default = NULL = current working directory.}
  \item{newdir}{ Optional new directory to put files in. Default = NULL
    = current working directory. }
  \item{make4}{ Make 4 cases that Ian used for his analysis? }
  \item{exe}{ Name of Stock Synthesis executable. Default="ss3.exe". }
  \item{steep}{ Estimate steepness? Default=FALSE. }
  \item{M}{ Estimate natural mortality? Default=FALSE. }
  \item{CV}{ Estimate CV in growth? Default=FALSE. }
  \item{trend}{ Create files for trend in selectivity? Default=FALSE. }
  \item{morphratio}{ Ratio of variability within to between growth
    morphs (platoons). }
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
}
\author{
Ian Taylor
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SSgetoutput}}
  \code{\link{SSsummarize}}
  \code{\link{SSrunsims}}
}
% \examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.

% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ data }
\keyword{ manip }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
