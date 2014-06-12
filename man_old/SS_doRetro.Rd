\name{SS_doRetro}
\alias{SS_doRetro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Run retrospective analyses }
\description{
  Do retrospective analyses by creating new directories, copying model
  files, and iteratively changing the starter file to set the number of
  years of data to exclude.
}
\usage{
SS_doRetro(masterdir, oldsubdir, newsubdir='retrospectives',
subdirstart='retro', years=0:-5, overwrite=TRUE, extras="-nox",
intern=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{masterdir}{
    Directory where everything takes place.
  }
  \item{oldsubdir}{
    Subdirectory within \code{masterdir} with existing model files.
  }
  \item{newsubdir}{
    Subdirectory within \code{masterdir} where retrospectives will be
    run. Default is 'retrospectives'.
  }
  \item{subdirstart}{
    First part of the pattern of names for the directories in which the
    models will actually be run.
  }
  \item{years}{
    Vector of values to iteratively enter into the starter file for
    retrospective year. Should be zero or negative values.
  }
  \item{overwrite}{
    Overwrite any input files with matching names in the subdirectories
    where models will be run.
  }
  \item{extras}{
    Additional commands to use when running SS. Default = "-nox" will
    reduce the amound of command-line output.
  }
  \item{intern}{
    Display runtime information from SS in the R console (vs. saving to
    a file).
  }
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
%% \note{
%% }
\seealso{
  \code{\link{SSgetoutput}}
}
\examples{
  \dontrun{
    # note: don't run this in your main directory--make a copy in case something goes wrong
    mydir <- "C:/Simple"

    ## retrospective analyses
    SS_doRetro(masterdir=mydir, oldsubdir="", newsubdir="retrospectives", years=0:-5)
    
    retroModels <- SSgetoutput(dirvec=file.path(mydir, "retrospectives",paste("retro",0:-5,sep="")))
    retroSummary <- SSsummarize(retroModels)
    endyrvec <- retroSummary$endyrs + 0:-5
    SSplotComparisons(retroSummary, endyrvec=endyrvec, legendlabels=paste("Data",0:-5,"years"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ manip }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
