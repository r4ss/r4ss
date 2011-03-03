\name{update_r4ss_files}
\alias{update_r4ss_files}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Updates r4ss files to newest versions on web. }
\description{
  Sources files containing R functions r4ss package from the google code
  repository. These may often be newer than those available form CRAN
  mirrors.
}
\usage{
update_r4ss_files(local = NULL, save = FALSE)
}
\arguments{
  \item{local}{ A local directory from which to source the files instead
  of getting them from the web.}
  \item{save}{ If TRUE, then copy files from web to local directory,
    then source from this same local directory }
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
% \seealso{
%   \code{\link{SS_parlines}}, \code{\link{SS_profile}}
% }
\examples{
\dontrun{
update_r4ss_files()
# getting file names from http://r4ss.googlecode.com/svn/trunk/ 
# most recent change: Today (6 hours ago) 
# 64 files found in http://r4ss.googlecode.com/svn/trunk/ 
#   sourcing IOTCmove.R
#   sourcing RebuildPlot.R
#   sourcing SSFishGraph.R
#   sourcing SS_changepars.R
#   sourcing SS_fitbiasramp.R
#   sourcing SS_makedatlist.R
#   ...
#   sourcing stackpoly.R
#   sourcing update_r4ss_files.R
# update complete.

# copy files from web to local directory and then source them
update_r4ss_files(local='c:/SS/R/r4ss_files/',save=T)

# source files from a local directory (i.e. if no network available)
update_r4ss_files(local='c:/SS/R/r4ss_files/',save=F)

}
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ file }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
