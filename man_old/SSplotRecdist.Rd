\name{SSplotRecdist}
\alias{SSplotRecdist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot of recruitment distribution among areas and seasons
}
\description{
  Image plot shows fraction of recruitment in each combination of area
  and season. This is based on the RECRUITMENT_DIST section of the
  Report.sso file.
}
\usage{
SSplotRecdist(replist, plot=TRUE, print=FALSE, areanames=NULL,
seasnames=NULL, xlab="", ylab="",
main="Distribution of recruitment by area and season",
plotdir="default", pwidth=7, pheight=7, punits="in",
res=300, ptsize=12, cex.main=1, verbose=TRUE)
}
\arguments{
  \item{replist}{
    list created by \code{SS_output}
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{areanames}{
    optional vector to replace c("Area1","Area2",...)
  }
  \item{seasnames}{
    optional vector to replace c("Season1","Season2",...)
  }
  \item{xlab}{
    optional x-axis label (if the area names aren't informative enough)
  }
  \item{ylab}{
    optional y-axis label (if the season names aren't informative enough)
  }
  \item{main}{
    title for plot
  }
  \item{plotdir}{
    directory where PNG files will be written. by default it will
    be the directory where the model was run.
  }
  \item{pwidth}{
    width of plot written to PNG file
  }
  \item{pheight}{
    height of plot written to PNG file
  }
  \item{punits}{
    units for PNG file
  }
  \item{res}{
    resolution for PNG file
  }
  \item{ptsize}{
    ptsize for PNG file
  }
  \item{cex.main}{
    character expansion for plot titles
  }
  \item{verbose}{
    report progress to R GUI?
  }
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
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{
  Ian Taylor
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_plots}}, \code{\link{SSplotRecdevs}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
