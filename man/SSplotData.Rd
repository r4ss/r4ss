\name{SSplotData}
\alias{SSplotData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Timeline of presence/absence of data by type, year, and fleet.
}
\description{
  Plot shows graphical display of what data is being used in the model.
  Some data types may not yet be included. Note, this is based on output
  from the model, not the input data file.
}
\usage{
SSplotData(replist, plot=TRUE,print=FALSE,
  plotdir="default", fleetcol="default",
  datatypes="all",fleets="all",fleetnames="default",ghost=FALSE,
  pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
  margins = c(5.1,2.1,4.1,8.1), verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{\link{SS_output}}
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{plotdir}{
    directory where PNG files will be written. by default it will
    be the directory where the model was run.
  }
  \item{fleetcol}{
    Either the string "default", or a vector of colors
    to use for each fleet.
  }
  \item{datatypes}{
    Either the string "all", or a vector including some subset of the
    following: "catch", "cpue", "lendbase", "sizedbase", "agedbase",
    "condbase", "ghostagedbase", "ghostcondbase", "ghostlendbase",
    "ladbase", "wadbase", "mnwgt", "discard", "tagdbase1", "tagdbase2".
  }
  \item{fleets}{
    Either the string "all", or a vector of numerical values, like
    c(1,3), listing fleets or surveys to be included in the plot.
  }
  \item{fleetnames}{
    A vector of alternative names to use in the plot. By default the
    parameter names in the data file are used.
  }
  \item{ghost}{
    TRUE/FALSE indicator for whether to show presence of composition
    data from ghost fleets (data for which the fit is shown, but is not
    included in the likelihood calculations).
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
  \item{margins}{
    margins of plot (passed to par() function), which may need to be
    increased if fleet names run off right-hand margin
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
  Ian Taylor, Chantel Wetzel
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_plots}}, \code{\link{SS_output}}, \code{\link{SS_readdat}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
