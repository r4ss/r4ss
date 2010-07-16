\name{SSplotIndices}
\alias{SSplotIndices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot indices of abundance and associated quantities.
}
\description{
  Plot indices of abundance and associated quantities.
}
\usage{
SSplotIndices(replist, subplots = 1:7, plot = TRUE, print = FALSE,
fleets = "all", fleetnames = "default",  smooth = TRUE, add = FALSE,
datplot = FALSE, labels = c("Year", "Index", "Observed index",
"Expected index","Log index", "Log observed index",
"Log expected index", "Standardized index"), pwidth = 7, pheight = 7,
punits = "in", res = 300, ptsize = 12,
cex.main = 1, plotdir = "default", verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{SSoutput}
  }
  \item{subplots}{
    vector controlling which subplots to create
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{fleets}{
    optional vector to subset fleets for which plots will be made
  }
  \item{fleetnames}{
    optional replacement for fleenames used in data file
  }
  \item{smooth}{
    add smoothed line to plots of observed vs. expected sample sizes
  }
  \item{add}{
  add to existing plot (not yet implemented)
  }
  \item{datplot}{
    make plot of data only?
  }
  \item{labels}{
    vector of labels for plots (titles and axis labels)
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
  \item{plotdir}{
    directory where PNG files will be written. by default it will
    be the directory where the model was run.
  }
  \item{verbose}{
    report progress to R GUI?
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}
\references{
  %% ~put references to the literature/web site here ~
}
\author{
  Ian Stewart, Ian Taylor
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
