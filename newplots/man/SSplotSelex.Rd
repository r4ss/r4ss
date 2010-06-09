\name{SSplotSelex}
\alias{SSplotSelex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot selectivity
}
\description{
  Plot selectivity, including retention and other quantities, with
  additional plots for time-varying selectivity.
}
\usage{
SSplotSelex(replist, fleets = "all", fleetnames = "default", selexlines
= 1:5, subplot = 1:9, plot = TRUE, print = FALSE, add = FALSE, labels
= c("Length (cm)", "Age (yr)", "Year", "Selectivity", "Retention"),
pwidth = 7, pheight = 7, punits = "in", res = 300, ptsize = 12,
cex.main = 1, plotdir = "default", verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{SSoutput}
  }
  \item{fleets}{
    optional vector to subset fleets for which to make plots
  }
  \item{fleetnames}{
    optional replacement for fleenames used in data file
  }
  \item{selexlines}{
    vector to select which lines get plotted. values are 1. Selectivity,
    2. Retention, 3. Discard mortality, 4. Keep = Sel*Ret, 5. Dead =
    Sel*(Ret+(1-Ret)*Mort).
  }
  \item{subplot}{
    vector controlling which subplots to create
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{add}{
    add to existing plot (not yet implemented)
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
