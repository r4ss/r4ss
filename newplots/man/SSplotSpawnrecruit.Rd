\name{SSplotSpawnrecruit}
\alias{SSplotSpawnrecruit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot spawner-recruit curve.
}
\description{
  Plot spawner-recruit curve based on output from Stock Synthesis model.
}
\usage{
SSplotSpawnrecruit(replist, add = F, plot = T, print = F, xlim = NULL,
ylim = NULL, xlab = "Spawning biomass (mt)", ylab = "Recruitment
(1,000s)", plotdir = "default", pwidth = 7, pheight = 7, punits = "in",
res = 300, ptsize = 12, cex.main = 1, verbose = T, line1 = "blue", line2
= "green", line3 = "black", ptcol = "red", virg = T, init = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{SSoutput}
  }
  \item{add}{
    add to existing plot?
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{xlim}{
    optional control of x range
  }
  \item{ylim}{
    optional control of y range
  }
  \item{xlab}{
    x-axis label
  }
  \item{ylab}{
    y-axis label
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
  \item{line1}{
    first line color
  }
  \item{line2}{
    second line color
  }
  \item{line3}{
    third line color
  }
  \item{ptcol}{
    point color
  }
  \item{virg}{
    add point for equilibrium conditions (x=B0,y=R0)
  }
  \item{init}{
    add point for initial conditions (x=B1,y=R1)
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
