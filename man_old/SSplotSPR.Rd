\name{SSplotSPR}
\alias{SSplotSPR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot SPR quantities.
}
\description{
  Plot SPR quantities, including 1-SPR and phase plot.
}
\usage{
SSplotSPR(replist, add = FALSE, plot = TRUE, print = FALSE, uncertainty
= TRUE, subplots = 1:4, col1 = "black", col2 = "blue", col3 = "green3",
col4 = "red", sprtarg = "default", btarg = "default",
labels = c("Year", "SPR", "1-SPR"), pwidth = 7, pheight = 7,
punits = "in", res = 300, ptsize = 12, cex.main = 1,
plotdir = "default", verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{SSoutput}
  }
  \item{add}{
    add to existing plot (not yet implemented)
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{uncertainty}{
    include plots showing uncertainty?
  }
  \item{subplots}{
    vector controlling which subplots to create
  }
  \item{col1}{
    first color used
  }
  \item{col2}{
    second color used
  }
  \item{col3}{
    third color used
  }
  \item{col4}{
    fourth color used
  }
  \item{sprtarg}{
    F/SPR proxy target. "default" chooses based on model output.
  }
  \item{btarg}{
    target depletion to be used in plots showing depletion. May be
    omitted by setting to NA. "default" chooses based on model output.
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
% \details{
%   %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
%   %%  ~Describe the value returned
%   %%  If it is a LIST, use
%   %%  \item{comp1 }{Description of 'comp1'}
%   %%  \item{comp2 }{Description of 'comp2'}
%   %% ...
% }
% \references{
%   %% ~put references to the literature/web site here ~
% }
\author{
  Ian Stewart, Ian Taylor
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_plots}}, \code{\link{SS_output}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
