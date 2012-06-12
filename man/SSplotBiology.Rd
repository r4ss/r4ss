\name{SSplotBiology}
\alias{SSplotBiology}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot biology related quantities.
}
\description{
  Plot biology related quantities from Stock Synthesis model output,
  including mean weight, maturity, fecundity, and spawning output.

}
\usage{
SSplotBiology(replist,
 plot=TRUE,print=FALSE,add=FALSE,subplots=1:10,seas=1,
 col1="red",col2="blue",
 legendloc="topleft",
 plotdir="default",
 labels=c("Length (cm)",               
   "Age (yr)",                         
   "Maturity",                         
   "Mean weight (kg) in last year",    
   "Spawning output",                  
   "Length (cm, middle of the year)",  
   "Natural mortality",                
   "Female weight (kg)",               
   "Female length (cm)",               
   "Fecundity",                        
   "Default fecundity label"),         
 pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
 verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{add}{
    add to existing plot
  }
  \item{subplots}{
    vector controlling which subplots to create
  }
  \item{seas}{
    which season to plot (obviously only works in seasonal models, but
    maybe not fully implemented even then)
  }
  \item{col1}{
    color of some points/lines
  }
  \item{col2}{
    color of other points/lines
  }
  \item{legendloc}{
    location of legend (see ?legend for more info)
  }
  \item{plotdir}{
    directory where PNG files will be written. by default it will
    be the directory where the model was run.
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
  \item{verbose}{
    return updates of function progress to the R GUI?
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
\keyword{ aplot }
\keyword{ hplot }% __ONLY ONE__ keyword per line
