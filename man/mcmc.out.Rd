\name{mcmc.out}
\alias{mcmc.out}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Summarize, analyze and plot key MCMC output.
}
\description{
  Makes four panel plot showing trace plots, moving average,
  autocorrelations, and densities for chosen parameters from MCMC
  output.
}
\usage{
mcmc.out(directory = "c:/mydirectory/", run = "mymodel/",
file = "keyposteriors.csv", namefile = "postplotnames.sso",
names = FALSE, headernames = TRUE, numparams = 1, closeall = TRUE,
burn = 0, thin = 1, scatter = FALSE, surface = FALSE, surf1 = 1,
surf2 = 2, stats = FALSE, plots = TRUE, header = TRUE, sep = ",",
print = FALSE)
}
\arguments{
  \item{directory}{
    Directory where all results are located, one level above directory
    for particular run.
  }
  \item{run}{
    Directory with files from a particular run.
  }
  \item{file}{
    File containing posterior samples for key parameters. This could be
    written by the function \code{\link{SSgetMCMC}}.
  }
  \item{namefile}{
    The (optional) file name of the dimension and names of posteriors.
  }
  \item{names}{
    Read in names file (T) or use generic naming (F).
  }
  \item{headernames}{
    Use the names in the header of \code{file}?
  }
  \item{numparams}{
    The number of parameters to analyze.
  }
  \item{closeall}{
    By default close all open devices.
  }
  \item{burn}{
    Optional burn-in value to apply on top of the option in the
    starter file and \code{\link{SSgetMCMC}}.
  }
  \item{thin}{
    Optional thinning value to apply on top of the option in the
    starter file, in the \code{-mcsave} runtime command, and in
    \code{\link{SSgetMCMC}}.
  }
  \item{scatter}{
    Can add a scatter-plot of all params at end, default is none.
  }
  \item{surface}{
    Add a surface plot of 2-way correlations.
  }
  \item{surf1}{
    The first parameter for the surface plot.
  }
  \item{surf2}{
    The second parameter for the surface plot.
  }
  \item{stats}{
    Print stats if desired.
  }
  \item{plots}{
    Show plots or not.
  }
  \item{header}{
    Data file with header?
  }
  \item{sep}{
    Separator for data file passed to the \code{read.table} function.
  }
  \item{print}{
    Send to screen unless asked to print.
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
  Ian Stewart
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{mcmc.nuisance}}, \code{\link{SSgetMCMC}}
}
% \examples{
%   \dontrun{
%   }
% }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
