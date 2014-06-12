\name{SSgetMCMC}
\alias{SSgetMCMC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Read MCMC output.
}
\description{
  Reads the MCMC output (in the posteriors.sso and
  derived_posteriors.sso files) from one or more models.
}
\usage{
SSgetMCMC(dir = NULL, verbose = TRUE, writecsv = FALSE,
csv1 = "keyposteriors.csv", csv2 = "nuisanceposteriors.csv",
keystrings = c("NatM", "R0", "steep", "RecrDev_2008", "Q_extraSD"),
nuisancestrings = c("Objective_function", "SPB_", "InitAge", "RecrDev"),
modelnames = "default", burnin = 0, thin = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{
    A string (or vector of strings) of the directory (or directories)
    with MCMC output.
  }
  \item{verbose}{
    TRUE/FALSE switch to get more or less information about the progress
    of the function.
  }
  \item{writecsv}{
    Write key parameters and certainty nuisance quantities to a CSV file.
  }
  \item{csv1}{
    First CSV file for key parameters.
  }
  \item{csv2}{
    Second CSV file for nuisance quantities.
  }
  \item{keystrings}{
    Vector of strings that partially match parameter names to write to
    the file csv1. This file intended to feed into
    \code{\link{mcmc.out}}.
  }
  \item{nuisancestrings}{
    Vector of strings that partially match derived quantity names to
    write to the file csv2. This file intended to feed into
    \code{\link{mcmc.nuisance}}.
  }
  \item{modelnames}{
    Either "default" or a vector of names to use in naming elements of
    list that is output by the function. Default is "model1", "model2",
    etc.
  }
  \item{burnin}{
    Optional burn-in value to apply on top of the option in the
    starter file.
  }
  \item{thin}{
    Optional thinning value to apply on top of the option in the
    starter file and in the \code{-mcsave} runtime command.
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
  Ian Taylor
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{mcmc.out}}, \code{\link{mcmc.nuisance}}, \code{\link{SSplotPars}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
