\name{mcmc.nuisance}
\alias{mcmc.nuisance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Summarize nuisance MCMC output
}
\description{
  Summarize nuisance MCMC output (used in combination with
  \code{\link{mcmc.out}} for key parameters).

}
\usage{
mcmc.nuisance(directory = "c:/mydirectory/", run = "mymodel/",
file = "posteriors.sso", file2 = "derived_posteriors.sso",
bothfiles = FALSE, printstats = FALSE, burn = 0, header = TRUE, thin = 1,
trace = 0, labelstrings = "all", columnnumbers = "all", sep = "")
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
    File containing posterior samples for nuisance parameters. This
    could be posteriors.sso or something written by the function
    \code{\link{SSgetMCMC}}.
  }
  \item{file2}{
    Optional second file containing posterior samples for nuisance
    parameters. This could be derived_posteriors.sso.
  }
  \item{bothfiles}{
    TRUE/FALSE indicator on whether to read \code{file2} in addition to
    \code{file1}.
  }
  \item{printstats}{
    Return all the statistics for a closer look.
  }
  \item{burn}{
    Optional burn-in value to apply on top of the option in the
    starter file and \code{\link{SSgetMCMC}}.
  }
  \item{header}{
    Data file with header?
  }
  \item{thin}{
    Optional thinning value to apply on top of the option in the
    starter file, in the \code{mcsave} runtime command, and in
    \code{\link{SSgetMCMC}}.
  }
  \item{trace}{
    Plot trace for param # (to help sort out problem parameters).
  }
  \item{labelstrings}{
    Vector of strings that partially match the labels of the parameters
    you want to consider.
  }
  \item{columnnumbers}{
    Vector of column numbers indicating the columns you want to consider.
  }
  \item{sep}{
    Separator for data file passed to the \code{read.table} function.
  }
}
% \details{
%   %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
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
  \code{\link{mcmc.out}}, \code{\link{SSgetMCMC}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
