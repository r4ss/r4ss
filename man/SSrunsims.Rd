\name{SSrunsims}
\alias{SSrunsims}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Run a collection of simulations for Stock Synthesis.
}
\description{
  Runs simulations by looping over choices of Stock Synthesis data and
  control files, generating bootstrap data, and then running an estimation
  model using this data.
}
\usage{
SSrunsims(sims = 1, newrun = TRUE, sim = FALSE, fit = FALSE, simfolder =
"sims", fitfolder = "fits", masterfolder = "fits", MLEdata = FALSE,
skipfiles = TRUE, simchoices = 1, fitchoices = 1, samedatafile = FALSE,
homepath = "c:/myfiles/", recdevmatrix = NULL, rescale = TRUE,
fitbiasramp = FALSE, exe = "ss3_opt", simextras = "-nox -nohess",
fitextras = "-nox -gbs 1000000000 -cbs 1000000000",
fyr = NULL, lyr = NULL, printfile=TRUE, intern = FALSE, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sims}{
    Vector of simulations to run for each case.
  }
  \item{newrun}{
    Define some settings? Pretty much always required. Default=TRUE.
  }
  \item{sim}{
    Do simulations? Default = FALSE.
  }
  \item{fit}{
    Do estimations? Default = FALSE.
  }
  \item{simfolder}{
    Name of folder where simulations are run. Default = "sims".
  }
  \item{fitfolder}{
    Name of folder where estimations are run. Default = "fits".
  }
  \item{masterfolder}{
    Name of folder to collect estimation results (if run in parallel). Default = "fits".
  }
  \item{MLEdata}{
    Use MLE values in estimation rather than bootstrap values. For model
    testing. Default = FALSE.
  }
  \item{skipfiles}{
    Skip any simulations that have already been run? Default = TRUE.
  }
  \item{simchoices}{
    Vector of indices of scenarios (combination of control and/or data file) to be
    simulated. Values are numeric, but correspond to letters in file
    names (1=ctl_simA.ss, 2=ctl_simB.ss). Default=1.
  }
  \item{fitchoices}{
    Vector of indices of scenarios (combination of control and/or data file) to be
    estimated. Values are numeric, but correspond to letters in file
    names (1=ctl_fitA.ss, 2=ctl_fitB.ss). Default=1.
  }
  \item{samedatafile}{
    Use the same data file for all simulations? (Alternative is one data
    file for each control file). Default=FALSE.
  }
  \item{homepath}{
    Location where the input files are located, and in which the above
    inputs for simfolder and fitfolder are located.
  }
  \item{recdevmatrix}{
    Matrix of recruit deviations. NULL = read from file or create new file if
    one doesn't exists. Default=NULL.
  }
  \item{rescale}{
    Argument passed to \code{\link{SS_recdevs}}: should the recdevs be
    rescaled to have mean = 0 and std. deviation = sigmaR? Default=TRUE.
  }
  \item{fitbiasramp}{
    Use the function \code{\link{SS_fitbiasramp}} to estimate bias
    adjustment inputs in control file during estimation mode? This
    requires running each model twice: once with no change to control
    file, and again with bias adjustment updated based on previous model
    run. Default=FALSE.
  }
  \item{exe}{
    Command to run executable. Default="ss3_opt".
  }
  \item{simextras}{
    Additional inputs to run model when in simulation mode. Default="-nox -nohess".
  }
  \item{fitextras}{
    Additional inputs to run model when in estimation mode. Default="-nox -gbs 1000000000 -cbs 1000000000".
  }
  \item{fyr}{
    First year of stochastic recruitment deviations to include in
    simulation models. NULL will run \code{SS_readctl} to get
    setting from control file. Default=NULL.
  }
  \item{lyr}{
    Last year of stochastic recruitment deviations to include in
    simulation models. NULL will run \code{SS_readctl} to get
    setting from control file. Default=NULL.
  }
  \item{printfile}{
    Switch to determine whether description of completed runs is printed
    to a file or not. Default=TRUE.
  }
  \item{intern}{
    Input passed to system command. If FALSE then output from ADMB is
    printed to screen during model run otherwise it is written to a file.
  }
  \item{verbose}{
    Print various messages to the command line as the function runs? Default=TRUE.
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
  %%  ~~who you are~~
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SSgetoutput}}
  \code{\link{SSsummarize}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ manip }
\keyword{ list }
