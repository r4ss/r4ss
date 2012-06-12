\name{SSgetoutput}
\alias{SSgetoutput}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get output from multiple Stock Synthesis models.
}
\description{
  Apply the function \code{\link{SS_output}} multiple times and save
  output as individual objects or a list of lists.
}
\usage{
SSgetoutput(keyvec = NULL, dirvec = NULL, getcovar = TRUE, getcomp = TRUE,
forecast = FALSE, verbose = TRUE, ncols = 210, global = FALSE, replace =
FALSE, listlists = TRUE, underscore=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{keyvec}{
    A vector of strings that are appended to the output files from each
    model if models are all in one directory. Default=NULL.
  }
  \item{dirvec}{
    A vector of directories (full path or relative to working directory)
    in which model output is located. Default=NULL.
  }
  \item{getcovar}{
    Choice to read or not read covar.sso output (saves time and memory). Default=TRUE.
  }
  \item{getcomp}{
    Choice to read or not read CompReport.sso output (saves time and memory). Default=TRUE.
  }
  \item{forecast}{
    Choice to read or not read forecast quantities. Default=FALSE.
  }
  \item{verbose}{
    Print various messages to the command line as the function runs? Default=TRUE.
  }
  \item{ncols}{
    Maximum number of columns in Report.sso (same input as for \code{\link{SS_output}}).
    Default=210.
  }
  \item{global}{
    Save output from each model as a global variable with a unique name. Default=FALSE.
  }
  \item{replace}{
    Replace existing global variables if they already exist. Default=FALSE.
  }
  \item{listlists}{
    Save output from each model as a element of a list (i.e. make a list of
    lists). Default = TRUE.
  }
  \item{underscore}{
    Add an underscore '_' between any file names and any keys in keyvec. Default=FALSE.
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
  \code{\link{SS_output}}
  \code{\link{SSsummarize}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ manip }
\keyword{ list }
