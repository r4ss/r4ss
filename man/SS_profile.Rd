\name{SS_profile}
\alias{SS_profile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Run a likelihood profile in Stock Synthesis. }
\description{
  Iteratively changes the control file using SS_changepars.
}
\usage{
SS_profile(dir = "C:/myfiles/mymodels/myrun/", masterctlfile =
  "control.ss_new", newctlfile = "control_modified.ss",
  linenum = NULL, string = NULL, profilevec = NULL,
  usepar = TRUE, dircopy = TRUE, exe.delete = FALSE,
  command = "SS3 -nox", model = "ss3", systemcmd =
  FALSE, saveoutput = TRUE, overwrite = FALSE, verbose =
  TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{
    Directory where input files and executable are located.}
  \item{masterctlfile}{
    Source control file. Default = "control.ss_new"
  }
  \item{newctlfile}{
    Destination for new control files (must match entry
    in starter file). Default = "control_modified.ss".
  }
  \item{linenum}{
    Line number of parameter to be changed.
    Can be used instead of \code{string} or left as NULL.
  }
  \item{string}{
    String partially matching name of parameter to be
    changed. Can be used instead of \code{linenum} or left as NULL.
  }
  \item{usepar}{
    Use PAR file from previous profile step for starting
    values? NOT IMPLEMENTED YET.
  }
  \item{dircopy}{
    Copy directories for each run? NOT IMPLEMENTED YET.
  }
  \item{exe.delete}{
    Delete exe files in each directory?
    NOT IMPLEMENTED YET.
  }
  \item{profilevec}{
    Vector of values to profile over.  Default = NULL.
  }
  \item{command}{
    Call to executable. Default = "SS3 -nox".
  }
  \item{model}{
    Name of executable. Default = "ss3".
  }
  \item{systemcmd}{
    Should R call SS using "system" function intead of
    "shell". This may be required when running R in Emacs. Default = FALSE.
  }
  \item{saveoutput}{
    Copy output .SSO files to unique names.
    Default = TRUE.
  }
  \item{overwrite}{
    Overwrite any existing .SSO files. Default = FALSE.
  }
  \item{verbose}{
    Controls amount of info output to command line.
    Default = TRUE.
  }
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%   \item{comp1 }{Description of 'comp1'}
%   \item{comp2 }{Description of 'comp2'}
%   ...
% }
% \references{ ~put references to the literature/web site here ~ }
\author{ Ian Taylor }
\note{
  The starting values used in this profile are not ideal and some models
  may not converge. Care should be taken in using an automated tool
  like this, and some models are likely to require rerunning with
  alternate starting values.

  Also, someday this function will be improved to work directly with the
  plotting function \code{\link{SSplotProfile}}, but they don't yet
  work well together. Thus, even if \code{\link{SS_profile}} is used,
  the output should be read using \code{\link{SSgetoutput}} or by
  multiple calls to \code{\link{SS_output}} before sending to
  \code{\link{SSplotProfile}}.
}
\seealso{
  \code{\link{SS_changepars}}, \code{\link{SS_parlines}}
}
% \examples{ }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ manip }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
