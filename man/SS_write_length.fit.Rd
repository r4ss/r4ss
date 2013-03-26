\name{SS_write_length.fit}
\alias{SS_write_length.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Write length.fit file to be used by the MFCL length-comp viewer.
}
\description{
  Writes files in the format used by the MFCL length-composition viewer.
  Inspired by Simon Hoyle's demonstration. Still needs work. 
}
\usage{
SS_write_length.fit(replist = NULL, outfile = "length.fit", compfile =
"CompReport.sso", dir = "default", overwrite = FALSE, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    List created by \code{SS_output}
  }
  \item{outfile}{
    Name of file to create.
  }
  \item{compfile}{
    SS output file with composition data info.
  }
  \item{dir}{
    Directory where stuff happens. Defaults to directory where model was
    run.
  }
  \item{overwrite}{
    Overwrite existing file?
  }
  \item{verbose}{
    More verbose info on progress of the function?
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
\references{
  \url{http://www.multifan-cl.org/},
  \url{http://www.spc.int/OceanFish/en/ofpsection/sam/research/272-mfcl-viewer}
}
\author{
  Ian Taylor
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
%   %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ manip }% __ONLY ONE__ keyword per line
