\name{SS_writestarter}
\alias{SS_writestarter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  write starter file
}
\description{
  write Stock Synthesis starter file from list object in R which was
  probably created using \code{\link{SS_readstarter}}
}
\usage{
SS_writestarter(mylist, dir = NULL, file = "starter.ss", overwrite = F,
verbose = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mylist}{
    List object created by \code{\link{SS_readstarter}}.
  }
  \item{dir}{
    Directory for new starter file. Default=NULL (working directory).
  }
  \item{file}{
    Filename for new starter file. Default="starter.ss".
  }
  \item{overwrite}{
    Should existing files be overwritten? Default=F.
  }
  \item{verbose}{
    Should there be verbose output while running the file? Default=T.
  }
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{
Ian Taylor
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
 \code{\link{SS_readctl}},
 \code{\link{SS_writestarter}}, \code{\link{SS_writeforecast}},
 \code{\link{SS_writedat}}, \code{\link{SS_writectl}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ manip }% __ONLY ONE__ keyword per line
