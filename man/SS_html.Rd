\name{SS_html}
\alias{SS_html}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create HTML files to view figures in browser.  }
\description{
  Writes a set of HTML files with tabbed navigation between them.
  Depends on \code{\link{SS_plots_test}} with settings in place to write
  figures to PNG files. Should open main file in default browser
  automatically.
}
\usage{
SS_html(replist=NULL,
        plotdir="plots",
        plotInfoTable=NULL,
        title="SS Output",
        width=500,
        openfile=TRUE,
        multimodel=FALSE,
        filenotes=NULL,
        verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{ Object created by \code{\link{SS_output}} }
  \item{plotdir}{ Directory where PNG files are located. }
  \item{plotInfoTable}{ CSV file with info on PNG files. By default,
    the \code{plotdir} directory will be searched for files with name
    beginning 'plotInfoTable*' }
  \item{title}{ Title for HTML page. }
  \item{width}{ Width of plots (in pixels). }
  \item{openfile}{ Automatically open index.html in default browser? } 
  \item{multimodel}{ Override errors associated with plots from multiple
    model runs. Only do this if you know what you're doing.}
  \item{filenotes}{ Add additional notes to home page. }
  \item{verbose}{ Display more info while running this function? }
}
% \references{ }
\author{ Ian Taylor }
\note{
  By default, this function will look in the directory where PNG files
  were created for CSV files with the name 'plotInfoTable...' written by
  'SS_plots_test. HTML files are written to link to these plots and put
  in the same directory. Please provide feedback on any bugs,
  annoyances, or suggestions for improvement.
} 
\seealso{
  \code{\link{SS_plots}}, \code{\link{SS_output}}
}
% \examples{
% }
\keyword{ aplot }
\keyword{ hplot }% __ONLY ONE__ keyword per line
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
