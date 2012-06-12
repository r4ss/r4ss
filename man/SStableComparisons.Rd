\name{SStableComparisons}
\alias{SStableComparisons}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make table comparing quantities across models
}
\description{
  Creates a table comparing key quantities from multiple models, which
  is a reduction of the full information in various parts of the list
  created using the \code{SSsummarize} function.
}
\usage{
SStableComparisons(summaryoutput,
           models="all",
           likenames=c("TOTAL",
             "Survey",
             "Length_comp",
             "Age_comp",
             "priors",
             "Size_at_age"),
           names=c("R0",
             "steep",
             "NatM",
             "Q",
             "L_at_Amax",
             "VonBert_K",
             "RecrDev_2008",
             "SPB_Virg",
             "Bratio_2012",
             "SPRratio_2011"),
           modelnames="default",
           csv=FALSE,
           csvdir="workingdirectory",
           csvfile="parameter_comparison_table.csv",
           verbose=TRUE,
           mcmc=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{summaryoutput}{
    list created by \code{SSsummarize}
  }
  \item{models}{
    optional subset of the models described in \code{summaryoutput}.
    Either "all" or a vector of numbers indicating columns in summary
    tables.
  }
  \item{likenames}{
    Labels for likelihood values to include, should match substring of
    labels in \code{summaryoutput$likelihoods}.
  }
  \item{names}{
    Labels for parameters or derived quantities to include, should match
    substring of labels in \code{summaryoutput$pars} or
    \code{summaryoutput$quants}. 
  }
  \item{modelnames}{
    optional vector of labels to use as column names. Default is
    'model1','model2',etc.
  }
  \item{csv}{
    write resulting table to CSV file?
  }
  \item{csvdir}{
    directory for optional CSV file
  }
  \item{csvfile}{
    filename for CSV file
  }
  \item{verbose}{
    report progress to R GUI?
  }
  \item{mcmc}{
    summarize MCMC output in table?
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
  \code{\link{SSsummarize}}, \code{\link{SSplotComparisons}},
  \code{\link{SS_output}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
