\name{SSplotComps}
\alias{SSplotComps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot composition data and fits.
}
\description{
  Plot composition data and fits from Stock Synthesis output.
  Mult-figure plots depend on \code{make_multifig}.
}
\usage{
SSplotComps(replist, subplots=1:12,
  kind="LEN", sizemethod=1, aalyear=-1, aalbin=-1, plot=TRUE,
  print=FALSE, fleets="all", fleetnames="default", sexes="all",
  datonly=FALSE, samplesizeplots=TRUE, compresidplots=TRUE, bub=FALSE,
  showsampsize=TRUE, showeffN=TRUE, minnbubble=8, pntscalar=2.6,
  pwidth=7, pheight=7, punits="in", ptsize=12, res=300,
  plotdir="default", cex.main=1, linepos=1, fitbar=FALSE, maxsize=3,
  do.sqrt=TRUE, smooth=TRUE, cohortlines=c(),
  labels = c("Length (cm)",            
             "Age (yr)",               
             "Year",                   
             "Observed sample size",   
             "Effective sample size",  
             "Proportion",             
             "cm",                     
             "Frequency",              
             "Weight",                 
             "Length",                 
             "(mt)",                   
             "(numbers x1000)",        
             "Stdev (Age) (yr)",       
             "Andre's conditional AAL plot, "),
  printmkt=TRUE,printsex=TRUE,
  maxrows=6,maxcols=6,maxrows2=2,maxcols2=4,rows=1,cols=1,
  fixdims=TRUE,fixdims2=FALSE,maxneff=5000,verbose=TRUE,
  scalebins=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{SSoutput}
  }
  \item{subplots}{
    vector controlling which subplots to create
  }
  \item{kind}{
    indicator of type of plot can be "LEN", "SIZE", "AGE", "cond", "GSTAGE",
    "L@A", or "W@A".
  }
  \item{sizemethod}{
    if kind = "SIZE" then this switch chooses which of the generalized
    size bin methods will be plotted.
  }
  \item{aalyear}{
    Years to plot multi-panel conditional age-at-length
    fits for all length bins; must be in a "c(YYYY,YYYY)" format. Useful
    for checking the fit of a dominant year class, critical time period,
    etc. Default=-1.
  }
  \item{aalbin}{
    The length bin for which multi-panel plots of the fit
    to conditional age-at-length data will be produced for all years.
    Useful to see if growth curves are ok, or to see the information on
    year classes move through the conditional data. Default=-1.
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{fleets}{
    optional vector to subset fleets for which plots will be made
  }
  \item{fleetnames}{
    optional vector of fleet names to put in the labels
  }
  \item{sexes}{
    which sexes to show plots for. Default="all" which will include
    males, females, and unsexed. This options is not fully implemented
    for all plots.
  }
  \item{datonly}{
    make plots of data without fits as well as data with fits?
  }
  \item{samplesizeplots}{
    make sample size plots?
  }
  \item{compresidplots}{
    make plots of residuals for fit to composition data?
  }
  \item{bub}{
    make bubble plot for numbers at age or size?
  }
  \item{showsampsize}{
    add sample sizes to plot
  }
  \item{showeffN}{
    add effective sample sizes to plot
  }
  \item{minnbubble}{
    number of unique x values before adding buffer. see ?bubble3 for more info.
  }
  \item{pntscalar}{
    maximum bubble size for bubble plots; each plot scaled independently
    based on this maximum size and the values plotted. Often some plots
    look better with one value and others with a larger or smaller
    value. Default=2.6
  }
  \item{pwidth}{
    default width of plots printed to files in units of \code{punits}. Default=7.
  }
  \item{pheight}{
    default height width of plots printed to files in units of \code{punits}. Default=7.
  }
  \item{punits}{
    units for \code{pwidth} and \code{pheight}. Can be "px" (pixels), "in" (inches), "cm" or "mm". Default="in".
  }
  \item{ptsize}{
    point size for plotted text in plots printed to files (see help("png") in R for details). Default=12.
  }
  \item{res}{
    resolution of plots printed to files. Default=300
  }
  \item{plotdir}{
    directory where PNG files will be written. by default it will
    be the directory where the model was run.
  }
  \item{cex.main}{
    character expansion parameter for plot titles
  }
  \item{linepos}{
    should lines be added before points (linepos=1) or after (linepos=2)?
  }
  \item{fitbar}{
    show fit to bars instead of points
  }
  \item{maxsize}{
    maximum size of bubble plots
  }
  \item{do.sqrt}{
    scale bubbles based on sqrt of size vector. see ?bubble3 for more info.
  }
  \item{smooth}{
    add loess smoother to observed vs. expected index plots and input vs. effective sample size?
  }
  \item{cohortlines}{
    optional vector of birth years for cohorts for which to add growth curves to numbers at length bubble plots
  }
  \item{labels}{
    vector of labels for plots (titles and axis labels)
  }
  \item{printmkt}{
    show market categories in plot titles?
  }
  \item{printsex}{
    show gender in plot titles?
  }
  \item{maxrows}{
    maximum (or fixed) number or rows of panels in the plot
  }
  \item{maxcols}{
    maximum (or fixed) number or columns of panels in the plot
  }
  \item{maxrows2}{
    maximum number of rows for conditional age at length plots
  }
  \item{maxcols2}{
    maximum number of columns for conditional age at length plots
  }
  \item{rows}{
    number or rows to return to as default for next plots to come or for
    single plots
  }
  \item{cols}{
    number or cols to return to as default for next plots to come or for
    single plots
  }
  \item{fixdims}{
    fix the dimensions at maxrows by maxcols or resize based on number of
    years of data
  }
  \item{fixdims2}{
    fix the dimensions at maxrows by maxcols in aggregate plots or
    resize based on number of fleets
  }
  \item{maxneff}{
    the maximum value to include on plots of input and effective sample
    size. Occasionally a calculation of effective N blows up to very large
    numbers, rendering it impossible to observe the relationship for other
    data. Default=5000.
  }
  \item{verbose}{
    return updates of function progress to the R GUI?
  }
  \item{scalebins}{
    Rescale expected and observed proportions by dividing by bin width
    for models where bins have different widths? Caution!: May not work
    correctly in all cases.
  }
  \item{\dots}{
    additional arguments that will be passed to the plotting.
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
  \code{\link{SS_plots}}, \code{\link{make_multifig}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }% __ONLY ONE__ keyword per line
