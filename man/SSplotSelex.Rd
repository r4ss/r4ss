\name{SSplotSelex}
\alias{SSplotSelex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot selectivity
}
\description{
  Plot selectivity, including retention and other quantities, with
  additional plots for time-varying selectivity.
}
\usage{
SSplotSelex(replist, infotable = NULL, fleets = "all",
fleetnames = "default", sizefactors = c("Lsel"),
agefactors = c("Asel"), years = "endyr", season = 1,
sexes="all", selexlines = 1:5, subplot = 1:25,
skipAgeSelex10 = TRUE,
plot = TRUE, print = FALSE, add = FALSE,
labels=c("Length (cm)",
  "Age (yr)",     
  "Year",         
  "Selectivity",  
  "Retention",    
  "Discard mortality"),
col1 = "red", col2 = "blue",
lwd = 2, fleetcols = "default", fleetpch = "default",
fleetlty = "default", spacepoints = 5,
staggerpoints = 1, legendloc = "bottomright", 
pwidth = 7, pheight = 7, punits = "in", res = 300, ptsize = 12,
cex.main = 1, plotdir = "default", verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    List created by \code{SS_output}
  }
  \item{fleets}{
    Optional vector to subset fleets for which to make plots
  }
  \item{infotable}{
    Optional table of information controlling appearance of plot and
    legend. Is produced as output and can be modified and entered as
    input.
  }
  \item{fleetnames}{
    Optional replacement for fleenames used in data file
  }
  \item{sizefactors}{
    Which elements of the factors column of SIZE_SELEX should be
    included in plot of selectivity across multiple fleets?
  }
  \item{agefactors}{
    Which elements of the factors column of AGE_SELEX should be
    included in plot of selectivity across multiple fleets?
  }
  \item{years}{
    Which years for selectivity are shown in multi-line plot (default =
    last year of model).
  }
  \item{season}{
    Which season (if seasonal model) for selectivity shown in multi-line
    plot (default = 1).
  }
  \item{sexes}{
    Optional vector to subset genders for which to make plots
    (1=females, 2=males)
  }
  \item{selexlines}{
    Vector to select which lines get plotted. values are 1. Selectivity,
    2. Retention, 3. Discard mortality, 4. Keep = Sel*Ret, 5. Dead =
    Sel*(Ret+(1-Ret)*Mort).
  }
  \item{subplot}{
    Vector controlling which subplots to create
  }
  \item{skipAgeSelex10}{
    Exclude plots for age selectivity type 10 (selectivity = 1.0 for all
    ages beginning at age 1)?
  }
  \item{lwd}{
    Line widths for plots
  }
  \item{fleetcols}{ Optional vector of colors for each fleet (in
    multi-fleet plots)
  } 
  \item{fleetpch}{ Optional vector of plot characters for each fleet (in
    multi-fleet plots)
  } 
  \item{fleetlty}{ Optional vector of line types for each fleet (in
    multi-fleet plots) 
  }
  \item{spacepoints}{
    number of years between points shown on top of lines (for long
    timeseries, points every year get mashed together)
  }
  \item{staggerpoints}{
    number of years to stagger the first point
    (if \code{spacepoints > 1}) for each line
    (so that adjacent lines have points in different years)
  }
  \item{legendloc}{
    location of legend. See ?legend for more info.
  }
  \item{plot}{
    Plot to active plot device?
  }
  \item{print}{
    Print to PNG files?
  }
  \item{add}{
    Add to existing plot (not yet implemented)
  }
  \item{labels}{
    vector of labels for plots (titles and axis labels)
  }
  \item{col1}{
    color for female growth curve
  }
  \item{col2}{
    color for male growth curve
  }
  \item{pwidth}{
    width of plot written to PNG file
  }
  \item{pheight}{
    height of plot written to PNG file
  }
  \item{punits}{
    units for PNG file
  }
  \item{res}{
    resolution for PNG file
  }
  \item{ptsize}{
    ptsize for PNG file
  }
  \item{cex.main}{
    character expansion for plot titles
  }
  \item{plotdir}{
    directory where PNG files will be written. by default it will
    be the directory where the model was run.
  }
  \item{verbose}{
    report progress to R GUI?
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
  Ian Stewart, Ian Taylor
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_plots}}, \code{\link{SS_output}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
