\name{SSv3_plots}
\alias{SSv3_plots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ function to plot many quantities related to output from Stock Synthesis }
\description{
  Creates a user-chosen set of plots, including biological quantities,
  time series, and fits to data.  Plots are sent to R GUI, single PDF file, or multiple PNG files.
}
\usage{
SSv3_plots(replist = "ReportObject", plot = 1:24, print = 0, pdf = F,
printfolder = "", dir = "default", fleets = "all", areas = "all",
fleetnames = "default", fleetcols = "default", fleetlty = 1,
fleetpch = 1, lwd = 1, areacols = "default", areanames = "default",
verbose = T, uncertainty = T, forecastplot = F, datplot = F,
Natageplot = T, samplesizeplots = T, compresidplots = T, sprtarg = 0.4,
btarg = 0.4, minbthresh = 0.25, pntscalar = 2.6, minnbubble = 8,
aalyear = -1, aalbin = -1, aalmaxbinrange = 0, aalresids = F,
maxneff = 5000, cohortlines = c(), smooth = T, showsampsize = T,
showeffN = T, showlegend = T, pwidth = 7, pheight = 7, punits = "in",
ptsize = 12, res = 300, cex.main = 1, selexlines = 1:5,
rows = 1, cols = 1, maxrows = 6, maxcols = 6,
maxrows2 = 2, maxcols2 = 4, tagrows = 3, tagcols = 3,
fixdims = T, new = T, catchasnumbers = F, legendloc = "topleft", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{  The name of the object in the R workspace produced by the \code{SSv3_output} function. This is the only required input that will never work with the default value. The name of the object should not be in quotes.}
  \item{plot}{ Plot sets to be created, see list of plots below.  Use to
    specify only those plot sets of interest, e.g., c(1,2,5,10). Plots
    for data not available in the model run will automatically be
    skipped, whether called or not. Default=1:24.}
  \item{print}{  Plot set to be printed to files?  PNG files are created
    in the specified directory for one or more requested plots.  Each
    plot has a unique name starting with the number of the set it comes
    from and attempting to be mildly descriptive.  This argument is
    independent of "plot" in that plots can be created on screen, or
    printed to file or both.  Manual rescaling cannot be done after
    printing, so this option may not be the best choice for each plot;
    saving files from the screen allows more control. Default=0.}
  \item{pdf}{ Send plots to PDF file instead of R GUI? Input \code{plot}
    must be used and input \code{print} must be 0. Default=0.}
  \item{printfolder}{ Name of subfolder to create within the working
    directory into which any PNG files specified by \code{print} will be
    saved. By default the working directory is used with no subfolder. Default="".}
  \item{dir}{ The directory in which any PNG files requested by \code{print} are created. By default it will be the same directory that the report file was read from by the \code{SSv3_output} function. Default="default".}
  \item{fleets}{ Either the string "all", or a vector of numerical
    values, like c(1,3), listing fleets or surveys for which plots should
    be made. By default, plots will be made for all fleets and surveys. Default="all".}
  \item{areas}{ Either the string "all", or a vector of numerical
    values, like c(1,3), listing areas for which plots should be made in
    a multi-area model. By default, plots will be made for all areas
    (excepting cases where the function has not yet been updated for
    multi-area models). Default="all".}
  \item{fleetnames}{  Either the string "default", or a vector of
    characters strings to use for each fleet name. Default="default".}
  \item{fleetcols}{ Either the string "default", or a vector of colors
    to use for each fleet.  Default="default".}
  \item{fleetlty}{ Vector of line types used for each fleet in some
    plots. Default=1.}
  \item{fleetpch}{ Vector of point types used for each fleet in some plots. Default=1.}
  \item{lwd}{ Line width for some plots. Default=1.}
  \item{areacols}{  Either the string "default", or a vector of colors
    to use for each area. Default="default".}
  \item{areanames}{ Optional vector of names for each area used in
    titles. Default="default".}
  \item{verbose}{  Return updates of function progress to the R GUI? Default=T.}
  \item{uncertainty}{ Include values in plots showing estimates of
    uncertainty (requires positive definite hessian in model and
    \code{covar}=T in \code{SSv3_output})?  Default=T.}
  \item{forecastplot}{ Include forecast years in the plots? Obviously
    requires forecast options to have been used in the model.  Default=T.}
  \item{datplot}{ Plot the data by itself? This is useful in document
    preparation. Setting datplot=F is equivalent to leaving off plots 15
    and 16.  Default=F.}
  \item{Natageplot}{ Plot the expected numbers at age bubble plots and
    mean-age time series?  Default=T.}
  \item{samplesizeplots}{ Show sample size plots?  Default=T.}
  \item{compresidplots}{  Show residuals for composition plots?}
  \item{sprtarg}{  Specify the F/SPR proxy target. Default=0.4.}
  \item{btarg}{  Target depletion to be used in plots showing
    depletion. May be omitted by setting to NA.  Default=0.4.}
  \item{minbthresh}{  Threshold depletion to be used in plots showing
    depletion. May be omitted by setting to NA. Default=0.25.}
  \item{pntscalar}{ This scalar defines the maximum bubble size for
    balloon plots; each plot scaled independently based on this maximum
    size and the values plotted. Often some plots look better with one
    value and others with a larger or smaller value. Default=2.6}
  \item{minnbubble}{ This defines the minimum number of years below
    which blank years will be added to bubble plots to avoid cropping.
    Default=8.}
  \item{aalyear}{ Years to plot multi-panel conditional age-at-length
    fits for all length bins; must be in a "c(YYYY,YYYY)" format. Useful
    for checking the fit of a dominant year class, critical time period,
    etc. Default=-1. }
  \item{aalbin}{ The length bin for which multi-panel plots of the fit
    to conditional age-at-length data will be produced for all years.
    Useful to see if growth curves are ok, or to see the information on
    year classes move through the conditional data. Default=-1. }
  \item{aalmaxbinrange}{ The largest length bin range allowed for composition data to be considered as conditional age-at-length data. Default=0. }
  \item{aalresids}{ Plot the full set of conditional age-at-length Pearson residuals? Default=F. }
  \item{maxneff}{ The maximum value to include on plots of input and effective sample size. Occasionally a calculation of effective N blows up to very large numbers, rendering it impossible to observe the relationship for other data. Default=5000. }
  \item{cohortlines}{ Optional vector of birth years for cohorts for which to add growth curves to numbers at length bubble plots. Default=c(). }
  \item{smooth}{ Add loess smoother to observed vs. expected index plots and input vs. effective sample size? Default=T. }
  \item{showsampsize}{ Display sample sizes on composition plots? Default=T. }
  \item{showeffN}{ Display effective sample sizes on composition plots? Default=T. }
  \item{showlegend}{ Display legends in various plots? Default=T. }
  \item{pwidth}{ Default width of plots printed to files in units of \code{punits}. Default=7. }
  \item{pheight}{ Default height width of plots printed to files in units of \code{punits}. Default=7. }
  \item{punits}{ Units for \code{pwidth} and \code{pheight}. Can be "px" (pixels), "in" (inches), "cm" or "mm". Default="in". }
  \item{ptsize}{ Point size for plotted text in plots printed to files (see help("png") in R for details). Default=12. }
  \item{res}{ Resolution of plots printed to files. Default=300. }
  \item{cex.main}{ Character expansion parameter for plot titles (only implemented for composition plots so far). Default=1. }
  \item{selexlines}{ Vector controling which lines should be shown on
    selectivity plots if the model includes retention. Default=1:5. }
  \item{rows}{ Number of rows to use for single panel plots. Default=1. }
  \item{cols}{ Number of columns to use for single panel plots. Default=1. }
  \item{maxrows}{ Maximum number of rows to for multi-panel plots. Default=6. }
  \item{maxcols}{ Maximum number of columns for multi-panel plots. Default=6. }
  \item{maxrows2}{ Maximum number of rows for conditional age at length multi-panel plots. Default=2. }
  \item{maxcols2}{ Maximum number of rows for conditional age at length multi-panel plots. Default=4. }
  \item{tagrows}{ Number of rows for tagging-related plots. Default=3. }
  \item{tagcols}{ Number of columns for tagging-related plots. Default=3. }
  \item{fixdims}{ Control whether multi-panel plots all have dimensions equal to maxrows by maxcols, or resized within those limits to fit number of plots. Default=T. }
  \item{new}{ Open a new window or add to existing plot windows. Default=T. }
  \item{catchasnumbers}{ Is catch input in numbers instead of biomass?
    Default=F. }
  \item{legendloc}{ Location for all legends. Default="topleft". }
  \item{\dots}{ Additional arguments that will be passed to some subfunctions. }
}
\details{
  \bold{List of plot groups}

  \enumerate{
    \item Biology
    \itemize{
      \item Mean weight at size in the last year (by sex if applicable)
      \item Maturity at size
      \item Spawning output at size
      \item Mid year mean length at age with 95\% range of lengths (by sex if applicable)
      \item Natural mortality at age if variable (by sex if applicable)
    }
    \item Time-varying growth
    \itemize{
      \item 3-D plot of mean length at age over time (by sex if applicable)
      \item Contour plot of mean length at age over time (by sex if applicable)
    }
    \item Time-varying selectivity and retention
    \itemize{
      \item 3-D plot of selectivity at length (or age or both) over time (by sex if applicable)
      \item Contour plot of selectivity at length (or age or both) over time (by sex if applicable)
      \item 3-D plot of retention at length over time (by sex if applicable)
      \item Contour plot of retention at length over time (by sex if
      applicable)
    }
    \item Ending selectivity and retention
    \itemize{
      \item Ending year selectivity at length (or age or both) and
      retention (by sex if applicable)
    }
    \item Basic time series
    \itemize{
      \item Time series of total biomass
      \item Time series of summary biomass
      \item Time series of landings by fleet with total
      \item Time series of catch by fleet with total
      \item Time series of discards by fleet with total
      \item Time series of discard fraction by fleet with total
      \item Time series of harvest rate by fleet
    }
    \item Recruitment plots
    \itemize{
      \item Time series of recruitment estimates
      \item Time series of recruitment estimates with forecast years (optional)
      \item Time series of recruitments with asymptotic 95% intervals
      \item Time series of recruitments with asymptotic 95% intervals and forecast years (optional)
    }
    \item Spawning biomass plots
    \itemize{
      \item Time series of spawning biomass estimates
      \item Time series of spawning biomass estimates with asymptotic intervals
      \item Time series of spawning biomass estimates with forecast years
      \item Time series of spawning biomass estimates with forecast years with asymptotic intervals
    }
    \item Depletion plots
    \itemize{
      \item Time series of depletion estimates
      \item Time series of depletion estimates with asymptotic intervals
      \item Time series of depletion estimates with forecast years
      \item Time series of depletion estimates with forecast years with asymptotic intervals
    }
    \item Recruitment deviation plots
    \itemize{
      \item Time series of recruitment deviations
      \item Time series of recruitment deviation variances with sigma R
    }
    \item Discard fractions and mean body weights
    \itemize{
      \item Fit to discard fractions by fleet (if applicable)
      \item Fit to mean body weight observations by fleet and partition (if applicable)
    }
    \item Management plots
    \itemize{
      \item Time series of SPR estimates
      \item Phase plot of 1-SPR and B/B40 with targets
    }
    \item Stock-recruit curve
    \itemize{
      \item Stock-recruit curve and estimated recruitments
    }
    \item CPUE plots
    \itemize{
      \item Fit to CPUE/Survey data by fleet (if applicable)
      \item Observed vs. expected CPUE/Survey by fleet (if applicable)
      \item Log fit to CPUE/Survey data by fleet (if applicable)
      \item Log observed vs. log expected CPUE/Survey by fleet (if applicable)
    }
    \item Numbers at age plots
    \itemize{
      \item Bubble plots of expected numbers at age by year (by sex if applicable)
      \item Time series of mean age in the population (by sex if applicable)
      \item Ageing imprecison standard deviations by ageing error key
    }
    \item Length-composition data plots
    \itemize{
      \item Multi-panel bar plots of length composition data by fleet, sex, partition
      \item Bubble plots of length composition data by fleet, sex, partition
    }
    \item Age-composition data plots
    \itemize{
      \item Multi-panel bar plots of age composition data by fleet, sex, partition
      \item Bubble plots of age composition data by fleet, sex, partition
    }
    \item Conditional age-at-length data plots
    \itemize{
      \item Multi-panel plots of conditional age-at length data by fleet, sex, partition
    }
    \item Fit to length-composition plots
    \itemize{
      \item Input vs. effective sample size for fit to length composition data by fleet, sex, partition
      \item Multi-panel plots of fit to length composition data by fleet, sex, partition
      \item Bubble plots of Pearson residuals for fit to length composition data by fleet, sex, partition
    }
    \item Fit to age-composition plots
    \itemize{
      \item Input vs. effective sample size plots for fit to age composition data by fleet, sex, partition
      \item Multi-panel plots of fit to age composition data by fleet, sex, partition
      \item Bubble plots of Pearson residuals for fit to age composition data by fleet, sex, partition
      \item Input vs. effective sample size for conditional age-at-length data by fleet, sex, partition
    }
    \item Fit to conditional age-at-length data
    \itemize{
      \item Multi-panel fit to age-at-length data by fleet, sex, partition for input years (optional)
      \item Bubbles of Pearsons for age-at-length by fleet, sex, partition for input years (optional)
      \item Multi-panel fit to age-at-length data by fleet, sex, partition for input length bin (optional)
      \item Multi-panel bubble plots of Pearsons for fit to age-at-length data by fleet, sex, partition
    }
    \item Fit to length-at-age plots
    \itemize{
      \item Multi-panel plots of fit to length-at-age data by fleet, sex, partition
      \item Bubble plots of Pearson residuals for fit to length-at-age data by fleet, sex, partition
    }
    \item Production
    \itemize{
      \item Yield curve
      \item Surplus production vs. biomass (Walters, et al. 2008)
    }
    \item CPUE data-only plots
    \item Tagging-related plots
    \itemize{
      \item Multi-panel plot of obs & exp recaps by tag group
      \item Plot of obs vs exp tag recaptures by year aggregated across group
      \item Bubble plot of observed recapture data
      \item Bubble plot of residuals
    }
  }
}
\value{
  Plots returned to R GUI, to single PDF file, or to multiple PNG files
  depending on the inputs chosen.
}
\references{ Walters, Hilborn, and  Christensen, 2008, Surplus
    production dynamics in declining and recovering fish populations.
    Can. J. Fish. Aquat. Sci. 65: 2536-2551 }
\author{
  Ian Stewart, Ian Taylor, Andre Punt, and other contributors
}
% \note{ }
% \seealso{ %~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
\dontrun{
SSv3_plots(replist = myreplist)
SSv3_plots(replist = myreplist, pdf=T, pwidth=8, pheight=8, rows=2,
    cols=2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
\keyword{ hplot }
% \keyword{ }% __ONLY ONE__ keyword per line
