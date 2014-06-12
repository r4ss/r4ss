\name{make_multifig}
\alias{make_multifig}
%%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create multi-figure plots.
}
\description{
  Function created as an alternative to lattice package for multi-figure
  plots of composition data and fits from Stock Synthesis output.
}
\usage{
make_multifig(ptsx, ptsy, yr, linesx = 0, linesy = 0, ptsSD = 0,
sampsize = 0, effN = 0, showsampsize = TRUE, showeffN = TRUE,
sampsizeround = 1, maxrows = 6, maxcols = 6, rows = 1, cols = 1,
fixdims = TRUE, main = "", cex.main = 1, xlab = "", ylab = "",
size = 1, maxsize = 3, do.sqrt = TRUE, minnbubble = 8, allopen = TRUE,
horiz_lab = "default", xbuffer = c(0.1, 0.1), ybuffer = c(0, 0.15),
ymin0 = TRUE, axis1 = "default", axis2 = "default", linepos = 1,
type = "o", bars = FALSE, barwidth = "default", ptscex = 1, ptscol = 1,
ptscol2 = 1, linescol = 2, lty = 1, lwd = 1, pch = 1, nlegends = 3,
legtext = list("yr","sampsize", "effN"), legx = "default",
legy = "default", legadjx = "default", legadjy = "default",
legsize = c(1.2, 1), legfont = c(2, 1), ipage = 0, scalebins = FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ptsx}{
    vector of x values for points or bars
  }
  \item{ptsy}{
    vector of y values for points or bars  of same length as ptsx
  }
  \item{yr}{
    vector of category values (years) of same length as ptsx
  }
  \item{linesx}{
    optional vector of x values for lines
  }
  \item{linesy}{
    optional vector of y values for lines
  }
  \item{ptsSD}{
    optional vector of standard deviations used to plot error bars
    on top of each point under the assumption of normally distributed
    error
  }
  \item{sampsize}{
    optional sample size vector of same length as ptsx
  }
  \item{effN}{
    optional effective sample size vector of same length as ptsx
  }
  \item{showsampsize}{
    show sample size values on plot?
  }
  \item{showeffN}{
    show effective sample size values on plot?
  }
  \item{sampsizeround}{
    rounding level for sample size values
  }
  \item{maxrows}{
    maximum (or fixed) number or rows of panels in the plot
  }
  \item{maxcols}{
    maximum (or fixed) number or columns of panels in the plot
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
    elements in \code{yr} input.
  }
  \item{main}{
    title of plot
  }
  \item{cex.main}{
    character expansion for title
  }
  \item{xlab}{
    x-axis label
  }
  \item{ylab}{
    y-axis label
  }
  \item{size}{
    vector of bubbles sizes if making a bubble plot
  }
  \item{maxsize}{
    maximum size of bubbles
  }
  \item{do.sqrt}{
    scale bubbles based on sqrt of size vector. see ?bubble3 for more info.
  }
  \item{minnbubble}{
    number of unique x values before adding buffer. see ?bubble3 for more info.
  }
  \item{allopen}{
    should all bubbles be open? see ?bubble3 for more info.
  }
  \item{horiz_lab}{
    axis labels set horizontal all the time (TRUE), never (FALSE) or only
    when relatively short ("default")
  }
  \item{xbuffer}{
    extra space around points on the left and right as fraction of total width of plot
  }
  \item{ybuffer}{
    like xbuffer
  }
  \item{ymin0}{
    fix minimum y-value at 0?
  }
  \item{axis1}{
    position of bottom axis values
  }
  \item{axis2}{
    position of left size axis values
  }
  \item{linepos}{
    should lines be added on top of points (linepos=1) or behind (linepos=2)?
  }
  \item{type}{
    type of line/points used for observed values (see 'type' in ?plot
    for details) on top of a grey polygon. Default is "o" for
    overplotting points on lines.
  }
  \item{bars}{
    should the ptsx/ptsy values be bars instead of points (TRUE/FALSE)
  }
  \item{barwidth}{
    width of bars in barplot, default method chooses based on quick and
    dirty formula also, current method of plot(...type='h') could be
    replaced with better approach
  }
  \item{ptscex}{
    character expansion factor for points (default=1)
  }
  \item{ptscol}{
    color for points/bars
  }
  \item{ptscol2}{
    color for negative value points in bubble plots
  }
  \item{linescol}{
    color for lines
  }
  \item{lty}{
    line type
  }
  \item{lwd}{
    line width
  }
  \item{pch}{
    point character type
  }
  \item{nlegends}{
    number of lines of text to add as legends in each plot
  }
  \item{legtext}{
    text in legend, a list of length=nlegends. values may be any of 1.
    "yr", 2. "sampsize", 3. "effN", or a vector of length = ptsx.
  }
  \item{legx}{
    vector of length=nlegends of x-values of legends (default is first one
    on left, all after on right)}
  \item{legy}{
    vector of length=nlegends of y-values of legends (default is top for all
    plots)
  }
  \item{legadjx}{
    left/right adjustment of legends around legx
  }
  \item{legadjy}{
    left/right adjustment of legends around legy
  }
  \item{legsize}{
    font size for legends. default=c(1.2,1.0) (larger for year and normal
    for others)
  }
  \item{legfont}{
    font type for legends, same as "font" under ?par
  }
  \item{ipage}{
    which page of plots when covering more than will fit within maxrows
    by maxcols.
  }
  \item{scalebins}{
    Rescale expected and observed proportions by dividing by bin width
    for models where bins have different widths? Caution!: May not work
    correctly in all cases.
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

% %% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_plots}},\code{\link{SSplotComps}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
\keyword{ hplot }% __ONLY ONE__ keyword per line
