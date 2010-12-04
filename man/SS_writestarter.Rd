\name{SS_writestarter}
\alias{SS_writestarter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  write Stock Synthesis starter file from list object in R
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SS_writestarter(mylist, dir = NULL, file = "starter.ss", overwrite = F, verbose = T)
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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(mylist, dir=NULL, file="starter.ss",
                      overwrite=F, verbose=T){
  if(verbose) print("running SS_writestarter",quote=F)
  if(mylist$type!="Stock_Synthesis_starter_file"){
    print("input 'mylist' should be a list with $type=='Stock_Synthesis_starter_file'",quote=F)
    return()
  }
  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})

  if(is.null(dir)) dir <- getwd() # set to working directory if no input provided
  outfile <- paste(dir,file,sep="/")
  if(file.exists(outfile)){
      if(!overwrite){
          print(paste("file exists:",outfile),quote=F)
          print("  set overwrite=T to replace",quote=F)
          return()
      }else{
          print(paste("overwriting file:",outfile),quote=F)
          file.remove(outfile)
      }
  }else{
      print(paste("writing new file:",outfile),quote=F)
  }

  # preliminary setup
  oldwidth <- options()$width
  options(width=1000)

  if(verbose) print(paste("opening connection to",outfile),quote=F)
  zz <- file(outfile, open="at")
  sink(zz)
  wl <- function(name){
      # simple function to clean up many repeated commands
      value = mylist[names(mylist)==name]
      writeLines(paste(value," #_",name,sep=""),con=zz)
  }

  writeLines("#C starter file written by R function SS_writestarter")
  writeLines("#C rerun model to get more complete formatting in starter.ss_new")
  writeLines(paste("#C should work with SS version:",mylist$SSversion))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#")

  # strings for control and data file names
  wl("datfile")
  wl("ctlfile")

  wl("init_values_src")
  wl("run_display_detail")
  wl("detailed_age_structure")
  wl("checkup")
  wl("parmtrace")
  wl("cumreport")
  wl("prior_like")
  wl("soft_bounds")
  wl("N_bootstraps")
  wl("last_estimation_phase")
  wl("MCMCburn")
  wl("MCMCthin")
  wl("jitter_fraction")
  wl("minyr_sdreport")
  wl("maxyr_sdreport")
  wl("N_STD_yrs")
  if(mylist$N_STD_yrs>0){
      wl("STD_yr_vec")
  }
  wl("converge_criterion")
  wl("retro_yr")
  wl("min_age_summary_bio")
  wl("depl_basis")
  wl("depl_denom_frac")
  wl("SPR_basis")
  wl("F_report_units")
  wl("F_report_basis")
  writeLines("#")
  writeLines("999")
  options(width=oldwidth)
  sink()
  close(zz)
  if(verbose) print(paste("file written to",outfile),quote=F)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
