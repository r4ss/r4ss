% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profile.R
\name{profile}
\alias{profile}
\title{Run a likelihood profile in Stock Synthesis.}
\usage{
profile(
  dir,
  oldctlfile = "control.ss_new",
  masterctlfile = lifecycle::deprecated(),
  newctlfile = "control_modified.ss",
  linenum = NULL,
  string = NULL,
  profilevec = NULL,
  usepar = FALSE,
  globalpar = FALSE,
  parlinenum = NULL,
  parstring = NULL,
  saveoutput = TRUE,
  overwrite = TRUE,
  whichruns = NULL,
  prior_check = TRUE,
  read_like = TRUE,
  exe = "ss",
  verbose = TRUE,
  ...
)
}
\arguments{
\item{dir}{A file path to the directory of interest.
The default value is \code{dir = NULL}, which leads to using
the current working directory.}

\item{oldctlfile}{Source control file. Default = "control.ss_new"}

\item{masterctlfile}{Deprecated. Use \code{oldctlfile} instead.}

\item{newctlfile}{Destination for new control files (must match entry in
starter file). Default = "control_modified.ss".}

\item{linenum}{Line number of parameter to be changed. Can be used instead
of \code{string} or left as NULL. Can be a vector if you are profiling multiple
parameters at the same time.}

\item{string}{String partially matching name of parameter to be changed. Can
be used instead of \code{linenum} or left as NULL. Can be a vector if you are
profiling multiple parameters at the same time.}

\item{profilevec}{Vector of values to profile over. If you are profileing
over multiple parameters at the same time this should be a data.frame or
matrix with a column for each parameter.}

\item{usepar}{Use PAR file from previous profile step for starting values?}

\item{globalpar}{Use global par file (\code{parfile_original_backup.sso}, which is
automatically copied from original \code{ss.par}) for all runs instead
of the par file from each successive run}

\item{parlinenum}{Line number in par file to change (if usepar = TRUE).
Can be a vector if you are profiling multiple parameters at the same time.}

\item{parstring}{String in par file preceding line number to change as
an alternative to parlinenum (only needed if usepar = TRUE).
Can be a vector if you are profiling multiple parameters at the same time.}

\item{saveoutput}{Copy output .sso files to unique names.  Default = TRUE.}

\item{overwrite}{Overwrite any existing .sso files. Default = TRUE. If FALSE,
then some runs may be skipped.}

\item{whichruns}{Optional vector of run indices to do. This can be used to
re-run a subset of the cases in situations where the function was
interrupted or some runs fail to converge. Must be a subset of 1:n, where n
is the length of profilevec.}

\item{prior_check}{Check to make sure the starter file is set to include
the prior likelihood contribution in the total likelihood.  Default = TRUE.}

\item{read_like}{Read the table of likelihoods from each model as it finishes.
Default = TRUE. Changing to FALSE should allow the function to play through
even if something is wrong with reading the table.}

\item{exe}{Executable name. Can be just the name of the executable
file if it is in the specified directory or in the user's PATH. Can
also include the absolute path or a path relative to the specified
directory. Needs to be a single character string, not a vector.
On Windows, \code{exe} can optionally have the \code{.exe} extension appended;
on Unix-based systems (i.e., Mac and Linux), no extension should be
included.}

\item{verbose}{A logical value specifying if output should be printed
to the screen.}

\item{...}{Additional arguments passed to \code{\link[=run]{run()}}, such as
\code{extras}, \code{show_in_console}, and \code{skipfinished}.}
}
\description{
Iteratively changes the control file for the chosen parameter. This
function was formerly called \code{SS_profile()}.
}
\note{
The starting values used in this profile are not ideal and some models
may not converge. Care should be taken in using an automated tool like this,
and some models are likely to require rerunning with alternate starting
values.

Also, someday this function will be improved to work directly with the
plotting function \code{\link[=SSplotProfile]{SSplotProfile()}}, but they don't yet work well
together. Thus, even if \code{\link[=profile]{profile()}} is used, the output should
be read using \code{\link[=SSgetoutput]{SSgetoutput()}} or by multiple calls to
\code{\link[=SS_output]{SS_output()}} before sending to \code{\link[=SSplotProfile]{SSplotProfile()}}.
}
\examples{
\dontrun{
# note: don't run this in your main directory
# make a copy in case something goes wrong
mydir <- "C:/ss/Simple - Copy"

# the following commands related to starter.ss could be done by hand
# read starter file
starter <- SS_readstarter(file.path(mydir, "starter.ss"))
# change control file name in the starter file
starter[["ctlfile"]] <- "control_modified.ss"
# make sure the prior likelihood is calculated
# for non-estimated quantities
starter[["prior_like"]] <- 1
# write modified starter file
SS_writestarter(starter, dir = mydir, overwrite = TRUE)

# vector of values to profile over
h.vec <- seq(0.3, 0.9, .1)
Nprofile <- length(h.vec)

# run profile command
profile <- profile(
  dir = mydir, # directory
  # "NatM" is a subset of one of the
  # parameter labels in control.ss_new
  oldctlfile = "control.ss_new",
  newctlfile = "control_modified.ss",
  string = "steep",
  profilevec = h.vec
)


# read the output files (with names like Report1.sso, Report2.sso, etc.)
profilemodels <- SSgetoutput(dirvec = mydir, keyvec = 1:Nprofile)
# summarize output
profilesummary <- SSsummarize(profilemodels)

# OPTIONAL COMMANDS TO ADD MODEL WITH PROFILE PARAMETER ESTIMATED
MLEmodel <- SS_output("C:/ss/SSv3.24l_Dec5/Simple")
profilemodels[["MLE"]] <- MLEmodel
profilesummary <- SSsummarize(profilemodels)
# END OPTIONAL COMMANDS

# plot profile using summary created above
SSplotProfile(profilesummary, # summary object
  profile.string = "steep", # substring of profile parameter
  profile.label = "Stock-recruit steepness (h)"
) # axis label

# make timeseries plots comparing models in profile
SSplotComparisons(profilesummary, legendlabels = paste("h =", h.vec))


###########################################################################

# example two-dimensional profile
# (e.g. over 2 of the parameters in the low-fecundity stock-recruit function)
base_dir <- "c:/mymodel"

dir_profile_SR <- file.path(base_dir, "Profiles/Zfrac_and_Beta")

# make a grid of values in both dimensions Zfrac and Beta
# vector of values to profile over
Zfrac_vec <- seq(from = 0.2, to = 0.6, by = 0.1)
Beta_vec <- c(0.5, 0.75, 1.0, 1.5, 2.0)
par_table <- expand.grid(Zfrac = Zfrac_vec, Beta = Beta_vec)
nrow(par_table)
## [1] 25
head(par_table)
##   Zfrac Beta
## 1   0.2 0.50
## 2   0.3 0.50
## 3   0.4 0.50
## 4   0.5 0.50
## 5   0.6 0.50
## 6   0.2 0.75

# run profile command
profile <- profile(
  dir = dir_profile_SR, # directory
  oldctlfile = "control.ss_new",
  newctlfile = "control_modified.ss",
  string = c("Zfrac", "Beta"),
  profilevec = par_table,
  extras = "-nohess" # argument passed to run()
)

# get model output
profilemodels <- SSgetoutput(
  dirvec = dir_profile_SR,
  keyvec = 1:nrow(par_table), getcovar = FALSE
)
n <- length(profilemodels)
profilesummary <- SSsummarize(profilemodels)

# add total likelihood (row 1) to table created above
par_table[["like"]] <- as.numeric(profilesummary[["likelihoods"]][1, 1:n])

# reshape data frame into a matrix for use with contour
like_matrix <- reshape2::acast(par_table, Zfrac ~ Beta, value.var = "like")

# make contour plot
contour(
  x = as.numeric(rownames(like_matrix)),
  y = as.numeric(colnames(like_matrix)),
  z = like_matrix
)
}

}
\seealso{
\code{\link[=SSgetoutput]{SSgetoutput()}},
\code{\link[=SS_changepars]{SS_changepars()}}, \code{\link[=SS_parlines]{SS_parlines()}}

Other run functions: 
\code{\link{copy_SS_inputs}()},
\code{\link{jitter}()},
\code{\link{populate_multiple_folders}()},
\code{\link{retro}()},
\code{\link{run}()},
\code{\link{tune_comps}()}

Other profile functions: 
\code{\link{PinerPlot}()},
\code{\link{SSplotProfile}()}
}
\author{
Ian G. Taylor, Kathryn L. Doering, Kelli F. Johnson,
Chantel R. Wetzel, James T. Thorson
}
\concept{profile functions}
\concept{run functions}