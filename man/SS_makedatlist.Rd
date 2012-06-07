\name{SS_makedatlist}
\alias{SS_makedatlist}
\title{
  make a list for SS data
}
\description{
  create a list similar to those built by \code{\link{SS_readdat}} which
  can be written to a Stock Synthesis data file using
  \code{\link{SS_writedat}}. In hindsight, this function doesn't seem
  very useful and I haven't taken time to describe the arguments below.
}
\usage{
SS_makedatlist(styr = 1971, endyr = 2001, nseas = 1, months_per_seas =
12, spawn_seas = 1, Nfleet = 1, Nsurveys = 1, N_areas = 1, fleetnames =
c("fishery1", "survey1"), surveytiming = 0.5, areas = 1, units_of_catch
= 1, se_log_catch = 0.01, Ngenders = 2, Nages = 40, init_equil = 0,
catch = NULL, CPUE = NULL, N_discard_fleets = 0, discard_data = NULL,
meanbodywt = NULL, DF_for_meanbodywt = 30, lbin_method = 2,
binwidth = 2, minimum_size = 2,
maximum_size = 90, comp_tail_compression = -1e-04, add_to_comp = 1e-04,
max_combined_lbin = 0, lbin_vector = seq(22, 90, 2), lencomp = NULL,
agebin_vector = 1:25, ageerror = data.frame(rbind(0:40 + 0.5, 0.001,
0:40 + 0.5, seq(0.525, 2.525, 0.05))), agecomp = NULL, Lbin_method = 3,
max_combined_age = 1, MeanSize_at_Age_obs = NULL, N_environ_variables =
0, N_environ_obs = 0, N_sizefreq_methods = 0, do_tags = 0,
morphcomp_data = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{styr}{
    start year of the model
  }
  \item{endyr}{
    end year of the model
  }
  \item{nseas}{
    number of seasons
  }
  \item{months_per_seas}{
    months per seasons
  }
  \item{spawn_seas}{
    spawning season
  }
  \item{Nfleet}{
    number of fishing fleets
  }
  \item{Nsurveys}{
    number of surveys
  }
  \item{N_areas}{
    number of areas
  }
  \item{fleetnames}{
    names of fleets
  }
  \item{surveytiming}{
    vector of survey timings
  }
  \item{areas}{
    area definitions for each fleet or survey
  }
  \item{units_of_catch}{
    units of catch for each fleet
  }
  \item{se_log_catch}{
    ... description to be added ...
  }
  \item{Ngenders}{
    ... description to be added ...
  }
  \item{Nages}{
    ... description to be added ...
  }
  \item{init_equil}{
    ... description to be added ...
  }
  \item{catch}{
    ... description to be added ...
  }
  \item{CPUE}{
        ... description to be added ... %%     ~~Describe \code{CPUE} here~~
  }
  \item{N_discard_fleets}{
    Number of fleets with discard data.
  }
  \item{discard_data}{
    Discard data (if exists).
  }
  \item{meanbodywt}{
        ... description to be added ... %%     ~~Describe \code{meanbodywt} here~~
  }
  \item{lbin_method}{
        ... description to be added ... %%     ~~Describe \code{lbin_method} here~~
  }
  \item{binwidth}{
        ... description to be added ... %%     ~~Describe \code{binwidth} here~~
  }
  \item{minimum_size}{
        ... description to be added ... %%     ~~Describe \code{minimum_size} here~~
  }
  \item{maximum_size}{
        ... description to be added ... %%     ~~Describe \code{maximum_size} here~~
  }
  \item{comp_tail_compression}{
        ... description to be added ... %%     ~~Describe \code{comp_tail_compression} here~~
  }
  \item{add_to_comp}{
        ... description to be added ... %%     ~~Describe \code{add_to_comp} here~~
  }
  \item{max_combined_lbin}{
        ... description to be added ... %%     ~~Describe \code{max_combined_lbin} here~~
  }
  \item{lbin_vector}{
        ... description to be added ... %%     ~~Describe \code{lbin_vector} here~~
  }
  \item{lencomp}{
        ... description to be added ... %%     ~~Describe \code{lencomp} here~~
  }
  \item{agebin_vector}{
        ... description to be added ... %%     ~~Describe \code{agebin_vector} here~~
  }
  \item{ageerror}{
        ... description to be added ... %%     ~~Describe \code{ageerror} here~~
  }
  \item{agecomp}{
        ... description to be added ... %%     ~~Describe \code{agecomp} here~~
  }
  \item{Lbin_method}{
        ... description to be added ... %%     ~~Describe \code{Lbin_method} here~~
  }
  \item{max_combined_age}{
        ... description to be added ... %%     ~~Describe \code{max_combined_age} here~~
  }
  \item{MeanSize_at_Age_obs}{
        ... description to be added ... %%     ~~Describe \code{MeanSize_at_Age_obs} here~~
  }
  \item{N_environ_variables}{
        ... description to be added ... %%     ~~Describe \code{N_environ_variables} here~~
  }
  \item{N_environ_obs}{
        ... description to be added ... %%     ~~Describe \code{N_environ_obs} here~~
  }
  \item{N_sizefreq_methods}{
        ... description to be added ... %%     ~~Describe \code{N_sizefreq_methods} here~~
  }
  \item{do_tags}{
    Include tag data? NOT IMPLEMENTED YET.
  }
  \item{morphcomp_data}{
        ... description to be added ... %%     ~~Describe \code{morphcomp_data} here~~
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
  Ian Taylor
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_readdat}}, \code{\link{SS_writedat}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
