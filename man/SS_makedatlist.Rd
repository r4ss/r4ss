\name{SS_makedatlist}
\alias{SS_makedatlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  make a list object corresponding to the Stock Synthesis data file
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SS_makedatlist(styr = 1971, endyr = 2001, nseas = 1, months_per_seas = 12, spawn_seas = 1, Nfleet = 1, Nsurveys = 1, N_areas = 1, fleetnames = c("fishery1", "survey1"), surveytiming = 0.5, areas = 1, units_of_catch = 1, se_log_catch = 0.01, Ngenders = 2, Nages = 40, init_equil = 0, catch = NULL, CPUE = NULL, discard_units = 2, discard_data = NULL, meanbodywt = NULL, lbin_method = 2, binwidth = 2, minimum_size = 2, maximum_size = 90, comp_tail_compression = -1e-04, add_to_comp = 1e-04, max_combined_lbin = 0, lbin_vector = seq(22, 90, 2), lencomp = NULL, agebin_vector = 1:25, ageerror = data.frame(rbind(0:40 + 0.5, 0.001, 0:40 + 0.5, seq(0.525, 2.525, 0.05))), agecomp = NULL, Lbin_method = 3, max_combined_age = 1, MeanSize_at_Age_obs = NULL, N_environ_variables = 0, N_environ_obs = 0, N_sizefreq_methods = 0, tag_data = 0, morphcomp_data = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{styr}{
%%     ~~Describe \code{styr} here~~
}
  \item{endyr}{
%%     ~~Describe \code{endyr} here~~
}
  \item{nseas}{
%%     ~~Describe \code{nseas} here~~
}
  \item{months_per_seas}{
%%     ~~Describe \code{months_per_seas} here~~
}
  \item{spawn_seas}{
%%     ~~Describe \code{spawn_seas} here~~
}
  \item{Nfleet}{
%%     ~~Describe \code{Nfleet} here~~
}
  \item{Nsurveys}{
%%     ~~Describe \code{Nsurveys} here~~
}
  \item{N_areas}{
%%     ~~Describe \code{N_areas} here~~
}
  \item{fleetnames}{
%%     ~~Describe \code{fleetnames} here~~
}
  \item{surveytiming}{
%%     ~~Describe \code{surveytiming} here~~
}
  \item{areas}{
%%     ~~Describe \code{areas} here~~
}
  \item{units_of_catch}{
%%     ~~Describe \code{units_of_catch} here~~
}
  \item{se_log_catch}{
%%     ~~Describe \code{se_log_catch} here~~
}
  \item{Ngenders}{
%%     ~~Describe \code{Ngenders} here~~
}
  \item{Nages}{
%%     ~~Describe \code{Nages} here~~
}
  \item{init_equil}{
%%     ~~Describe \code{init_equil} here~~
}
  \item{catch}{
%%     ~~Describe \code{catch} here~~
}
  \item{CPUE}{
%%     ~~Describe \code{CPUE} here~~
}
  \item{discard_units}{
%%     ~~Describe \code{discard_units} here~~
}
  \item{discard_data}{
%%     ~~Describe \code{discard_data} here~~
}
  \item{meanbodywt}{
%%     ~~Describe \code{meanbodywt} here~~
}
  \item{lbin_method}{
%%     ~~Describe \code{lbin_method} here~~
}
  \item{binwidth}{
%%     ~~Describe \code{binwidth} here~~
}
  \item{minimum_size}{
%%     ~~Describe \code{minimum_size} here~~
}
  \item{maximum_size}{
%%     ~~Describe \code{maximum_size} here~~
}
  \item{comp_tail_compression}{
%%     ~~Describe \code{comp_tail_compression} here~~
}
  \item{add_to_comp}{
%%     ~~Describe \code{add_to_comp} here~~
}
  \item{max_combined_lbin}{
%%     ~~Describe \code{max_combined_lbin} here~~
}
  \item{lbin_vector}{
%%     ~~Describe \code{lbin_vector} here~~
}
  \item{lencomp}{
%%     ~~Describe \code{lencomp} here~~
}
  \item{agebin_vector}{
%%     ~~Describe \code{agebin_vector} here~~
}
  \item{ageerror}{
%%     ~~Describe \code{ageerror} here~~
}
  \item{agecomp}{
%%     ~~Describe \code{agecomp} here~~
}
  \item{Lbin_method}{
%%     ~~Describe \code{Lbin_method} here~~
}
  \item{max_combined_age}{
%%     ~~Describe \code{max_combined_age} here~~
}
  \item{MeanSize_at_Age_obs}{
%%     ~~Describe \code{MeanSize_at_Age_obs} here~~
}
  \item{N_environ_variables}{
%%     ~~Describe \code{N_environ_variables} here~~
}
  \item{N_environ_obs}{
%%     ~~Describe \code{N_environ_obs} here~~
}
  \item{N_sizefreq_methods}{
%%     ~~Describe \code{N_sizefreq_methods} here~~
}
  \item{tag_data}{
%%     ~~Describe \code{tag_data} here~~
}
  \item{morphcomp_data}{
%%     ~~Describe \code{morphcomp_data} here~~
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
function(styr=1971,
             endyr=2001,
             nseas=1,
             months_per_seas=12,
             spawn_seas=1,
             Nfleet=1,
             Nsurveys=1,
             N_areas=1,
             fleetnames=c("fishery1","survey1"),
             surveytiming=0.5,
             areas=1,
             units_of_catch=1,
             se_log_catch=0.01,
             Ngenders=2,
             Nages=40,
             init_equil=0,
             catch=NULL,
             CPUE=NULL,
             discard_units=2,
             discard_data=NULL,
             meanbodywt=NULL,
             lbin_method=2,
             binwidth=2,
             minimum_size=2,
             maximum_size=90,
             comp_tail_compression= -0.0001,
             add_to_comp=0.0001,
             max_combined_lbin=0,
             lbin_vector=seq(22, 90, 2),
             lencomp=NULL,
             agebin_vector=1:25,
             ageerror=data.frame(rbind(0:40+.5,.001,0:40+.5,seq(0.525,2.525,0.05))),
             agecomp=NULL,
             Lbin_method=3,
             max_combined_age=1,
             MeanSize_at_Age_obs=NULL,
             N_environ_variables=0,
             N_environ_obs=0,
             N_sizefreq_methods=0,
             tag_data=0,
             morphcomp_data=0
             ){
        SSversion <- "SSv3.04B"

        N_lbins <- length(lbin_vector)
        N_agebins <- length(agebin_vector)

        N_catch <- ifelse(is.null(catch), 0, nrow(catch))
        N_cpue <- ifelse(is.null(CPUE), 0, nrow(CPUE))
        N_discard <- ifelse(is.null(discard_data), 0, nrow(discard_data))
        N_meanbodywt <- ifelse(is.null(meanbodywt), 0, nrow(meanbodywt))
        N_lencomp <- ifelse(is.null(lencomp), 0, nrow(lencomp))
        N_ageerror_definitions <- ifelse(is.null(ageerror), 0, nrow(ageerror)/2)
        N_agecomp <- ifelse(is.null(agecomp), 0, nrow(agecomp))
        N_MeanSize_at_Age_obs <- ifelse(is.null(MeanSize_at_Age_obs), 0, nrow(MeanSize_at_Age_obs))


        fleetinfo1 <- data.frame(rbind(rep(surveytiming,Nfleet+Nsurveys),
                                        rep(areas,Nfleet+Nsurveys)))
        names(fleetinfo1) <- fleetnames
        names(fleetinfo1)[1] <- paste("#",names(fleetinfo1)[1],sep="")
        fleetinfo1$input <- c("#_surveytiming","#_areas")

        fleetinfo2 <- data.frame(rbind(rep(units_of_catch,Nfleet),
                                        rep(se_log_catch,Nfleet)))
        names(fleetinfo2) <- fleetnames[1:Nfleet]
        names(fleetinfo2)[1] <- paste("#",names(fleetinfo2)[1],sep="")
        fleetinfo2$input <- c("#_units_of_catch","#_se_log_catch")

        names(ageerror) <- c("#_age0",paste("age",1:nages,sep=""))

        datlist <- list(SSversion = SSversion,
                        styr = styr,
                        endyr = endyr,
                        nseas = nseas,
                        months_per_seas = months_per_seas,
                        spawn_seas = spawn_seas,
                        Nfleet = Nfleet,
                        Nsurveys = Nsurveys,
                        N_areas = N_areas,
                        fleetnames = fleetnames,
                        surveytiming = surveytiming,
                        areas = areas,
                        units_of_catch = units_of_catch,
                        se_log_catch = se_log_catch,
                        fleetinfo1 = fleetinfo1,
                        fleetinfo2 = fleetinfo2,
                        Ngenders = Ngenders,
                        Nages = Nages,
                        init_equil = init_equil,
                        N_catch = N_catch,
                        catch = catch,
                        N_cpue = N_cpue,
                        CPUE = CPUE,
                        discard_units = discard_units,
                        N_discard = N_discard,
                        discard_data = discard_data,
                        N_meanbodywt = N_meanbodywt,
                        meanbodywt = meanbodywt,
                        lbin_method = lbin_method,
                        binwidth = binwidth,
                        minimum_size = minimum_size,
                        maximum_size = maximum_size,
                        comp_tail_compression = comp_tail_compression,
                        add_to_comp = add_to_comp,
                        max_combined_lbin = max_combined_lbin,
                        N_lbins = N_lbins,
                        lbin_vector = lbin_vector,
                        N_lencomp = N_lencomp,
                        lencomp = lencomp,
                        N_agebins = N_agebins,
                        agebin_vector = agebin_vector,
                        N_ageerror_definitions = N_ageerror_definitions,
                        ageerror = ageerror,
                        N_agecomp = N_agecomp,
                        agecomp = agecomp,
                        Lbin_method = Lbin_method,
                        max_combined_age = max_combined_age,
                        N_MeanSize_at_Age_obs = N_MeanSize_at_Age_obs,
                        MeanSize_at_Age_obs = MeanSize_at_Age_obs,
                        N_environ_variables = N_environ_variables,
                        N_environ_obs = N_environ_obs,
                        N_sizefreq_methods = N_sizefreq_methods,
                        tag_data = tag_data,
                        morphcomp_data = morphcomp_data
                        )
        return(datlist)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
