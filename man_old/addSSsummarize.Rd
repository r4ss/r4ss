\name{addSSsummarize}
\alias{addSSsummarize}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Add a model to the list of models to compare
}

\description{
  Adds specified quantities from any model to the list of models
  returned from \code{\link{SSsummarize}} for further comparison.
}

\usage{
addSSsummarize(origModels,newModels)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{origModels}{
    A list of models created by \code{\link{SSsummarize}}.
  }
  \item{newModels}{
	A list of models to add to the originals models list.  Each
	new model is an element of the list, and is a list itself
	with possible components described in the details below.
  }
}

\details{
  The models to add do not need to contain all of the entries.
  Missing entries are given NA values, so are not plotted.  Each
  model is a list with the following elements:
  \describe{
  	\item{npars}{the number of parameters in the model}
  	\item{maxgrad}{the maximum gradient component (if used)}
  	\item{nsexes}{the number of sexes}
  	\item{likelihoods}{likelihoods from the model. A data.frame
  		with the 2nd column as names, which matches on names
  		from origModels. Names which do not match will be added
  		SS uses the following names}
		\itemize{
            \item TOTAL
            \item Equil_catch
            \item Survey
            \item Length_comp
            \item Age_comp
            \item Recruitment
            \item recast_Recruitment
            \item Parm_priors
            \item Parm_softbounds
            \item Parm_devs
            \item Crash_Pen
            \item Size_at_age}
  	\item{likelambdas}{NOT IMPLEMENTED}
  	\item{pars}{NOT IMPLEMENTED YET FOR DIFFICULTY IN MATCHING PARAMETERS}
  	\item{parsSD}{NOT IMPLEMENTED YET FOR DIFFICULTY IN MATCHING PARAMETERS}
  	\item{parsphases}{NOT IMPLEMENTED YET FOR DIFFICULTY IN MATCHING PARAMETERS}
  	\item{SpawnBio}{Spawning biomass matrix}
         \itemize{
            \item 1st column is year
            \item 2nd column is spawning biomass in same units as
            			original models (SS reports female spawning biomass)
            \item 3rd column is the standard deviation of estimated
            			spawning biomass
            \item 4th column is a lower bound of the confidence
            			interval to be plotted (say from an MCMC)
            \item 5th column is an upper bound of the confidence
            			interval to be plotted (say from an MCMC)
         }
       \item{Bratio}{Depletion matrix}
           \itemize{
               \item 1st column is year
               \item 2nd column is depletion
               \item 3rd column is the standard deviation of depletion (optional)
               \item 4th column is a lower bound of the confidence interval to be plotted (say from an MCMC)
               \item 5th column is an upper bound of the confidence interval to be plotted (say from an MCMC)
           }
       \item{SPRratio}{SPR ratio matrix}
           \itemize{
               \item 1st column is year
               \item 2nd column is depletion
               \item 3rd column is the standard deviation (optional)
               \item 4th column is a lower bound of the confidence interval to be plotted (say from an MCMC)
               \item 5th column is an upper bound of the confidence interval to be plotted (say from an MCMC)
           }
       \item{recruits}{Recruitment matrix}
			\itemize{
            	\item 1st column is year
            	\item 2nd column is recruitment as in original models (SS reports age-0 recruits)
            	\item 3rd column is the standard deviation (optional)
            	\item 4th column is a lower bound of the confidence interval to be plotted (say from an MCMC)
            	\item 5th column is an upper bound of the confidence interval to be plotted (say from an MCMC)
			}
       \item{recdevs}{Recruitment deviate matrix}
       		\itemize{
       			\item 1st column is year
       			\item 2nd column is deviate (matched with original models)
       		}
       \item{growth}{NOT IMPLEMENTED}
       \item{indices}{Matrix of fits to indices}
       		\itemize{
       			\item 1st column is year
       			\item 2nd column is observed index (data)
       			\item 3rd column is expected index (prediction)
       			\item 4th column is catchability coefficient (q)
       			\item 5th column is standard error of index (total used in fitting)
       			\item 6th column is a likelihood for this point, or enter any value to make sure it plots, or enter NA not to plot the estimate
			}
       \item{InitAgeYrs}{NOT IMPLEMENTED}
   }
}

\value{
  Returns list as is returned from \code{\link{SSsummarize}}, but
  contains additions for the new models.  

  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}

% \references{
%   %% ~put references to the literature/web site here ~
% }

\author{
  Allan Hicks
}

\note{
  This function was made to compare TINSS results and SS results, and assumed that you would
    always start with a list of SS models output from \code{\link{SSsummarize}}.  It has not been tested
    to see how it works when starting with an empty list.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SSsummarize}}
  \code{\link{SSplotComparisons}}
}
\examples{
  \dontrun{
  ######################################
  #DO NOT RUN
tinss1 <- list(npars=A$fit$npar,maxgrad=A$fit$maxgrad,nsexes=1,
               #note, there is an estimated parameter called sd_sbt,
               #      but it is a single value
               SpawnBio=data.frame(c(1964,1965,A$yrs),
                                   c(A$sbo,A$sbo,A$sbt)*1e6,0,
                                   qnorm(0.025,c(A$so,A$so,A$sbt)*1e6,0),
                                   qnorm(0.975,c(A$so,A$so,A$sbt)*1e6,0)),
               Bratio=data.frame(A$yrs,A$sbt/A$sbo,0,
                                 qnorm(0.025,A$sbt/A$sbo,0),
                                 qnorm(0.975,A$sbt/A$sbo,0)),
               SPRratio=data.frame(A$yr,A$spr,0,qnorm(0.025,A$spr,0),
                                   qnorm(0.975,A$spr,0)),
               recruits=data.frame(A$yr,A$nt[,1]*1e6,0,qnorm(0.025,A$nt[,1]*1e5,0),
                                   qnorm(0.975,A$nt[,1]*1e6,0)),
               #I'm not sure exactly what wt are,
               #   but it is important to line them up correctly
               recdevs=data.frame(A$recYrs,A$wt),  
               indices = data.frame(A$iyr,1e6*A$yt,1e6*A$qbt,
                                    rep(A$q,length(A$iyr)),rep(0.4,length(A$iyr)),
                                    rep(0,length(A$iyr)),rep(1,length(A$iyr)))
               )
  tinss <- list(tinss1,tinss1)   #can add more models here


  #add TINSS model to SS models already summarized                
  SSnTINSS <- addSSsummarize(models,tinss)
  mcmcInd <- seq(burnin+1,nrow(A$mc.sbt),thin)
  SSnTINSS$mcmc[[2]] <- data.frame(A$mc.sb0[mcmcInd],
                                   A$mc.sbt[mcmcInd,],
                                   A$mc.depl[mcmcInd,],
                                   A$mc.spr[mcmcInd,],
                                   A$mc.rt[mcmcInd,],
                                   log(A$mcmc[mcmcInd,"Ro"]*1e6),
                                   A$mcmc[mcmcInd,"msy"]*1e6)  
  names(SSnTINSS$mcmc[[2]]) <-
    c("SPB_Virgin",paste("SPB",A$yrs,sep="_"),
      paste("Bratio",A$yrs,sep="_"),
      paste("SPRratio",A$yr,sep="_"),
      paste("Recr",A$yr,sep="_"),"SR_R0","TotYield_MSY")
  modelnames <- c("SS", "TINSS","TINSS.MLE")
  
  SSplotComparisons(SSnTINSS, legendlabels=modelnames,
                    subplot=2,endyr=2011,mcmcVec=c(T,T,F))
  title(main="MCMC")
  SSplotComparisons(SSnTINSS, legendlabels=modelnames,
                    subplot=4,endyr=2011,mcmcVec=c(T,T,F))
  title(main="MCMC")
  ###############################################
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ manip }
\keyword{ list }
