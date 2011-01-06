SStableComparisons <-
  function(summaryoutput,
           models="all",
           likenames=c("TOTAL",
             "Survey",
             "Length_comp",
             "Age_comp",
             "priors",
             "Size_at_age"),
           names=c("SR_R0",
             "SR_steep",
             "NatM",
             "Q",
             "L_at_Amax",
             "VonBert_K",
             "RecrDev_2008",
             "SPB_Virg",
             "Bratio_2011",
             "SPRratio_2010"),
           modelnames="default",
           csv=FALSE,
           csvdir="workingdirectory",
           csvfile="parameter_comparison_table.csv")
{
  # get stuff from summary output
  n           <- summaryoutput$n
  pars        <- summaryoutput$pars
  quants      <- summaryoutput$quants
  likelihoods <- summaryoutput$likelihoods
  npars       <- summaryoutput$npars

  likenames <- paste(likenames,"_like",sep="")
  likelihoods$Label <- paste(likelihoods$Label,"_like",sep="")
  names <- c(likenames, names)
  
  if(models[1]=="all") models <- 1:n
  ncols <- length(models)
  nnames <- length(names)
  if(modelnames[1]=="default") modelnames <- paste("model",1:ncols,sep="")
  tab <- as.data.frame(matrix(NA,nrow=0,ncol=ncols+1))

  bigtable <- rbind(likelihoods[,c(n+1,models)],
                    pars[,c(n+1,models)],
                    quants[,c(n+1,models)])

  # loop over big list of names to get values
  for(iname in 1:nnames){
    name <- names[iname]
    
    # get values 
    vals <- bigtable[grep(name, bigtable$Label),]

    # fix scale on a few things
    if(name=="SR_R0"){
      vals[-1] <- round(exp(vals[-1])/1e6,6)
      vals[1] <- "R0_billions"
    }
    if(name=="SPB_Virg"){
      vals[-1] <- as.numeric(vals[-1])/1e6
      vals[1] <- "SB0_million_mt"
    }

    if(name=="Q"){
      vals <- rbind(NA,vals)
      vals[1,1] <- "Q_from_indices"
      Calc_Q <- aggregate(mysummary$indices$Calc_Q,by=list(model=mysummary$indices$Model),FUN=mean)$x
      vals[1,-1] <- Calc_Q[models]
    }
    
    # add to table
    tab <- rbind(tab, vals)
  }

  names(tab) <- c("Label",modelnames)
  rownames(tab) <- 1:nrow(tab)

  if(csv){
    if(csvdir=="workingdirectory") csvdir <- getwd()
    fullpath <- paste(csvdir,csvfile,sep="/")
    cat("writing table to:\n  ",fullpath,"\n")
    write.csv(tab,fullpath,row.names=FALSE)
  }
  return(tab)
}
