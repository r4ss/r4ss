SStableComparisons <-
  function(summaryoutput,
           models="all",
           likenames=c("TOTAL",
             "Survey",
             "Length_comp",
             "Age_comp",
             "priors",
             "Size_at_age"),
           names=c("R0",
             "steep",
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
           csvfile="parameter_comparison_table.csv",
           verbose=TRUE)
{
  if(verbose) cat("running SStableComparisons\n")
  
  # get stuff from summary output
  n           <- summaryoutput$n
  nsexes      <- summaryoutput$nsexes
  pars        <- summaryoutput$pars
  quants      <- summaryoutput$quants
  likelihoods <- summaryoutput$likelihoods
  npars       <- summaryoutput$npars
  indices     <- summaryoutput$indices
  
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
    if(verbose) cat("name=",name,": ",sep="")
    # get values 
    vals <- bigtable[grep(name, bigtable$Label),]

    # fix scale on a few things
    if(name %in% c("SR_LN(R0)","SR_R0")){
      vals[-1] <- round(exp(vals[-1])/1e6,6)
      vals[1] <- "R0_billions"
    }
    if(name=="SPB_Virg"){
      vals[-1] <- as.numeric(vals[-1])/1e6
      vals[1] <- "SB0_million_mt"
    }
    if(length(grep("SPB",name))>0 & any(nsexes==1)){
      cat("dividing name by 2 for single-sex models:",(1:n)[nsexes==1],"\n")
      for(i in (1:n)[nsexes==1]) vals[1+i] <- vals[1+i]/2
    }

    if(name=="Q"){
      Calc_Q <- aggregate(Calc_Q ~ Model+FleetNum,data=indices,FUN=mean)
      cat("\n")
      fleetvec <- unique(Calc_Q$FleetNum)
      for(f in rev(sort(as.numeric(fleetvec)))){
        vals <- rbind(NA,vals)
        vals[1,1] <- paste("Q_calc_mean_fleet_",f,sep="")
        vals[1,-1] <- Calc_Q$Calc_Q[Calc_Q$FleetNum==f]
      }
    }
    if(verbose) cat("added ",nrow(vals)," row",ifelse(nrow(vals)!=1,"s",""),"\n",sep="")
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
