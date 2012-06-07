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
             "Bratio_2012",
             "SPRratio_2011"),
           modelnames="default",
           csv=FALSE,
           csvdir="workingdirectory",
           csvfile="parameter_comparison_table.csv",
           verbose=TRUE,
           mcmc=FALSE)
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
  
  if(models[1]=="all") models <- 1:n
  ncols <- length(models)
  if(modelnames[1]=="default") modelnames <- paste("model",1:ncols,sep="")
  tab <- as.data.frame(matrix(NA,nrow=0,ncol=ncols+1))
  
  if(!mcmc) {

    likenames <- paste(likenames,"_like",sep="")
    likelihoods$Label <- paste(likelihoods$Label,"_like",sep="")
    names <- c(likenames, names)
  
    nnames <- length(names)

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
      if(((length(grep("SPB",name))>0  | length(grep("SSB",name))>0) & any(nsexes==1))){
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
  }
  
  if(mcmc) {
    nnames <- length(names)
    for(iname in 1:nnames){
      name <- names[iname]
      if(verbose) cat("name=",name,": ",sep="")
      vals <- as.data.frame(matrix(NA,ncol=length(models)+1,nrow=1))
      vals[1] <- name
      for(imodel in models) {   ###loop over models and create a vector of medians to put into tab
        mcmcTable <- summaryoutput$mcmc[[imodel]]
        # get values 
        tmp <- mcmcTable[,grep(name, names(mcmcTable))]  #for future functionality grabbing more than one column
        if(!is.null(dim(tmp))) stop("This only works with a single column from the mcmc. Use a specific name")
        vals[1,imodel+1] <- median(tmp)  #First element is label
      }
      # fix scale on a few things
      if(name %in% c("SR_LN(R0)","SR_R0")) {
        vals[1,-1] <- round(exp(vals[1,-1])/1e6,6)
        vals[1,1] <- "R0_billions"
      }
      if(substring(name,1,4)=="Recr") {
        vals[1,-1] <- round(vals[1,-1]/1e6,6)
        vals[1,1] <-paste(vals[1,1],"billions",sep="_")
      }
      if(substring(name,1,3)%in%c("SPB","SSB") | substring(name,1,8)=="TotYield") {
        vals[1,-1] <- round(vals[1,-1]/1e6,6)
        vals[1,1] <-paste(vals[1,1],"millions",sep="_")
      }
      if(name=="SPB_Virg"){
        vals[1,-1] <- as.numeric(vals[1,-1])/1e6
        vals[1,1] <- "SB0_million_mt"
      }
      if(((length(grep("SPB",name))>0  | length(grep("SSB",name))>0) & any(nsexes==1))){
        cat("dividing name by 2 for single-sex models:",(1:n)[nsexes==1],"\n")
        for(i in (1:n)[nsexes==1]) vals[1,1+i] <- vals[1,1+i]/2
      }
      if(verbose) cat("added an mcmc row\n")
      # add to table
      tab <- rbind(tab, vals)
    }
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
