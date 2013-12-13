##' Work some of Jim Thorson's Laplace magic
##'
##' Jim will need to fill in stuff here.
##' @title 
##' @param File 
##' @param Input_SD_Group_Vec 
##' @param CTL_linenum_List 
##' @param ESTPAR_num_List 
##' @param PAR_num_Vec 
##' @param Int_Group_List 
##' @param Version 
##' @param StartFromPar 
##' @param Intern 
##' @param ReDoBiasRamp 
##' @param BiasRamp_linenum_Vec 
##' @param CTL_linenum_Type 
##' @return 
##' @author James Thorson
NegLogInt_Fn <-
  function(File=NA, Input_SD_Group_Vec,
           CTL_linenum_List, ESTPAR_num_List,
           PAR_num_Vec, Int_Group_List, Version=5,
           StartFromPar=TRUE, Intern=TRUE,
           ReDoBiasRamp=FALSE, BiasRamp_linenum_Vec=NULL,
           CTL_linenum_Type=NULL){

  # Directory
  if(is.na(File)) File = paste(getwd(),"/",sep="")

  # Error messages
  if(ReDoBiasRamp==TRUE & is.null(BiasRamp_linenum_Vec)) error("If ReDoBiasRamp==TRUE, then BiasRamp_linenum_Vec must be specified")

  # Make sure print is high enough for when passing values to ADMB
  options(digits=15)

  # Iteration tracker
  Iteration = Iteration + 1
  assign("Iteration", value=Iteration, envir=.GlobalEnv)

  # Transform parameter vector
  SD_Group_Vec = Input_SD_Group_Vec

  # Modify inputs when necessary
  if(is.null(CTL_linenum_Type)) CTL_linenum_Type = rep(NA, length(SD_Group_Vec))

  # Write record to file (part 1)
  if(!("Optimization_record.txt" %in% list.files(File))) write("Start optimization",file=paste(File,"Optimization_record.txt",sep=""),append=FALSE)
  write("",file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
  write(date(),file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
  write(paste("Iteration",Iteration),file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
  write(paste("SD_Group_Vec",paste(SD_Group_Vec,collapse=" ")),file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)

  # If ss3.par is availabile from the last iteration then use it as starting point
  STARTER = SS_readstarter(paste(File,"Starter.SS",sep=""), verbose=FALSE)
  if( paste("ss3_",Iteration-1,".par",sep="") %in% list.files(File) & StartFromPar==TRUE ){
    STARTER$init_values_src = 1
    PAR_0 = scan(paste(File,"ss3_",Iteration-1,".par",sep=""), comment.char="#", quiet=TRUE)
  }else{
    STARTER$init_values_src = 0
  }
  SS_writestarter(STARTER, dir=File, file="starter.ss", overwrite=TRUE, verbose=FALSE)

  # Read CTL
  CTL = readLines(paste(File,STARTER$ctlfile,sep=""))
  # Modify CTL
  for(ParI in 1:length(SD_Group_Vec)){
    for(CtlLineI in 1:length(CTL_linenum_List[[ParI]])){
      Temp = as.vector(unlist(sapply(CTL[CTL_linenum_List[[ParI]][CtlLineI]], FUN=function(Char){strsplit(Char," ")[[1]]})))
      #Temp = sapply(CTL[CTL_linenum_List[[ParI]][CtlLineI]]," ", FUN=strsplit)[[1]]
      Temp = as.vector(unlist(sapply(Temp, FUN=function(Char){strsplit(Char,"\t")[[1]]})))
      Temp = Temp[which(Temp!="")]
      if(length(grep("#",Temp))>=1) Temp = Temp[-(grep("#",Temp):length(Temp))]
      Temp = as.numeric(Temp)
      #print(Temp)
      #print(CTL_linenum_Type[ParI])
      if(is.na(CTL_linenum_Type[ParI])){
        if(length(Temp)==7) CTL_linenum_Type[ParI] = "Short_Param"
        if(length(Temp)==14) CTL_linenum_Type[ParI] = "Long_Penalty"
      }
      if(CTL_linenum_Type[ParI] %in% c("Short_Param","Long_Param")){
        Temp[3] = SD_Group_Vec[ParI]
        Temp[7] = -1 * abs(Temp[7])
        # Modify values of PAR file for short-line values
        if( "PAR_0" %in% ls() ){
          PAR_0[PAR_num_Vec[ParI]] = SD_Group_Vec[ParI]
        }
        #if(ParI==2){
        #  assign("Temp", value=Temp, envir=.GlobalEnv)
        #  stop()
        #}
      }
      if(CTL_linenum_Type[ParI]=="Long_Penalty"){
        Temp[12] = SD_Group_Vec[ParI]
      }
      CTL[CTL_linenum_List[[ParI]][CtlLineI]] = paste(Temp, collapse=" ")
      #if(ParI==2){
        #assign("Temp", value=Temp, envir=.GlobalEnv)
        #stop()
      #}
    }
  }
  #assign("CTL", value=CTL, envir=.GlobalEnv)
  #stop()
  # Write CTL
  writeLines(CTL,paste(File,STARTER$ctlfile,sep=""))
  if( "PAR_0" %in% ls() ) write(PAR_0, file=paste(File,"ss3.par",sep=""), ncolumns=10)

  # Run SS
  setwd(File)
    shell("ss3 -nohess -cbs 500000000 -gbs 500000000",intern=Intern)
    Sys.sleep(1)

  # Check convergence
  Converged = FALSE
  if("ss3.par" %in% list.files(File)){
    # Move PAR files
    file.rename(from=paste(File,"ss3.par",sep=""), to=paste(File,"ss3_",Iteration,"-first.par",sep=""))
    # Read and check
    PAR = scan(paste(File,"ss3_",Iteration,"-first.par",sep=""), what="character", quiet=TRUE)
    if( ifelse(is.na(as.numeric(PAR[11])),FALSE,as.numeric(PAR[16])<1) ){
      Converged = TRUE
    }else{
      write(paste("*** Optimization ",1," didn't converge ***",sep=""),file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
    }
  }

  # Try re-running with default starting values
  if(Converged==FALSE){
    # Change starter to take PAR file
    STARTER = SS_readstarter(paste(File,"Starter.SS",sep=""), verbose=FALSE)
    STARTER$init_values_src = 1
    SS_writestarter(STARTER, dir=File, file="starter.ss", overwrite=TRUE, verbose=FALSE)
    # Loop through all previous start values
    PreviousIteration = 0
    while(Converged==FALSE & PreviousIteration<=Iteration){
      # Read in original estimate
      if(PreviousIteration==0) PAR_0 = scan(paste(File,"ss3_",PreviousIteration,".par",sep=""), comment.char="#", quiet=TRUE)
      if(PreviousIteration>=1 & PreviousIteration<Iteration) PAR_0 = scan(paste(File,"ss3_",PreviousIteration,"-first.par",sep=""), comment.char="#", quiet=TRUE)
      if(PreviousIteration==Iteration & "ss3_init.par"%in%list.files(File)) PAR_0 = scan(paste(File,"ss3_init.par",sep=""), comment.char="#", quiet=TRUE)
      # Modify values of PAR file for short-line values
      for(ParI in 1:length(SD_Group_Vec)){
        if(length(Temp)==7){
          if( "PAR_0" %in% ls() ){
            PAR_0[PAR_num_Vec[ParI]] = SD_Group_Vec[ParI]
          }
        }
      }
      if( "PAR_0" %in% ls() ) write(PAR_0, file=paste(File,"ss3.par",sep=""), ncolumns=10)
      # Run SS
      shell("ss3.exe -nohess -cbs 500000000 -gbs 500000000",intern=Intern)
      Sys.sleep(1)
      # Check convergence
      if("ss3.par" %in% list.files(File)){
        # Move PAR files
        file.copy(from=paste(File,"ss3.par",sep=""), to=paste(File,"ss3_",Iteration,"-first.par",sep=""), overwrite=TRUE)
        file.remove(paste(File,"ss3.par",sep=""))
        # Read and check
        PAR = scan(paste(File,"ss3_",Iteration,"-first.par",sep=""), what="character", quiet=TRUE)
        if( ifelse(is.na(as.numeric(PAR[11])),FALSE,as.numeric(PAR[16])<1) ){
          Converged = TRUE
          write(paste("*** Optimization ",2,"-",PreviousIteration," did converge ***",sep=""),file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
        }else{
          write(paste("*** Optimization ",2,"-",PreviousIteration," didn't converge ***",sep=""),file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
        }
      }
      # Increment
      PreviousIteration = PreviousIteration + 1
    }
  }

  # Only calculate Integral if model is converged
  if(Converged==TRUE){
    # Re-run to get Hessian
    STARTER = SS_readstarter(paste(File,"Starter.SS",sep=""), verbose=FALSE)
      STARTER$init_values_src = 1
      SS_writestarter(STARTER, dir=File, file="starter.ss", overwrite=TRUE, verbose=FALSE)
      file.copy(from=paste(File,"ss3_",Iteration,"-first.par",sep=""), to=paste(File,"ss3.par",sep=""), overwrite=TRUE)
      file.remove(paste(File,"ss3.std",sep=""))
      shell("ss3 -maxfn 0 -cbs 500000000 -gbs 500000000",intern=Intern)
      Sys.sleep(1)

    # Estimate new bias ramp
    SsOutput = try(SS_output(File, covar=TRUE, forecast=FALSE), silent=TRUE)
    if( "ss3.std" %in% list.files(File) & file.info(paste(File,"ss3.std",sep=""))$size>0 & ReDoBiasRamp==TRUE & class(SsOutput)!='try-error' ){
      BiasRamp = SS_fitbiasramp(SsOutput, altmethod="psoptim", print=FALSE, plot=FALSE)
      file.remove(paste(File,"ss3.std",sep=""))
      # Put into CTL
      CTL = readLines(paste(File,STARTER$ctlfile,sep=""))
        CTL[BiasRamp_linenum_Vec] = apply(BiasRamp$df, MARGIN=1, FUN=paste, collapse=" ")
      writeLines(CTL,paste(File,STARTER$ctlfile,sep=""))
      # Re-run to get Hessian
      shell("ss3 -cbs 500000000 -gbs 500000000",intern=Intern)
      Sys.sleep(1)
    }
  }

  # Check for STD
  Converged = FALSE
  if( "ss3.std"%in%list.files(File) & file.info(paste(File,"ss3.std",sep=""))$size>0 ) Converged=TRUE

  # If STD exists, then approximate marginal likelihood
  if(Converged==TRUE){
    # Save objects for replicating analysis
    file.rename(from=paste(File,"ss3.par",sep=""), to=paste(File,"ss3_",Iteration,".par",sep=""))
      file.rename(from=paste(File,"ss3.std",sep=""), to=paste(File,"ss3_",Iteration,".std",sep=""))
      file.rename(from=paste(File,"ss3.cor",sep=""), to=paste(File,"ss3_",Iteration,".cor",sep=""))
      file.rename(from=paste(File,"admodel.hes",sep=""), to=paste(File,"admodel_",Iteration,".hes",sep=""))
      file.rename(from=paste(File,"Report.sso",sep=""), to=paste(File,"Report_",Iteration,".sso",sep=""))
      file.copy(from=paste(File,STARTER$datfile,sep=""), to=paste(File,STARTER$datfile,"_",Iteration,".dat",sep=""))
      file.copy(from=paste(File,STARTER$ctlfile,sep=""), to=paste(File,STARTER$ctlfile,"_",Iteration,".ctl",sep=""))

    # Read in some stuff
    STD = scan(paste(File,"ss3_",Iteration,".std",sep=""), what="character", quiet=TRUE)
      STD = data.frame(matrix(STD[-c(1:(which(STD=="1")[1]-1))], ncol=4, byrow=TRUE), stringsAsFactors=FALSE)
    PAR = scan(paste(File,"ss3_",Iteration,".par",sep=""), comment.char="#", quiet=TRUE)
    DIAG = read.admbFit(paste(File,"ss3_",Iteration,sep=""))
    HESS = getADMBHessian(File=File,FileName=paste("admodel_",Iteration,".hes",sep=""))
    # Calculate Hessian
    cov <- pseudoinverse(HESS$hes)
    scale <- HESS$scale
    cov.bounded <- cov*(scale %o% scale)
    #se <- sqrt(diag(cov.bounded))
    #cor <- cov.bounded/(se %o% se)
    Hess = pseudoinverse(cov.bounded)

    # Confirm that correct parameters are being included in Hessian
    if(Iteration==1){
      write("RECORD FOR PARAMETERS IN INTEGRAL",file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
      for(IntI in 1:length(Int_Group_List)){
        Temp = unlist(ESTPAR_num_List[Int_Group_List[[IntI]]])
        write(paste("Group",IntI),file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
        write.table(STD[Temp,],file=paste(File,"Optimization_record.txt",sep=""),append=TRUE, col.names=FALSE, row.names=FALSE)
      }
    }

    # Calculate NLL (while adding in constant of integration for random-walk coefficients)
    NLL = DIAG$nloglike
    for(ParI in 1:length(SD_Group_Vec)){
      # Add in constant of integration for "Long_Penalty" parameters
      if(CTL_linenum_Type[ParI]=="Long_Penalty"){
        NLL = NLL + -1 * (-log(2*pi)/2 - log(SD_Group_Vec[ParI])) * length(ESTPAR_num_List[[ParI]])
      }
    }
    # Add in constant of proportionality for recruitment (i.e. to account for Rick's bias-correction ramp)
    BiasAdj = readLines(paste(File,"Report_",Iteration,".sso",sep=""))
    BiasAdjStart = pmatch("SPAWN_RECRUIT",BiasAdj) + 7
    BiasAdjTable = read.table(paste(File,"Report_",Iteration,".sso",sep=""), header=TRUE, nrows=2, skip=BiasAdjStart, comment.char="#")
    SigmaR = as.numeric(strsplit(BiasAdj[BiasAdjStart-4]," ")[[1]][1])
    # Deal with eras
    RecDevPen = matrix(NA,nrow=3,ncol=2,dimnames=list(c("Early","Main","Forecast"),c("negative-Rick","full")))
    # Deal with early era
    RecDevPen['Early','full'] = -1 * (-log(SigmaR) * BiasAdjTable[2,2])
    RecDevPen['Early','negative-Rick'] = -log(SigmaR) * BiasAdjTable[2,2]*BiasAdjTable[2,5]
    # Deal with main era
    RecDevPen['Main','full'] = -1 * (-log(SigmaR) * BiasAdjTable[1,2])
    RecDevPen['Main','negative-Rick'] = -log(SigmaR) * BiasAdjTable[1,2]*BiasAdjTable[1,5]
    # Deal with forecast era
    RecDevPen['Forecast','full'] = -1 * (-log(SigmaR) * length(which(STD[,2]=="Fcast_recruitments")))
    RecDevPen['Forecast','negative-Rick'] = 0
    # Add into NLL and record
    NLL = NLL + sum(RecDevPen)
    write.table(RecDevPen, file=paste(File,"ss3_",Iteration,".pen",sep=""))
    write(c("","sum(RecDevPen) = ",sum(RecDevPen)), file=paste(File,"ss3_",Iteration,".pen",sep=""), append=TRUE)

    # Approximate integral using Laplace Approximation
    Int_num_List = vector("list", length=length(Int_Group_List))
    LnDet = rep(0, length(Int_Group_List))
    for(IntI in 1:length(Int_Group_List)){
      # Only calculate if necessary
      if( length(unlist(ESTPAR_num_List[Int_Group_List[[IntI]]])) > 0 ){
        # Determine indices for integral
        Int_num_List[[IntI]] = unlist(ESTPAR_num_List[Int_Group_List[[IntI]]])
        #Version 1 -- use full hessian
        if(Version==1){
          if(IntI==1){
            LnDet[IntI] = determinant(Hess, logarithm=TRUE)$modulus[[1]]
          }
          if(IntI>=2) LnDet[IntI] = 0
        }
        #Version 5 -- use back-transformed hessian, use subset
        if(Version==5){
          #Hess2 <- cov * solve(scale %o% scale)
          #Which2 = 38 + 1:46
          LnDet[IntI] = determinant(Hess[Int_num_List[[IntI]],Int_num_List[[IntI]]], logarithm=TRUE)$modulus[[1]]
        }
        #Version 6 -- use subset of covariance calculated from COR file
        if(Version==6){
          Cov <- DIAG$cov
          LnDet[IntI] = -1 * determinant(Cov[Int_num_List[[IntI]],Int_num_List[[IntI]]], logarithm=TRUE)$modulus[[1]]
        }
      }
    }

    # Calculate combined objective function
    Ln_Integral = log(2*pi) + (-1/2)*sum(LnDet) + -1*NLL
    #Integral = 2*pi * sqrt(1/exp(LnDet)) * exp(-NLL)

    # Write record to file (part 2)
    write(paste("LnLike",-NLL),file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
    write(paste("NegLnDet",paste(-LnDet,collapse=" ")),file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
  }else{
    # Indicate that this model didn't converge
    if("ss3.par" %in% list.files(File)) file.remove(paste(File,"ss3.par",sep=""))
    Ln_Integral = -1e10 * sum( SD_Group_Vec )
  }
  write(paste("Ln_Integral",Ln_Integral),file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)

  return(-1*Ln_Integral)
}
