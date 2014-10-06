##' Perform SS implementation of Laplace Approximation
##'
##' (Attempt to) perform the SS implementation of the Laplace Approximation
##' from Thorson, Hicks and Methot (2014) ICES J. Mar. Sci. 
##'
##' @param File Directory containing Stock Synthesis files
##' (e.g., "C:/Users/James Thorson/Desktop/") 
##' @param Input_SD_Group_Vec Vector where each element is the standard deviation
##' for a group of random effects (e.g., a model with a single group of random
##' effects will have Input_SD_Group_Vec be a vector of length one) 
##' @param CTL_linenum_List List (same length as \code{Input_SD_Group_Vec}),
##' where each
##' element is a vector giving the line number(s) for the random effect standard
##' deviation parameter or penalty in the CTL file (and where each line will
##' correspond to a 7-parameter or 14-parameter line). 
##' @param ESTPAR_num_List List (same length as \code{Input_SD_Group_Vec}),
##' where each
##' element is a vector giving the parameter number for the random effect
##' coefficients in that group of random effects. These "parameter numbers"
##' correspond to the number of these parameters in the list of parameters in the
##' "ss3.std" output file.
##' @param PAR_num_Vec Vector giving the number in the "ss3.par" vector for each
##' random effect coefficient. 
##' @param Int_Group_List List where each element is a vector, providing a way of
##' grouping different random effect groups into a single category. This is not
##' used (but input is still required) when \code{Version=1}.
##' @param Version Integer (options are 1, 5, and 6) giving the type of Laplace
##' Approximation. I recommend 1.
##' @param StartFromPar Logical flag (TRUE or FALSE) saying whether to start each
##' round of optimization from a "ss3.par" file (I recommend TRUE) 
##' @param Intern Logical flag saying whether to display all ss3 runtime output
##' in the R terminal
##' @param ReDoBiasRamp Logical flag saying whether to re-do the bias ramp
##' (using \code{\link{SS_fitbiasramp}}) each time Stock Synthesis is run.
##' @param BiasRamp_linenum_Vec Vector giving the line numbers from the CTL file
##' that contain the information about the bias ramp. 
##' @param CTL_linenum_Type Character vector (same length as
##' \code{Input_SD_Group_Vec}),
##' where each element is either "Short_Param", "Long_Penalty", "Long_Penalty".
##' Default is NULL, and if not explicitly specified the program will attempt to
##' detect these automatically based on the length of relevant lines from the CTL
##' file.
##' @param systemcmd Should R call SS using "system" function intead of "shell".
##' This may be required when running R in Emacs on Windows. Default = FALSE.
##' @author James Thorson
##' @export
##' @references Thorson, J.T., Hicks, A.C., and Methot, R.D. 2014. Random
##' effect estimation of time-varying factors in Stock Synthesis. ICES J. Mar.
##' Sci.

NegLogInt_Fn <-
  function(File=NA, Input_SD_Group_Vec,
           CTL_linenum_List, ESTPAR_num_List,
           PAR_num_Vec, Int_Group_List, Version=5,
           StartFromPar=TRUE, Intern=TRUE,
           ReDoBiasRamp=FALSE, BiasRamp_linenum_Vec=NULL,
           CTL_linenum_Type=NULL,systemcmd=FALSE){
  # figure out operating system
  OS <- "Mac" # don't know the version$os info for Mac
  if(length(grep("linux",version$os)) > 0) OS <- "Linux"
  if(length(grep("mingw",version$os)) > 0) OS <- "Windows"

  # Directory
  if(is.na(File)) File = paste(getwd(),"/",sep="")

  if( "Iteration.txt" %in% list.files(File)){
      Iteration <- read.table(file=file.path(File,"Iteration.txt"))[[1]]
  }else{
      Iteration <- 0
  }
  
  # Error messages
  if(ReDoBiasRamp==TRUE & is.null(BiasRamp_linenum_Vec)){
      stop("If ReDoBiasRamp==TRUE, then BiasRamp_linenum_Vec must be specified")
  }

  # Make sure print is high enough for when passing values to ADMB
  options(digits=15)

  # Iteration tracker (setting as a global variable for reasons that Jim Thorson can explain)
  Iteration <- Iteration + 1
  #  writing Iteration to a file to avoid CRAN rules about global variables
  write(Iteration,file=file.path(File,"Iteration.txt"))
  #  alternative method would be to read it out of Optimization_record.txt using code
  #  like the following:
  #    record <- readLines(file.path(File,"Optimization_record.txt"))
  #    Iterations <- as.numeric(substring(record[grep("Iteration",record)],11))
  #    Iteration <- max(Iterations)
  
  # Transform parameter vector
  SD_Group_Vec = Input_SD_Group_Vec

  # Modify inputs when necessary
  if(is.null(CTL_linenum_Type)) CTL_linenum_Type = rep(NA, length(SD_Group_Vec))

  # Write record to file (part 1)
  if(!("Optimization_record.txt" %in% list.files(File))){
      write("Start optimization",file=paste(File,"Optimization_record.txt",sep=""),
            append=FALSE)
  }
  write("",file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
  write(date(),file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
  write(paste("Iteration",Iteration),file=paste(File,"Optimization_record.txt",sep=""),
        append=TRUE)
  write(paste("SD_Group_Vec",paste(SD_Group_Vec,collapse=" ")),
        file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)

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
      Temp = as.vector(unlist(sapply(CTL[CTL_linenum_List[[ParI]][CtlLineI]],
          FUN=function(Char){strsplit(Char," ")[[1]]})))
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
  command <- "ss3 -nohess -cbs 500000000 -gbs 500000000"
  if(OS!="Windows"){
    command <- paste("./",command,sep="")
  }
  if(OS=="Windows" & !systemcmd){
    shell(cmd=command,intern=Intern)
  }else{
    system(command,intern=Intern)
  }
  Sys.sleep(1)

  # Check convergence
  Converged = FALSE
  if("ss3.par" %in% list.files(File)){
    # Move PAR files
    file.rename(from=paste(File,"ss3.par",sep=""),
                to=paste(File,"ss3_",Iteration,"-first.par",sep=""))
    # Read and check
    PAR = scan(paste(File,"ss3_",Iteration,"-first.par",sep=""),
        what="character", quiet=TRUE)
    if( ifelse(is.na(as.numeric(PAR[11])),FALSE,as.numeric(PAR[16])<1) ){
      Converged = TRUE
    }else{
      write(paste("*** Optimization ",1," didn't converge ***",sep=""),
            file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
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
      if(PreviousIteration==0){
          PAR_0 = scan(paste(File,"ss3_",PreviousIteration,".par", sep=""),
              comment.char="#", quiet=TRUE)
      }
      if(PreviousIteration>=1 & PreviousIteration<Iteration){
          PAR_0 = scan(paste(File,"ss3_",PreviousIteration,"-first.par",sep=""),
              comment.char="#", quiet=TRUE)
      }
      if(PreviousIteration==Iteration & "ss3_init.par"%in%list.files(File)){
          PAR_0 = scan(paste(File,"ss3_init.par",sep=""), comment.char="#", quiet=TRUE)
      }
      # Modify values of PAR file for short-line values
      for(ParI in 1:length(SD_Group_Vec)){
        if(length(Temp)==7){
          if( "PAR_0" %in% ls() ){
            PAR_0[PAR_num_Vec[ParI]] = SD_Group_Vec[ParI]
          }
        }
      }
      if( "PAR_0" %in% ls() ){
          write(PAR_0, file=paste(File,"ss3.par",sep=""), ncolumns=10)
      }
      # Run SS
      command <- "ss3 -nohess -cbs 500000000 -gbs 500000000"
      if(OS!="Windows"){
        command <- paste("./",command,sep="")
      }
      if(OS=="Windows" & !systemcmd){
        shell(cmd=command,intern=Intern)
      }else{
        system(command,intern=Intern)
      }
      Sys.sleep(1)
      # Check convergence
      if("ss3.par" %in% list.files(File)){
        # Move PAR files
        file.copy(from=paste(File,"ss3.par",sep=""),
                  to=paste(File,"ss3_",Iteration,"-first.par",sep=""), overwrite=TRUE)
        file.remove(paste(File,"ss3.par",sep=""))
        # Read and check
        PAR = scan(paste(File,"ss3_",Iteration,"-first.par",sep=""),
            what="character", quiet=TRUE)
        if( ifelse(is.na(as.numeric(PAR[11])),FALSE,as.numeric(PAR[16])<1) ){
          Converged = TRUE
          write(paste("*** Optimization ",2,"-",PreviousIteration," did converge ***",
                      sep=""),file=paste(File,"Optimization_record.txt",sep=""),
                append=TRUE)
        }else{
          write(paste("*** Optimization ",2,"-",PreviousIteration," didn't converge ***",sep=""),
                file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
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
      SS_writestarter(STARTER, dir=File, file="starter.ss", overwrite=TRUE,
                      verbose=FALSE)
      file.copy(from=paste(File,"ss3_",Iteration,"-first.par",sep=""),
                to=paste(File,"ss3.par",sep=""), overwrite=TRUE)
      file.remove(paste(File,"ss3.std",sep=""))
      command <- "ss3 -maxfn 0 -cbs 500000000 -gbs 500000000"
      if(OS!="Windows"){
        command <- paste("./",command,sep="")
      }
      if(OS=="Windows" & !systemcmd){
        shell(cmd=command,intern=Intern)
      }else{
        system(command,intern=Intern)
      }
      Sys.sleep(1)

    # Estimate new bias ramp
    SsOutput = try(SS_output(File, covar=TRUE, forecast=FALSE), silent=TRUE)
    if( "ss3.std" %in% list.files(File) & file.info(paste(File,"ss3.std",sep=""))$size>0
       & ReDoBiasRamp==TRUE & class(SsOutput)!='try-error' ){
      BiasRamp = SS_fitbiasramp(SsOutput, altmethod="psoptim", print=FALSE, plot=FALSE)
      file.remove(paste(File,"ss3.std",sep=""))
      # Put into CTL
      CTL = readLines(paste(File,STARTER$ctlfile,sep=""))
        CTL[BiasRamp_linenum_Vec] = apply(BiasRamp$df, MARGIN=1, FUN=paste, collapse=" ")
      writeLines(CTL,paste(File,STARTER$ctlfile,sep=""))
      # Re-run to get Hessian
      command <- "ss3 -cbs 500000000 -gbs 500000000"
      if(OS!="Windows"){
        command <- paste("./",command,sep="")
      }
      if(OS=="Windows" & !systemcmd){
        shell(cmd=command,intern=Intern)
      }else{
        system(command,intern=Intern)
      }
      Sys.sleep(1)
    }
  }

  # Check for STD
  Converged = FALSE
  if( "ss3.std"%in%list.files(File) & file.info(paste(File,"ss3.std",sep=""))$size>0 ) Converged=TRUE

  # If STD exists, then approximate marginal likelihood
  if(Converged==TRUE){
    # Save objects for replicating analysis
    file.rename(from=paste(File,"ss3.par",sep=""),
                to=paste(File,"ss3_",Iteration,".par",sep=""))
    file.rename(from=paste(File,"ss3.std",sep=""),
                to=paste(File,"ss3_",Iteration,".std",sep=""))
    file.rename(from=paste(File,"ss3.cor",sep=""),
                to=paste(File,"ss3_",Iteration,".cor",sep=""))
    file.rename(from=paste(File,"admodel.hes",sep=""),
                to=paste(File,"admodel_",Iteration,".hes",sep=""))
    file.rename(from=paste(File,"Report.sso",sep=""),
                to=paste(File,"Report_",Iteration,".sso",sep=""))
    file.copy(from=paste(File,STARTER$datfile,sep=""),
              to=paste(File,STARTER$datfile,"_",Iteration,".dat",sep=""))
    file.copy(from=paste(File,STARTER$ctlfile,sep=""),
              to=paste(File,STARTER$ctlfile,"_",Iteration,".ctl",sep=""))

    # Read in some stuff
    STD = scan(paste(File,"ss3_",Iteration,".std",sep=""), what="character", quiet=TRUE)
    STD = data.frame(matrix(STD[-c(1:(which(STD=="1")[1]-1))], ncol=4, byrow=TRUE),
        stringsAsFactors=FALSE)
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
      write("RECORD FOR PARAMETERS IN INTEGRAL",
            file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
      for(IntI in 1:length(Int_Group_List)){
        Temp = unlist(ESTPAR_num_List[Int_Group_List[[IntI]]])
        write(paste("Group",IntI),file=paste(File,"Optimization_record.txt",sep=""),
              append=TRUE)
        write.table(STD[Temp,],file=paste(File,"Optimization_record.txt",sep=""),
                    append=TRUE, col.names=FALSE, row.names=FALSE)
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
    # Add in constant of proportionality for recruitment (i.e. to account for
    # Rick's bias-correction ramp)
    BiasAdj = readLines(paste(File,"Report_",Iteration,".sso",sep=""))
    BiasAdjStart = pmatch("SPAWN_RECRUIT",BiasAdj) + 7
    BiasAdjTable = read.table(paste(File,"Report_",Iteration,".sso",sep=""),
        header=TRUE, nrows=2, skip=BiasAdjStart, comment.char="#")
    SigmaR = as.numeric(strsplit(BiasAdj[BiasAdjStart-4]," ")[[1]][1])
    # Deal with eras
    RecDevPen = matrix(NA,nrow=3,ncol=2,dimnames=list(c("Early","Main","Forecast"),
                                            c("negative-Rick","full")))
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
    write(c("","sum(RecDevPen) = ",sum(RecDevPen)),
          file=paste(File,"ss3_",Iteration,".pen",sep=""), append=TRUE)

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
          LnDet[IntI] = determinant(Hess[Int_num_List[[IntI]],Int_num_List[[IntI]]],
                   logarithm=TRUE)$modulus[[1]]
        }
        #Version 6 -- use subset of covariance calculated from COR file
        if(Version==6){
          Cov <- DIAG$cov
          LnDet[IntI] = -1 * determinant(Cov[Int_num_List[[IntI]],Int_num_List[[IntI]]],
                                         logarithm=TRUE)$modulus[[1]]
        }
      }
    }

    # Calculate combined objective function
    Ln_Integral = log(2*pi) + (-1/2)*sum(LnDet) + -1*NLL
    #Integral = 2*pi * sqrt(1/exp(LnDet)) * exp(-NLL)

    # Write record to file (part 2)
    write(paste("LnLike",-NLL),file=paste(File,"Optimization_record.txt",sep=""),
          append=TRUE)
    write(paste("NegLnDet",paste(-LnDet,collapse=" ")),
          file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)
  }else{
    # Indicate that this model didn't converge
    if("ss3.par" %in% list.files(File)) file.remove(paste(File,"ss3.par",sep=""))
    Ln_Integral = -1e10 * sum( SD_Group_Vec )
  }
  write(paste("Ln_Integral",Ln_Integral),
        file=paste(File,"Optimization_record.txt",sep=""),append=TRUE)

  return(-1*Ln_Integral)
}
