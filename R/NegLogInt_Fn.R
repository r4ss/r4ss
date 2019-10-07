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
##' ".cor" output file.
##' @param PAR_num_Vec Vector giving the number in the ".par" vector for each
##' random effect coefficient.
##' @param Int_Group_List List where each element is a vector, providing a way of
##' grouping different random effect groups into a single category. Although
##' this input is still required, it is no has the former input Version has been
##' hardwired to Version = 1.
##' @param StartFromPar Logical flag (TRUE or FALSE) saying whether to start each
##' round of optimization from a ".par" file (I recommend TRUE)
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
##' @param systemcmd Should R call SS using "system" function instead of "shell".
##' This may be required when running R in Emacs on Windows. Default = FALSE.
##' @param exe SS executable name (excluding extension), either "ss" or "ss3".
##' This string is used for both calling the executable and also finding the
##' output files like ss.par. For 3.30, it should always be "ss" since the
##' output file names are hardwired in the TPL code.
##' @seealso \code{\link{read.admbFit}}, \code{\link{getADMBHessian}}
##' @author James Thorson
##' @export
##' @references Thorson, J.T., Hicks, A.C., and Methot, R.D. 2014. Random
##' effect estimation of time-varying factors in Stock Synthesis. ICES J. Mar.
##' Sci.
#' @examples
#'
#'   \dontrun{
#'     direc <- "C:/Models/LaplaceApprox/base"  #need the full path because wd is changed in function
#'     if("Optimization_record.txt" %in% list.files(direc)) {
#'          file.remove(file.path(direc,"Optimization_record.txt"))
#'     }
#'     Opt <- optimize(f=NegLogInt_Fn,
#'                    interval=c(0.001, 0.12),
#'                    maximum=FALSE,
#'                    File=direc,
#'                    Input_SD_Group_Vec=1,
#'                    CTL_linenum_List=list(127:131),
#'                    ESTPAR_num_List=list(86:205),
#'                    Int_Group_List=1,
#'                    PAR_num_Vec=NA,
#'                    Intern=TRUE)
#'   }

NegLogInt_Fn <- function(File=NA, Input_SD_Group_Vec,
                         CTL_linenum_List, ESTPAR_num_List,
                         PAR_num_Vec, Int_Group_List=list(1),
                         StartFromPar=TRUE, Intern=TRUE,
                         ReDoBiasRamp=FALSE, BiasRamp_linenum_Vec=NULL,
                         CTL_linenum_Type=NULL,systemcmd=FALSE,
                         exe="ss"){
  # test exe input
  if(!(exe=="ss" | exe=="ss3")){
    # turns out 3.30 != "3.30" in R
    warning('exe inputs other than "ss" and "ss3" may not work')
  }
  # frequently used files
  parfile <- paste0(exe, ".par")
  stdfile <- paste0(exe, ".std")
  corfile <- paste0(exe, ".cor")
  
  # figure out operating system
  OS <- "Mac" # don't know the version$os info for Mac
  if(length(grep("linux",version$os)) > 0) OS <- "Linux"
  if(length(grep("mingw",version$os)) > 0) OS <- "Windows"

  # Directory
  if(is.na(File)){
    File <- getwd()
  }

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

  # Iteration tracker (previously set as a global variable)
  Iteration <- Iteration + 1
  #  writing Iteration to a file to avoid CRAN rules about global variables
  write(Iteration,file=file.path(File,"Iteration.txt"))
  #  alternative method would be to read it out of Optimization_record.txt using code
  #  like the following:
  #    record <- readLines(OptRecord)
  #    Iterations <- as.numeric(substring(record[grep("Iteration",record)],11))
  #    Iteration <- max(Iterations)

  # Transform parameter vector
  SD_Group_Vec <- Input_SD_Group_Vec

  # Modify inputs when necessary
  if(is.null(CTL_linenum_Type)){
    CTL_linenum_Type <- rep(NA, length(SD_Group_Vec))
  }

  # define some filenames with full path
  OptRecord <- file.path(File, "Optimization_record.txt")
  ParFile   <- file.path(File, parfile)
  
  # Write record to file (part 1)
  if(!("Optimization_record.txt" %in% list.files(File))){
    write("Start optimization",file=OptRecord,
          append=FALSE)
  }
  write("",file=OptRecord,append=TRUE)
  write(date(),file=OptRecord,append=TRUE)
  write(paste("Iteration",Iteration),file=OptRecord,
        append=TRUE)
  write(paste("SD_Group_Vec",paste(SD_Group_Vec,collapse=" ")),
        file=OptRecord,append=TRUE)

  # If .par is availabile from the last iteration then use it as starting point
  STARTER <- SS_readstarter(file.path(File,"starter.ss"), verbose=FALSE)
  if( StartFromPar==TRUE &&
     paste0(exe, "_", Iteration-1, ".par") %in% list.files(File) ){
    STARTER$init_values_src <- 1
    PAR_0 <- scan(file.path(File, paste0(exe, "_",Iteration-1,".par")),
                  comment.char="#", quiet=TRUE)
  }else{
    STARTER$init_values_src <- 0
  }
  SS_writestarter(STARTER, dir=File, file="starter.ss", overwrite=TRUE, verbose=FALSE)

  # Read CTL
  CTL <- readLines(file.path(File,STARTER$ctlfile))
  # Modify CTL
  for(ParI in 1:length(SD_Group_Vec)){
    for(CtlLineI in 1:length(CTL_linenum_List[[ParI]])){
      Temp <- as.vector(unlist(sapply(CTL[CTL_linenum_List[[ParI]][CtlLineI]],
                                      FUN=function(Char){strsplit(Char," ")[[1]]})))
      #Temp <- sapply(CTL[CTL_linenum_List[[ParI]][CtlLineI]]," ", FUN=strsplit)[[1]]
      Temp <- as.vector(unlist(sapply(Temp, FUN=function(Char){strsplit(Char,"\t")[[1]]})))
      Temp <- Temp[which(Temp!="")]
      if(length(grep("#",Temp))>=1){
        Temp <- Temp[-(grep("#",Temp):length(Temp))]
      }
      Temp <- as.numeric(Temp)
      #print(Temp)
      #print(CTL_linenum_Type[ParI])
      if(is.na(CTL_linenum_Type[ParI])){
        if(length(Temp)==7){
          CTL_linenum_Type[ParI] <- "Short_Param"
        }
        if(length(Temp)==14){
          CTL_linenum_Type[ParI] <- "Long_Penalty"
        }
      }
      if(CTL_linenum_Type[ParI] %in% c("Short_Param","Long_Param")){
        Temp[3] <- SD_Group_Vec[ParI]
        Temp[7] <- -1 * abs(Temp[7])
        # Modify values of PAR file for short-line values
        if( "PAR_0" %in% ls() ){
          PAR_0[PAR_num_Vec[ParI]] <- SD_Group_Vec[ParI]
        }
        #if(ParI==2){
        #  assign("Temp", value=Temp, envir=.GlobalEnv)
        #  stop()
        #}
      }
      if(CTL_linenum_Type[ParI]=="Long_Penalty"){
        Temp[12] <- SD_Group_Vec[ParI]
      }
      CTL[CTL_linenum_List[[ParI]][CtlLineI]] <- paste(Temp, collapse=" ")
      #if(ParI==2){
      #assign("Temp", value=Temp, envir=.GlobalEnv)
      #stop()
      #}
    }
  }
  #assign("CTL", value=CTL, envir=.GlobalEnv)
  #stop()
  # Write CTL
  writeLines(CTL, file.path(File,STARTER$ctlfile))
  if( "PAR_0" %in% ls() ){
    write(PAR_0, file=ParFile, ncolumns=10)
  }

  # Run SS
  setwd(File)
  command <- paste0(exe, " -nohess -cbs 500000000 -gbs 500000000")
  if(OS!="Windows"){
    command <- paste0("./", command)
  }
  if(OS=="Windows" & !systemcmd){
    shell(cmd=command,intern=Intern)
  }else{
    system(command,intern=Intern)
  }
  Sys.sleep(1)

  # Check convergence
  Converged <- FALSE
  if(parfile %in% list.files(File)){
    # Move PAR files
    file.rename(from=ParFile,
                to=file.path(File,paste0(exe, "_",Iteration,"-first.par")))
    # Read and check
    PAR <- scan(file.path(File,paste0(exe, "_",Iteration,"-first.par")),
                what="character", quiet=TRUE)
    if( ifelse(is.na(as.numeric(PAR[11])),FALSE,as.numeric(PAR[16])<1) ){
      Converged <- TRUE
    }else{
      write(paste0("*** Optimization ",1," didn't converge ***"),
            file=OptRecord,append=TRUE)
    }
  }

  # Try re-running with default starting values
  if(Converged==FALSE){
    # Change starter to take PAR file
    STARTER <- SS_readstarter(file.path(File,"starter.ss"), verbose=FALSE)
    STARTER$init_values_src <- 1
    SS_writestarter(STARTER, dir=File, file="starter.ss", overwrite=TRUE, verbose=FALSE)
    # Loop through all previous start values
    PreviousIteration <- 0
    while(Converged==FALSE & PreviousIteration<=Iteration){
      # Read in original estimate
      if(PreviousIteration==0){
        PAR_0 <- scan(file.path(File, paste0(exe, "_",PreviousIteration,".par")),
                      comment.char="#", quiet=TRUE)
      }
      if(PreviousIteration>=1 & PreviousIteration<Iteration){
        PAR_0 <- scan(file.path(File, paste0(exe, "_",PreviousIteration,"-first.par")),
                      comment.char="#", quiet=TRUE)
      }
      if(PreviousIteration==Iteration & paste0(exe, "_init.par") %in% list.files(File)){
        PAR_0 <- scan(file.path(File, paste0(exe, "_init.par")), comment.char="#", quiet=TRUE)
      }
      # Modify values of PAR file for short-line values
      for(ParI in 1:length(SD_Group_Vec)){
        if(length(Temp)==7){
          if( "PAR_0" %in% ls() ){
            PAR_0[PAR_num_Vec[ParI]] <- SD_Group_Vec[ParI]
          }
        }
      }
      if( "PAR_0" %in% ls() ){
        write(PAR_0, file=ParFile, ncolumns=10)
      }
      # Run SS
      command <- paste0(exe, " -nohess -cbs 500000000 -gbs 500000000")
      if(OS!="Windows"){
        command <- paste0("./", command)
      }
      if(OS=="Windows" & !systemcmd){
        shell(cmd=command,intern=Intern)
      }else{
        system(command,intern=Intern)
      }
      Sys.sleep(1)
      # Check convergence
      if(parfile %in% list.files(File)){
        # Move PAR files
        file.copy(from=ParFile,
                  to=file.path(File,paste0(exe, "_",Iteration,"-first.par")),
                  overwrite=TRUE)
        file.remove(ParFile)
        # Read and check
        PAR <- scan(file.path(File, paste0(exe, "_",Iteration,"-first.par")),
                    what="character", quiet=TRUE)
        if( ifelse(is.na(as.numeric(PAR[11])),FALSE,as.numeric(PAR[16])<1) ){
          Converged <- TRUE
          write(paste0("*** Optimization ",2,"-", PreviousIteration,
                       " did converge ***"),
                file=OptRecord, append=TRUE)
        }else{
          write(paste0("*** Optimization ",2,"-",PreviousIteration,
                       " didn't converge ***"),
                file=OptRecord, append=TRUE)
        }
      }
      # Increment
      PreviousIteration <- PreviousIteration + 1
    }
  }

  # Only calculate Integral if model is converged
  if(Converged==TRUE){
    # Re-run to get Hessian
    STARTER <- SS_readstarter(file.path(File,"starter.ss"), verbose=FALSE)
    STARTER$init_values_src <- 1
    SS_writestarter(STARTER, dir=File, file="starter.ss", overwrite=TRUE,
                    verbose=FALSE)
    file.copy(from=file.path(File, paste0(exe, "_",Iteration,"-first.par")),
              to=ParFile, overwrite=TRUE)
    if(file.exists(file.path(File, stdfile))) {
      file.remove(file.path(File, stdfile))
    }
    command <- paste0(exe, " -maxfn 0 -cbs 500000000 -gbs 500000000")
    if(OS!="Windows"){
      command <- paste0("./", command)
    }
    if(OS=="Windows" & !systemcmd){
      shell(cmd=command,intern=Intern)
    }else{
      system(command,intern=Intern)
    }
    Sys.sleep(1)

    # Estimate new bias ramp
    if( ReDoBiasRamp==TRUE
       & stdfile %in% list.files(File)
       & file.info(file.path(File, stdfile))$size>0 ){
      # try reading output
      SsOutput <- try(SS_output(File, covar=TRUE, forecast=FALSE, verbose=F, printstats=F), silent=TRUE)
      if( class(SsOutput)!='try-error' ){
        BiasRamp <- SS_fitbiasramp(SsOutput, altmethod="psoptim", print=FALSE, plot=FALSE)
        file.remove(file.path(File, stdfile))
        # Put into CTL
        CTL <- readLines(file.path(File, STARTER$ctlfile))
        CTL[BiasRamp_linenum_Vec] <- apply(BiasRamp$df, MARGIN=1, FUN=paste, collapse=" ")
        writeLines(CTL, file.path(File, STARTER$ctlfile))
        # Re-run to get Hessian
        command <- "ss3 -cbs 500000000 -gbs 500000000"
        if(OS!="Windows"){
          command <- paste0("./", command)
        }
        if(OS=="Windows" & !systemcmd){
          shell(cmd=command,intern=Intern)
        }else{
          system(command,intern=Intern)
        }
        Sys.sleep(1)
      }
    }
  }

  # Check for STD
  Converged <- FALSE
  if( stdfile%in%list.files(File) & file.info(file.path(File, stdfile))$size>0 ){
    Converged <- TRUE
  }

  # If STD exists, then approximate marginal likelihood
  if(Converged==TRUE){
    # Save objects for replicating analysis
    file.rename(from=ParFile,
                to=file.path(File, paste0(exe, "_",Iteration,".par")))
    file.rename(from=file.path(File, stdfile),
                to=file.path(File, paste0(exe, "_",Iteration,".std")))
    file.rename(from=file.path(File, corfile),
                to=file.path(File, paste0(exe, "_",Iteration,".cor")))
    file.rename(from=file.path(File,"admodel.hes"),
                to=file.path(File, paste0("admodel_",Iteration,".hes")))
    file.rename(from=file.path(File,"Report.sso"),
                to=file.path(File, paste0("Report_",Iteration,".sso")))
    file.copy(from=file.path(File,STARTER$datfile),
              to=file.path(File, paste0(STARTER$datfile,"_",Iteration,".dat")))
    file.copy(from=file.path(File,STARTER$ctlfile),
              to=file.path(File, paste0(STARTER$ctlfile,"_",Iteration,".ctl")))

    # Read in some stuff
    STD <- scan(file.path(File, paste0(exe, "_",Iteration,".std")),
                what="character", quiet=TRUE)
    STD <- data.frame(matrix(STD[-c(1:(which(STD=="1")[1]-1))], ncol=4, byrow=TRUE),
                      stringsAsFactors=FALSE)
    PAR <- scan(file.path(File, paste0(exe, "_",Iteration,".par")),
                comment.char="#", quiet=TRUE)
    DIAG <- read.admbFit(file.path(File, paste0(exe, "_",Iteration)))
    HESS <- getADMBHessian(File=File, FileName=paste0("admodel_",Iteration,".hes"))
    # Calculate Hessian
    cov <- corpcor::pseudoinverse(HESS$hes)
    scale <- HESS$scale
    cov.bounded <- cov*(scale %o% scale)
    #se <- sqrt(diag(cov.bounded))
    #cor <- cov.bounded/(se %o% se)
    Hess <- corpcor::pseudoinverse(cov.bounded)

    # Confirm that correct parameters are being included in Hessian
    if(Iteration==1){
      write("RECORD FOR PARAMETERS IN INTEGRAL",
            file=OptRecord,append=TRUE)
      for(IntI in 1:length(Int_Group_List)){
        Temp <- unlist(ESTPAR_num_List[Int_Group_List[[IntI]]])
        write(paste("Group",IntI),file=OptRecord,
              append=TRUE)
        write.table(STD[Temp,],file=OptRecord,
                    append=TRUE, col.names=FALSE, row.names=FALSE)
      }
    }

    # Calculate NLL (while adding in constant of integration for random-walk coefficients)
    NLL <- DIAG$nloglike
    for(ParI in 1:length(SD_Group_Vec)){
      # Add in constant of integration for "Long_Penalty" parameters
      if(CTL_linenum_Type[ParI]=="Long_Penalty"){
        NLL <- NLL + -1 * (-log(2*pi)/2 - log(SD_Group_Vec[ParI])) * length(ESTPAR_num_List[[ParI]])
      }
    }
    # Add in constant of proportionality for recruitment (i.e. to account for
    # Rick's bias-correction ramp)
    BiasAdj <- readLines(file.path(File, paste0("Report_",Iteration,".sso")))
    # starting with 3.24U, a new output was added 3 lines after SPAWN_RECRUIT
    if(grep("Bmsy/Bzero", BiasAdj[pmatch("SPAWN_RECRUIT", BiasAdj) + 3])==1){
      shift <- 8
    }else{
      shift <- 7
    }
    BiasAdjStart <- pmatch("SPAWN_RECRUIT",BiasAdj) + shift
    BiasAdjTable <- read.table(file.path(File, paste0("Report_",Iteration,".sso")),
                               header=TRUE, nrows=2, skip=BiasAdjStart, comment.char="#")
    SigmaR <- as.numeric(strsplit(BiasAdj[BiasAdjStart-4]," ")[[1]][1])
    # Deal with eras
    RecDevPen <- matrix(NA,nrow=3,ncol=2,dimnames=list(c("Early","Main","Forecast"),
                                             c("negative-Rick","full")))
    # Deal with early era
    RecDevPen['Early','full'] <- -1 * (-log(SigmaR) * BiasAdjTable[2,2])
    RecDevPen['Early','negative-Rick'] <- -log(SigmaR) * BiasAdjTable[2,2]*BiasAdjTable[2,5]
    # Deal with main era
    RecDevPen['Main','full'] <- -1 * (-log(SigmaR) * BiasAdjTable[1,2])
    RecDevPen['Main','negative-Rick'] <- -log(SigmaR) * BiasAdjTable[1,2]*BiasAdjTable[1,5]
    # Deal with forecast era
    RecDevPen['Forecast','full'] <- -1 * (-log(SigmaR) * length(which(STD[,2]=="Fcast_recruitments")))
    RecDevPen['Forecast','negative-Rick'] <- 0
    # Add into NLL and record
    NLL <- NLL + sum(RecDevPen)
    write.table(RecDevPen, file=file.path(File, paste0(exe, "_",Iteration,".pen")))
    write(c("","sum(RecDevPen) = ",sum(RecDevPen)),
          file=file.path(File, paste0(exe, "_",Iteration,".pen")), append=TRUE)

    # Approximate integral using Laplace Approximation
    Int_num_List <- vector("list", length=length(Int_Group_List))
    LnDet <- rep(0, length(Int_Group_List))
    for(IntI in 1:length(Int_Group_List)){
      # Only calculate if necessary
      if( length(unlist(ESTPAR_num_List[Int_Group_List[[IntI]]])) > 0 ){
        # Determine indices for integral
        Int_num_List[[IntI]] <- unlist(ESTPAR_num_List[Int_Group_List[[IntI]]])

        #Version 1 -- use full hessian
        if(IntI==1){
          LnDet[IntI] <- determinant(Hess, logarithm=TRUE)$modulus[[1]]
        }
        if(IntI>=2) LnDet[IntI] <- 0

        #### alternative versions taken out based on recommendation from Jim Thorson
        ## #Version 5 -- use back-transformed hessian, use subset
        ## if(Version==5){
        ##   #Hess2 <- cov * solve(scale %o% scale)
        ##   #Which2 <- 38 + 1:46
        ##   LnDet[IntI] <- determinant(Hess[Int_num_List[[IntI]],Int_num_List[[IntI]]],
        ##            logarithm=TRUE)$modulus[[1]]
        ## }
        ## #Version 6 -- use subset of covariance calculated from COR file
        ## if(Version==6){
        ##   Cov <- DIAG$cov
        ##   LnDet[IntI] <- -1 * determinant(Cov[Int_num_List[[IntI]],Int_num_List[[IntI]]],
        ##                                  logarithm=TRUE)$modulus[[1]]
        ## }
      }
    }

    # Calculate combined objective function
    Ln_Integral <- log(2*pi) + (-1/2)*sum(LnDet) + -1*NLL
    #Integral <- 2*pi * sqrt(1/exp(LnDet)) * exp(-NLL)

    # Write record to file (part 2)
    write(paste("LnLike",-NLL),file=OptRecord,
          append=TRUE)
    write(paste("NegLnDet",paste(-LnDet,collapse=" ")),
          file=OptRecord,append=TRUE)
  }else{
    # Indicate that this model didn't converge
    if("ss3.par" %in% list.files(File)) file.remove(ParFile)
    Ln_Integral <- -1e10 * sum( SD_Group_Vec )
  }
  write(paste("Ln_Integral",Ln_Integral),
        file=OptRecord,append=TRUE)

  return(-1*Ln_Integral)
}
