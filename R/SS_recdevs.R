#' Insert a vector of recruitment deviations into the control file.
#' 
#' A function to insert a vector of recruitment deviations into the control
#' file for simulation studies. This can also be achieved by using the .par
#' file, but Ian Taylor prefers this approach for no good reason.
#' 
#' 
#' @param fyr First year of the recdev vector.
#' @param lyr Last year of the recdev vector.
#' @param ctl Either NULL to read anew or an already read control file.
#' Default=NULL.
#' @param recdevs Either NULL to generate anew or an already generated vector
#' of recdevs. Default=NULL.
#' @param rescale Should the recdevs be rescaled to have mean = 0 and std.
#' deviation = sigmaR? Default=TRUE.
#' @param scaleyrs Vector of years over which rescaling (if chosen) should
#' occur.
#' @param dir Directory where files are located. Default is to use the working
#' directory in use by R. Default="working_directory".
#' @param ctlfile Name of control file to modify.  Default="control.ss_new".
#' @param newctlfile Name of new file to output modified control file.
#' Default="control_modified.ss".
#' @param verbose Verbose output to R command line? Default=TRUE.
#' @param writectl Write new file? Default=TRUE.
#' @param returnctl Return contents ctl file as an object in the R workspace.
#' Default=FALSE.
#' @param newmaxbias Replace the maximum bias adjustment fraction with any
#' non-NULL value. Default=NULL.
#' @author Ian Taylor
#' @export
#' @keywords data manip
SS_recdevs <-
function(
         fyr, lyr, ctl=NULL, recdevs=NULL,
         rescale=TRUE,scaleyrs=NULL,
         dir="working_directory",
         ctlfile="control.ss_new",
         newctlfile="control_modified.ss",
         verbose=TRUE, writectl=TRUE, returnctl=FALSE,
         newmaxbias=NULL
         )
{

  ################################################################################
  #
  # SS_recdevs March 31, 2010.
  # This function comes with no warranty or guarantee of accuracy
  #
  # Purpose: Add newly generated stochastic recruitment deviation inputs to the Control file for SSv3
  # Written: Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
  # Returns: writes a new control file and/or returns a character vector of all lines of the control file
  # Notes:   See users guide for documentation: http://code.google.com/p/r4ss/wiki/
  # Required packages: none
  #
  ################################################################################

  # notes on inputs:
  # fyr              first year
  # lyr              last year
  # ctl              input file name?
  # recdevs          input vector of devs
  # rescale          rescale to zero center and have standard error = sigmaR?

  current_wd <- getwd()
  if(dir!="working_directory") setwd(dir)

  # define a general function for reading values from control file
  readfun <- function(string, maxlen=Inf)
  {
    line1 <- grep(string,ctl)
    if(length(line1)<1) stop("no line contains the phrase, '",string,"'",sep="")
    if(length(line1)>1) stop("more than one line contains the phrase, '",string,"'",sep="")

    splitline <- strsplit(ctl[line1], "#")[[1]]
    vecstrings <- strsplit(splitline[1]," +")[[1]]
    vec <- as.numeric(vecstrings[vecstrings!=""])
    if(length(vec) > maxlen)
      stop(paste("this line has more than ",maxlen," value",c("s","")[1+(maxlen==1)],": ",ctl[line1],sep=""))
    return(vec)
  } # end readfun

  # read control file if ctl is not supplied
  if(is.null(ctl)) ctl = readLines(ctlfile)

  # get sigma R
  sigmaR <- readfun("SR_sigmaR")[3]

  # make sure model includes recdevs and get some information
  do_recdev <- readfun("do_recdev", maxlen=1)
  if(do_recdev==0) stop("do_recdev should be set to 1 or 2")
  yrs <- fyr:lyr
  Nrecdevs <- lyr-fyr+1
  phase <- readfun("recdev phase", maxlen=1)
  advanced <- readfun("advanced options", maxlen=1)
  if(advanced!=1) stop("advanced options must be turned on in control file")
  if(phase>0){
    newphase <- -abs(phase)
    if(verbose) print(paste("making recdev phase to negative:",newphase),quote=FALSE)
    ctl[grep("recdev phase",ctl)] <- paste(newphase,"#_recdev phase")
  }

  # turn on read_recdevs
  key1 <- grep("read_recdevs",ctl)
  ctl[key1] <- paste(Nrecdevs,"#_read_recdevs")

  # check for keyword at start of following section
  key2 <- grep("Fishing Mortality info",ctl)
  if(length(key2)==0){
    print("The phrase 'Fishing Mortality info' does not occur after the recdev section.",quote=FALSE)
    print("Format of control file may be messy.",quote=FALSE)
  }else{
    key2==key2[1]
  }

  # generate new recdevs
  if(!is.null(recdevs)){
    if(length(recdevs)!=Nrecdevs){
      stop(paste("input 'recdevs' has length=",length(recdevs)," but Nrecdevs=lyr-fyr+1=",Nrecdevs,sep=""))
    }else{
      newdevs <- recdevs
    }
  }else{
    newdevs <- rnorm(n=Nrecdevs)
  }
  if(rescale){
    if(is.null(scaleyrs)){
      scaleyrs <- yrs %in% yrs
    }else{
      scaleyrs <- yrs %in% scaleyrs
    }
    if(verbose){
      print(paste("rescaling recdevs vector so yrs ",min(yrs[scaleyrs]),":",max(yrs[scaleyrs]),sep=""),quote=FALSE)
      print(paste("have mean 0 and std. dev. = sigmaR = ",sigmaR,sep=""),quote=FALSE)
    }
    newdevs <- sigmaR*(newdevs-mean(newdevs[scaleyrs]))/sd(newdevs[scaleyrs])
  }
  # build new recdev section
  newsection <- c(
    "#_end of advanced SR options",
    "#",
    "# read specified recr devs",
    "#_Yr Input_value"
  )
  #newsection <-c(newsection, rep("",(key2-key1-1)-length(newsection))) # preserve length of file

  for(i in 1:Nrecdevs) newsection[4+i] <-
    paste((fyr:lyr)[i], c("  "," ")[1+(newdevs[i]<0)], newdevs[i], " #_stochastic_recdev_with_sigmaR=", sigmaR, sep="")

  ctl <- c(ctl[1:key1],newsection,ctl[key2:length(ctl)])
  #ctl[(key1+1):(key2-1)] <- newsection

  # if maxbias is input, then replace
  if(!is.null(newmaxbias)) ctl[grep("max_bias",ctl)] <- paste(newmaxbias,"#_max_bias_adj_in_MPD")

  # write and/or return the modified control file
  if(writectl){
    writeLines(ctl,newctlfile)
    if(verbose) print(paste("wrote new file:",newctlfile),quote=FALSE)
  }
  #reset working directory
  setwd(current_wd)
  if(returnctl) return(ctl)
} # end function

