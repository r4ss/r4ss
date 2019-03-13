#' A function to create a table of biology for assessment reporting:
#' length, weight, \% mature, fecundity, and selectivity
#'
#' Takes the object created by SS_output to create table for reporting
#' for West Coast groundfish.  Works with Stock Synthesis versions 3.30.12 
#' and later.
#'
#' @param dir Locates the directory of the files to be read in, double
#' @param replist List created by \code{SS_output}
#' backslashes (or forwardslashes) and quotes necessary.
#' @param printfolder The sub-directory under 'dir' (see below) in which the
#' PNG files will be located.  The default sub-directory is "plots".
#' The directory will be created if it doesn\'t exist.
#' If 'printfolder' is set to "", it is ignored and the PNG files will be located
#' in the directory specified by 'dir'.
#' @param dir The directory in which a PDF file (if requested) will be created
#' and within which the printfolder sub-directory (see above) will be created
#' if png=TRUE. By default it will be the same directory that the report file
#' was read from by the \code{SS_output} function. Alternatives to the default
#' can be either relative (to the working directory) or absolute paths.
#' The function will attempt to create the directory it doesn't exist, but it
#' does not do so recursively.
#' @param fleetnames Either the string "default", or a vector of characters
#' strings to use for each fleet name. Default="default".
#' @param selexyr The year to summarize selectivity, the default is the final 
#' model yr strings to use for each fleet name. Default="default".
#' 
#' 
#' @return A csv files containing biology and selectivity tables
#' @author Chantel Wetzel
#' @export
#'
SSbiologytables <- function (replist = NULL, printfolder="tables", dir="default", fleetnames = "default", selexyr = "default")
{
	print.numeric  <- function(x, digits) { formatC(x, digits = digits, format = "f") }

	inputs      <- replist$inputs
	biology     <- replist$endgrowth # biology at length final model year
  nsexes      <- replist$nsexes  
  nfleets     <- replist$nfleets
  lbinspop    <- replist$lbinspop
  nlbinspop   <- replist$nlbinspop
  sizeselex   <- replist$sizeselex
  ageselex    <- replist$ageselex
  accuage     <- replist$accuage # max age
  FleetNames  <- replist$FleetNames

  ### deal with directories in which to create PNG or PDF files
  if(dir=="default"){
    # directory within which printfolder will be created
    # by default it is assumed to be the location of the model files
    dir <- inputs$dir }

  # figure out path to where PNG files will go
  plotdir <- file.path(dir,printfolder)
  plotdir.isdir <- file.info(plotdir)$isdir
  if(is.na(plotdir.isdir) | !plotdir.isdir){
    dir.create(plotdir) }

  # set fleet-specific names, and plotting parameters
  if(fleetnames[1]=="default"){
  	fleetnames <- FleetNames }

  # determine the year to summarize
  if(selexyr[1]=="default"){
  	selexyr <- replist$endyr }

  # Table
  # Age: Ave Len - Ave Wgt - % mature (by sex)
  # "Mat*Fecund" is = biology$Fecundity %*% alk (mat = 1, fecundity = fecundity_l * ALK)
  bio = data.frame(Age          = biology[biology$Sex == 1, "Age_Beg"], 
  		 		         Ave_Length_f = print(biology[biology$Sex == 1, "Len_Beg"]   ,digits = 1),
  		 		         Ave_Wght_f   = print(biology[biology$Sex == 1, "Wt_Beg"]    ,digits = 2),
  		 		         Mature_f     = print(biology[biology$Sex == 1, "Len_Mat"]   ,digits = 2),
  		 		         Fecund_f     = print(biology[biology$Sex == 1, "Mat*Fecund"],digits = 2))
    
  if (nsexes == 2){
    bio = data.frame(bio, 
    		 		         Ave_Length_m = print(biology[biology$Sex == 2, "Len_Beg"],digits = 1),
    		 		         Ave_Wght_m   = print(biology[biology$Sex == 2, "Wt_Beg"], digits = 2),
    		 		         Mature_m     = print(biology[biology$Sex == 2, "Len_Mat"],digits = 2))	}
  
  write.csv(bio, paste0(plotdir, "/biology_by_age.csv"), row.names = F)

  # Selectivity by length and age
  selex.age = selex.age.ret = data.frame(Age = 0:accuage)
  for(j in 1:nsexes){
  	for (i in 1:nfleets){
  		ind = ageselex[!is.na(ageselex$Fleet), "Fleet"] == i
  		find = which(ageselex[ind, "Sex"] == j & ageselex[ind, "Yr"] == selexyr & ageselex[ind,"Factor"] == "Asel")
  		selex.age = data.frame(selex.age, print(as.numeric(ageselex[find,8:dim(ageselex)[2]]), digits = 2))	
  	}
  }
  colnames(selex.age) = c("Age", paste0(FleetNames, "_f"), paste0(FleetNames, "_m"))
  write.csv(selex.age, paste0(plotdir, "/selectivity_by_age.csv"), row.names = F)

  # Selectivity by length and age
  retnames = NULL
  selex.size = selex.size.ret = data.frame(Length = as.numeric(names(sizeselex[6:dim(sizeselex)[2]])) )
  for(j in 1:nsexes){
  	for (i in 1:nfleets){
  		find = which(sizeselex$Fleet == i & sizeselex$Sex == j & sizeselex$Yr == selexyr & sizeselex$Factor == "Lsel")
  		selex.size = data.frame(selex.size, print(as.numeric(sizeselex[find, 6:dim(sizeselex)[2]]), digits = 2))

  		find = which(sizeselex$Fleet == i & sizeselex$Sex == j & sizeselex$Yr == selexyr & sizeselex$Factor == "Keep")
      if (length(find) != 0){
        if(j == 1) { retnames = c(retnames, FleetNames[i]) }
        selex.size.ret = data.frame(selex.size.ret, print(as.numeric(sizeselex[find, 6:dim(sizeselex)[2]]), digits = 2)) }  		
  	}
  }
  colnames(selex.size) =  c("Length", paste0(FleetNames, "_f"), paste0(FleetNames, "_m"))
  colnames(selex.size.ret) =  c("Length", paste0(retnames, "_f"), paste0(retnames, "_m"))
  write.csv(selex.size, paste0(plotdir, "/selectivity_by_size.csv"), row.names = F)
  write.csv(selex.size.ret, paste0(plotdir, "/retention_by_size.csv"), row.names = F)

}	

