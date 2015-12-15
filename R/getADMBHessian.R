##' Read admodel.hes file
##'
##' This function reads in all of the information contained in the
##' admodel.hes file. Some of this is needed for relaxing the
##' covariance matrix, and others just need to be recorded and
##' rewritten to file so ADMB "sees" what it's expecting.
##' @param File Directory in which .hes file is located.
##' @param FileName Name of .hes file.
##' @return A list with elements num.pars, hes, hybrid_bounded_flag, and scale. 
##' @author Cole Monnahan
##' @export
##' @seealso \code{\link{read.admbFit}}, \code{\link{NegLogInt_Fn}}
##' @note Also published here:
##' \url{http://www.admb-project.org/examples/admb-tricks/covariance-calculations}
getADMBHessian <- function(File, FileName){
    ## This function reads in all of the information contained in the
    ## admodel.hes file. Some of this is needed for relaxing the
    ## covariance matrix, and others just need to be recorded and
    ## rewritten to file so ADMB "sees" what it's expecting.
    filename <- file(paste(File,FileName,sep=""), "rb")
    on.exit(close(filename))
    num.pars <- readBin(filename, "integer", 1)
    hes.vec <- readBin(filename, "numeric", num.pars^2)
    hes <- matrix(hes.vec, ncol=num.pars, nrow=num.pars)
    hybrid_bounded_flag <- readBin(filename, "integer", 1)
    scale <- readBin(filename, "numeric", num.pars)
    result <- list(num.pars=num.pars, hes=hes,
                   hybrid_bounded_flag=hybrid_bounded_flag, scale=scale)
    return(result)
}
