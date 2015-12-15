#' Plot uncertainty around chosen selectivity ogive from MCMC.
#' 
#' Plot uncertainty in selectivity from an MCMC output for whichever fleet/year
#' was chosen in the optional extra "more stddev reporting"
#' 
#' 
#' @param post A data frame containing either derived_posteriors.sso or a good
#' subset of it. This can be an element of the list created by the the
#' \code{\link{SSgetMCMC}} function.
#' @param add TRUE/FALSE option to add results to an existing plot.
#' @param nsexes Number of sexes in the model (should match model values but is
#' only used in the title).
#' @param shift Optional adjustment to the x values to avoid overlap of
#' intervals when overplotting on an existing plot.
#' @param fleetname Optional input to make the title better. Default will be
#' something like "Fleet 1", using the numbering from the model.
#' @param col Color for points and lines.
#' @author Ian Taylor
#' @export
#' @keywords aplot hplot
SSplotMCMC_ExtraSelex <- function(post, add=FALSE, nsexes=1,shift=0,
                                  fleetname="default",col="blue"){
  # post is a data.frame containing either derived_posteriors.sso or a good subset of it
  #      it can be an element of the list created by the the SSgetMCMC function
  # add will add to existing plot
  # nsexes should match model values but is only used in the title
  # shift will shift the x values by for overplotting on an existing plot
  # fleetname will make title better
  # col will color points and lines
  
  cols <- grep("Selex_std",names(post))
  if(length(cols)==0){
    stop("no columns in posteriors include text 'Selex_std'")
  }else{
    sel <- post[,cols]
    names <- names(post)[cols]
    splitnames <- strsplit(names,"_")
    namesDF <- as.data.frame(matrix(unlist(strsplit(names,"_")),ncol=6,byrow=T))
    i       <- as.numeric(as.character(namesDF$V3))[1]
    m       <- as.character(namesDF$V4)[1]
    agelen  <- as.character(namesDF$V5)[1]
    bin     <- sort(unique(as.numeric(as.character(namesDF$V6))))+shift
    quants <- apply(sel,2,quantile, probs=c(0.025,0.5,0.975))
    
    xlab <- "Age (years)"
    if(fleetname=="default") fleetname <- paste("Fleet",i)

    if(m=="Fem" & nsexes==1) sextitle3 <- ""
    if(m=="Fem" & nsexes==2) sextitle3 <- "females"
    if(m=="Mal") sextitle3 <- "males"
    main <- paste("Uncertainty in selectivity for",fleetname,sextitle3)
    no0 <- as.numeric(quants[3,]-quants[1,])!=0
    upper=quants[3,]
    lower=quants[1,]
    if(!add) matplot(bin,t(quants),lty=c(3,1,3),lwd=c(1,3,1),xlab=xlab,ylim=c(0,1),main=main,
                     ylab="Selectivity",type="n",xlim=c(0,max(bin)))
    
    lines(bin,quants[2,],lty=1,col=col,lwd=1,type="o")
    arrows(x0=bin[no0], y0=quants[1,no0], x1=bin[no0], y1=quants[3,no0],
           length=0.01, angle=90, code=3, col=col)
    abline(h=0,col="grey")
  }
}
