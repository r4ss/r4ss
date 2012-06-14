SSplotRecdist <-
  function(replist,plot=TRUE,print=FALSE,
           areanames=NULL,
           seasnames=NULL,
           xlab="",
           ylab="",
           main="Distribution of recruitment by area and season",
           plotdir="default",
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           verbose=TRUE)
{
  # plot of recruitment distribution between seasons and areas
  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  if(plotdir=="default") plotdir <- replist$inputs$dir

  nareas <- replist$nareas
  nseasons <- replist$nseasons
  recdist <- replist$recruitment_dist

  areavec <- 1:nareas
  seasvec <- 1:nseasons
  if(is.null(areanames)) areanames <- paste("Area",1:nareas,sep="")
  if(is.null(seasnames)) seasnames <- paste("Season",1:nseasons,sep="")

  recmat <- matrix(0,nrow=nareas,ncol=nseasons)
  
  for(iarea in areavec){
    for(iseas in seasvec){
      recmat[iarea,iseas] <- sum(recdist$Value[recdist$Area==iarea & recdist$Seas==iseas & recdist$Used==1])
    }
  }

  recdistfun <- function(){
    image(areavec,seasvec,recmat,axes=F,xlab=xlab,ylab=ylab,
          main=main,cex.main=cex.main)
    axis(1,at=areavec,labels=areanames)
    axis(2,at=seasvec,labels=seasnames)
    box()

    for(iarea in areavec){
      for(iseas in seasvec){
        text(iarea,iseas,paste(round(100*recmat[iarea,iseas],1),"%",sep=""))
      }
    }
  }
  
  rownames(recmat) <- areanames
  colnames(recmat) <- seasnames
  cat("recruitment distribution by area and season:\n")
  print(recmat)
  
  if(plot) recdistfun()
  if(print){
    file <- paste(plotdir,"recruitment_distribution.png",sep="")
    caption <- "Recruitment distribution by area and season"
    plotinfo <- pngfun(file=file, caption=caption)
    recdistfun()
    dev.off()
  }

  if(!is.null(plotinfo)) plotinfo$category <- "Recruitment"
  return(invisible(plotinfo))
}
