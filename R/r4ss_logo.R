#' Make a simple logo for r4ss organization on GitHub
#' 
#' I was tired of the automatically generated symbol
#' that appeared by default.
#' 
#' @author Ian Taylor
#' 

r4ss_logo <- function(){
  png('r4ss_logo.png',res=300,units='in',width=3,height=3)
  par(mar=rep(0,4))
  plot(0, 0, type='n', axes=FALSE, xlab="", ylab="")
  for(i in 4:1){
    phi <- pi + pi/4 - i*pi/2
    r <- 0.5
    text(r*cos(phi), 1.2*r*sin(phi), substring('R4SS',i,i),
         font=4, cex=12, col=rich.colors.short(5,alpha=1)[i+1])
  }
  dev.off()
}
