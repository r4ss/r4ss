TSCplot <- function(SSout,
                    ylimBar="default",
                    ylimDepl=c(0,1.025),
                    colBar= "yellow",
                    cexBarLabels=1.1,
                    cex.axis=1.1,
                    space=0.0,
                    pchDepl=19,
                    colDepl="red",
                    lwdDepl=3,
                    pchSpace = 5,
                    ht=5,wd=7,
                    labelLines=2.8,
                    makePDF=NULL,
                    makePNG=NULL)  {
                    
    ### Plots the barchart of catches and depletion trajctory for the TSC report

    if(!is.null(makePDF) & !is.null(makePNG)) stop("Cannot specify both makePDF and makePNG. Choose only one.\n")

    indVirgin <- which(SSout$timeseries$Era=="VIRG")
    ind <- which(SSout$timeseries$Era=="TIME")
    ind <- c(ind,max(ind)+1)
    tmp <- SSout$timeseries[ind,]
    deadCatch <- tmp[,grep("dead\\(B\\)",names(tmp))]
    SP <- data.frame(Yr=tmp$Yr, SpawnBio=tmp$SpawnBio, Dead_Catch=apply(deadCatch,1,sum))
    if(ylimBar=="default") {
        ylimBar <- c(0,max(SP$Dead_Catch)*1.05)
    }
    ind <- seq(1,nrow(SP),pchSpace)

    if(is.null(makePDF)) { windows(height=ht,width=wd) }
    if(!is.null(makePDF)) { pdf(file=makePDF,width=wd,height=ht) }
    if(!is.null(makePNG)) { png(file=makePNG,width=wd,height=ht,units = "in", pointsize = 10, res=300) }
    par(mar=c(4,5,2,5))
    barplot(SP$Dead_Catch[-length(SP$Yr)],  names.arg = SP$Yr[-length(SP$Yr)], ylim=ylimBar, ylab="", col='yellow', cex=cexBarLabels, cex.axis=cex.axis, space=space)

    par(new=T)
    plot(SP$Yr, SP$SpawnBio/SP$SpawnBio[1], yaxt='n', yaxs='i', xaxt = 'n', ylab="", xlab="",
           ylim=ylimDepl, type='l', lwd=lwdDepl,  cex.axis=cex.axis)
    points(SP$Yr[ind], SP$SpawnBio[ind]/SSout$timeseries$SpawnBio[indVirgin], pch=pchDepl, col=colDepl)
    axis(4, at=seq(0, 1, 0.1), cex.axis=cex.axis)
    mtext(c("Year","Total mortality catch (mt)", "Depletion"), side=c(1,2,4), line=labelLines, cex=1.5)

    if(!is.null(makePDF)) {
        dev.off()
        cat("The plot is in pdf file",makePDF,"\n")
    }
    if(!is.null(makePNG)) {
        dev.off()
        cat("The plot is in png file",makePNG,"\n")
    }

    invisible(SP)
}
