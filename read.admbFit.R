
read.admbFit<-function(file){
  ret<-list()
  # read par file
  parfile<-as.numeric(scan(paste(file,'.par', sep=''),
    what='', n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar<-as.integer(parfile[1])
  ret$nloglike<-parfile[2] #objective function value
  ret$maxgrad<-parfile[3]

  # read cor file
  file<-paste(file,'.cor', sep='')
  lin<-readLines(file)
  # total parameter including sdreport variables
  ret$totPar<-length(lin)-2
  #log of the determinant of the hessian
  ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2])
  sublin<-lapply(strsplit(lin[1:ret$totPar+2], ' '),function(x)x[x!=''])
  ret$names<-unlist(lapply(sublin,function(x)x[2]))
  ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
  ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))
  ret$cor<-matrix(NA, ret$totPar, ret$totPar)
  corvec<-unlist(sapply(1:length(sublin), function(i)sublin[[i]][5:(4+i)]))
  ret$cor[upper.tri(ret$cor, diag=TRUE)]<-as.numeric(corvec)
  ret$cor[lower.tri(ret$cor)] <- t(ret$cor)[lower.tri(ret$cor)]
  # covariance matrix
  ret$cov<-ret$cor*(ret$std %o% ret$std)
  return(ret)
}
