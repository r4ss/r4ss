barfun <- function(x, y, x.pos="left", plot=TRUE, ...){
  #make barplot-like shape which is really a polygon
  if(any(sort(x)!=x)){
    stop("x must be a vector of strictly increasing values")
  }
  if(length(x)!=length(y) | any(!is.numeric(x), !is.numeric(y))){
    stop("x and y must be numeric vectors of the same length")
  }
  n <- length(x)

  if(x.pos=="left"){
    # x-values represent left-hand sides of each bin
    # make final value as the last in the sequence plus diff from previous
    x.vec <- c(x, tail(x,1) + diff(tail(x,2)))
  }
  if(x.pos=="right"){
    # x-values represent right-hand sides of each bin
    # make final value as the last in the sequence plus diff from previous
    x.vec <- c(x[1], x[1] - diff(x[1:2]))
  }
  if(x.pos=="center"){
    # x-values represent right-hand sides of each bin
    # make final value as the last in the sequence plus diff from previous
    diff <- diff(head(x,2))
    x.vec <- c(x[1] - diff/2, x + diff/2)
  }
  x.double <- sort(c(x.vec, x.vec))
  y.double <- c(0, y[sort(rep(1:n, 2))], 0)
  if(plot){
    polygon(x.double, y.double, ...)
  }
  return(data.frame(x=x.double, y=y.double))
}
