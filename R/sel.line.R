#' a function for drawing selectivity curves
#' 
#' This function is primarily intended for use by the selfit function.
#' 
#' 
#' @param x vector of x values (age or length)
#' @param model selectivity model "Double_Normal" or "Double_Logistic"
#' @param sp vector of parameters
#' @param min.dist minimum value for selectivity
#' @param max.dist maximum value for selectivity
#' @author Tommy Garrison
#' @export
#' @seealso \code{\link{selfit}}
#' @examples
#' 
#' \dontrun{
#' plot(0, xlim = c(0, 50), ylim = c(0, 1),
#' xlab = 'Length', ylab = 'Selectivity', type = 'n',
#' xaxs = 'i', yaxs = 'i') 
#' sel.line(model = 'Double_Normal', min.dist = 10, max.dist = 50,
#' sp = c(25, -0.5, 3, 3, -5, 0))
#' }
#' 
sel.line <-
    function(x, model, sp, min.dist, max.dist)
{

    ################################################################################
    #
    # sel.line        March 31, 2009.
    # This function comes with no warranty or guarantee of accuracy
    #
    # Purpose: Plot selectivity function for Stock Synthesis.
    # Written: Tommy Garrison, UW
    # Returns: plot of double normal or double logistic selectivity
    # General: parameterization matched Stock Synthesis v.3
    # Notes:   For documentation go to: http://code.google.com/p/r4ss/wiki/Documentation
    # Required packages: none
    #
    ################################################################################

    if(model == "Double_Logistic") {
        sel <- function(x) {
            t1 <- min.dist+(1/(1.+exp(-sp[3])))*(sp[1]-min.dist)
            t1min <- 1/(1+exp(-exp(sp[4])*(min.dist-t1)))*0.9999
            t1max <- 1/(1.+exp(-exp(sp[4])*(sp[1]-t1)))*1.0001
            t1power <- log(0.5)/log((0.5-t1min)/(t1max-t1min))
            t2 <- (sp[1]+sp[8])+(1/(1+exp(-sp[6])))*(max.dist-(sp[1]+sp[8]))
            t2min <- 1/(1+exp(-exp(sp[7])*(sp[1]+sp[8]-t2)))*0.9999
            t2max <- 1/(1+exp(-exp(sp[7])*(max.dist-t2)))*1.0001
            t2power <- log(0.5)/log((0.5-t2min)/(t2max-t2min))
            final <- 1/(1+exp(-sp[5]))
            join1 <- 1/(1+exp(10.*(x-sp[1])))
            join2 <- 1/(1+exp(10.*(x-(sp[1]+sp[8]))))
            join3 <- 1/(1+exp(10.*(x-max.dist)))
            upselex <- sp[2] + (1 - sp[2]) * (( 1/(1+exp(-exp(sp[4])*(x-t1)))-t1min ) / (t1max-t1min))^t1power
            downselex <- (1 + (final - 1) * abs(((( 1/(1+exp(-exp(sp[7])*(x-t2))) -t2min ) / (t2max-t2min) )))^t2power)
            sel  <- ((((upselex*join1)+1.0*(1.0-join1))*join2) + downselex*(1-join2))*join3 + final*(1-join3)
            return(sel)
        }}

    if(model == "Double_Normal") {
        sel <- function(x) {
            sel <- rep(NA, length(x))
            startbin <- 1

            peak <- sp[1]
            upselex <- exp(sp[3])
            downselex <- exp(sp[4])
            final <- sp[6]

            if(sp[5] < -1000) {
                j1 <-  -1001 - round(sp[5])
                sel[1:j1] <- 1.0e-06
            }
            if(sp[5] >= -1000) {
                j1 <- startbin - 1
                if(sp[5] > -999) {
                    point1 <- 1.0/(1.0+exp(-sp[5]))
                    t1min <- exp(-(x[startbin]-peak)^2 / upselex)
                }
            }
            if(sp[6] < -1000) j2 <- -1000- round(sp[6])
            if(sp[6] >= -1000) j2 <- length(x)
            peak2 <- peak + 2 + (0.99*x[j2]- peak - 2)/(1.+exp(-sp[2]))
            if(sp[6] > -999) {
                point2 <- 1.0/(1.0 + exp(-final))
                t2min <- exp(-(x[j2]-peak2)^2 / downselex)
            }
            t1 <- x - peak
            t2 <- x - peak2
            join1 <- 1.0/(1.0 + exp(-(20./(1.0 + abs(t1)))*t1))
            join2 <- 1.0/(1.0 + exp(-(20./(1.0 + abs(t2)))*t2))
            if(sp[5] > -999) asc <- point1 + (1.0-point1) * (exp(-t1^2 / upselex)-t1min)/(1.0-t1min)
            if(sp[5] <= -999) asc <- exp(-t1^2 / upselex)
            if(sp[6] > -999) dsc <- 1.0 + (point2 - 1.0) * (exp(-t2^2 / downselex)-1.0) / (t2min-1.0)
            if(sp[6] <= -999) dsc <- exp(-(t2)^2/downselex)
            sel[(j1+1):j2] <- asc*(1.0-join1)+join1*(1.0-join2+dsc*join2)

            if(startbin > 1 && sp[5] >= -1000) {
                sel[1:startbin] <- (x[1:startbin] / x[startbin])^2 * sel[startbin]
            }

            if(j2 < length(x)) sel[(j2+1):length(x)] <- sel[j2]
            return(sel)
        }}

    if(model == "Double_Normal") col <- "blue"
    if(model == "Double_Logistic") col <- "red"

    curve(sel, add=TRUE, from=min.dist, to=max.dist, type='l', lwd=1, col=col)

} # end sel.line function

