#library(shiny)

#' A Shiny app that displays various selectivity curves given parameters that would be input into SS

#' Currently implemented only for 
#' \enumerate{
#'   \item logisitic (type 1)
#'   \item double normal (type 24)
#' }
#' This could possibly be hosted on a Shiny server instead of within r4ss
#' 
#' @author Allan Hicks, Andrea Havron, Ian Taylor,
#' @author inspired by tcl/tk code written by Tommy Garrison

selShapes <- function() {
	d <- system.file("Shiny", package = "r4ss")
	runApp(file.path(d,"Selectivities"))
}

