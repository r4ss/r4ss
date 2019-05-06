#library(shiny)

#' A Shiny app that displays various selectivity curves given parameters that would be input into SS

#' Currently implemented only for 
#' \enumerate{
#'   \item logisitic (type 1)
#'   \item double normal (type 24)
#' }
#' This could possibly be hosted on a Shiny server instead of within r4ss
#'
#' @export
#' @author Allan Hicks, Andrea Havron, Ian Taylor,
#' @author inspired by tcl/tk code written by Tommy Garrison

selShapes <- function() {
	if(!require("shiny")) {
		cat("Please install and load the 'shiny' package using the following commands:\n")
		cat("install.packages('shiny')\n")
		cat("library(shiny)\n")
	}
	cat("Press ESC in the R console to exit\n")
	flush.console()

	d <- system.file("Shiny", package = "r4ss")
	shiny::runApp(file.path(d,"Selectivities"))
}

