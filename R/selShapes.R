#library(shiny)
selShapes <- function() {
	d <- system.file("Shiny", package = "r4ss")
	runApp(file.path(d,"Selectivities"))
}

