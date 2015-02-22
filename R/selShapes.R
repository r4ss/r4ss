#library(shiny)
selShapes <- function() {
	d <- system.file("Shiny", package = "ss3sim")
	runApp(file.path(d,"Selectivities"))
}

