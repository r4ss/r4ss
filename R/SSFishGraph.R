#' A function for converting Stock Synthesis output to the format used by
#' FishGraph
#' 
#' Only skeleton of a function right now, needs work. Intended as a translator
#' to convert the output from object created by \code{\link{SS_output}} to the
#' format used by FishGraph.
#' 
#' 
#' @param replist Object created by SS_output
#' @param title Title of output
#' @param species Species name
#' @author Ian Taylor
#' @references A website related to FishGraph is
#' \url{http://r-forge.r-project.org/projects/fishgraph/}
#' @keywords data manip
SSFishGraph <-
  function(replist,
           title="SSv3 output",
           species="some kind of fish")
{
  x <- list() # this will be the big output list

  ### create list: info
  x$info <- list()
  x$info$date <- replist$repfiletime
  x$info$title <- title
  x$info$species <- species
  x$info$model <- paste("Stock Synthesis version", strsplit(replist$SS_version,";")[[1]][1])
  x$info$base.run <- "don't know"
  # for all these units, it's probably best to read them from a file
  # (these values are just what was present in the gag example file)
  x$info$units.length <- "mm"
  x$info$units.weight <- "gutted lbs"
  x$info$units.landings.wgt <- "1000 gutted lbs"
  x$info$units.numbers <- "1000s"
  x$info$units.naa <- "10^6 fish"
  x$info$units.biomass <- "mt"
  x$info$units.ssb <- "10^9 eggs"
  x$info$units.rec <- "1000 fish"
  x$info$rec.model <- c("Beverton-Holt with flat-top beyond Bzero",
                        "Ricker",
                        "standard Beverton-Holt",
                        "ignore steepness")[replist$SRRtype]
  x$info$units.length2 <- "inches"
  x$info$units.length3 <- "cm"
  x$info$units.weight2 <- "gutted kg"
  x$info$units.landings <- "1000 gutted lbs"
  x$info$units.discards <- "1000 gutted lbs"

  ### create list: parms
  # note: this will probably require renaming parameters
  parmtable <- replist$parameters
  x$parms <- as.list(parmtable$Value)
  names(x$parms) <- parmtable$Label

  ### create list: like
  x$like <- list()

  ### create list: sel.parms
  x$sel.parms <- list()

  ### create list: N.age
  x$N.age <- list()

  ### create list: B.age
  x$B.age <- list()

  ### create list: Z.age
  x$Z.age <- list()

  ### create list: L.age.pred.num
  x$L.age.pred.num <- list()

  ### create list: L.age.pred.wgt
  x$L.age.pred.wgt <- list()

  ### create list: C.age.pred.num.c.hal
  x$C.age.pred.num.c.hal <- list()

  ### create list: C.age.pred.num.c.dv
  x$C.age.pred.num.c.dv <- list()

  ### create list: C.age.pred.num.hb
  x$C.age.pred.num.hb <- list()

  ### create list: C.age.pred.num.mrfss
  x$C.age.pred.num.mrfss <- list()

  ### create list: prop.m.obs
  x$prop.m.obs <- list()

  ### create list: sel.age
  x$sel.age <- list()

  ### create list: comp.mats
  x$comp.mats <- list()

  ### create list: t.series
  x$t.series <- list()

  ### create list: a.series
  x$a.series <- list()

  ### create list: eq.series
  x$eq.series <- list()

  ### create list: pr.series
  x$pr.series <- list()

  ### create list: CLD.est.mats
  x$CLD.est.mats <- list()

  ### create list: sel.length
  x$sel.length <- list()

  ### create list: F.age
  x$F.age <- list()

  ### create list: M.age
  x$M.age <- list()

  ### create list: M2.age
  x$M2.age <- list()

  ### return the big list
  return(x)
}

