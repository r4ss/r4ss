# contains small helper functions that can be used across multiple functions in
# the r4ss pkg


#' Get the name of the data .ss_new file in a directory
#'
#' In previous versions of Stock Synthesis,
#' the file new data file was named `data.ss_new`.
#' `_echo` was added to the name when the file was parsed into three separate
#' files.
#' @param dir Relative or absolute path to a directory
#' @return A string with the name of the data .ss_new file. If not found, will
#'  be NA. Both of strings are searched for using `dir(pattern = )` and
#'  if both exist, then `data_echo.ss_new` is returned.
#'
get_dat_new_name <- function(dir) {
  datname <- tail(
    dir(path = dir, pattern = "data_?e?c?h?o?\\.ss_new"),
    1
  )
  ifelse(length(datname) == 0, NA, datname)
}

#' Allow Multi-Plots
#' Set the par() to options suitable for ss3diags multi plots.
#'
#' See \link[graphics]{par} for more details on each parameter.
#' 
#' @param mfrow determines plot frame set up
#' @param plot.cex cex graphic option
#' @param mai graphical par for plot margins
#' @param labs if TRUE margins are narrow 
#' @param omi Outer margins in lines of text.  
#' 
#' @keywords ssplot utils
#' 
#' @export
#' 
sspar <- function(mfrow=c(1,1),
                  plot.cex=1,
                  mai=c(0.55,0.6,0.1,.1),
                  omi = c(0.,0.,0.,0)+ 0.1,
                  labs=TRUE){
  if(labs==F){
    mai=c(0.25,0.25,0.15,.15)
    omi = c(0.3,0.35,0.2,0.2)}
  par(list(mfrow=mfrow,mai = mai, mgp =c(2.,0.5,0),omi =omi, tck = -0.02,cex=plot.cex))
}

#' Convert Time-Steps
#'
#' Function to convert non-annual into annual time-steps for retros and cpue residuals
#'  
#' @param ss3out outputs from r4ss::SS_output() or r4ss::SSsummarize()
#' @param time.steps  time steps behind yrs e.g. 0.25 for quarterly 
#' @param end.time last time step e.g. 2018.75 with a cpue observation
#' 
#' @return Reformatted Rep file outputs
#' 
#' @keywords utils rep retro retrocomps
#' 
#' @export
#' 
SSdiagsTime2Year = function(ss3out,time.steps=0.25,end.time){
  if(is.null(ss3out$len)==F | is.null(ss3out$len)==F){
  type = "retrocomps"} else {  
  type = ifelse(is.null(ss3out$modelnames),"rep","retro")
  }
  # Conversion match function
  convTY <- function(indices,end.time,time.steps){
    steps = (unique(indices$Time))
    nsteps = length(steps)
    Time = rev(rev(seq(0,end.time,time.steps))[1:nsteps])
    Yr = floor(Time)  
    Seas = (Time-Yr)*4+1
    conv = match(indices$Time,steps)
    indices$Yr = Yr[conv]
    indices$Time = Time[conv]
    indices$Seas = Seas[conv]
    return(indices)
  }
  
  if(type == "rep"){ 
    ss3out$cpue =  convTY(ss3out$cpue,end.time,time.steps)
    #if(!is.null(ss3out$lendbase)) ss3out$lendbase =  convTY(ss3out$lendbase,end.time,time.steps)
    # length comps not working
  }
  if(type == "retro"){ 
    ss3out$indices =  convTY(ss3out$indices,end.time,time.steps)
    # SSB
    ssb = ss3out$SpawnBio
    steps = unique(ssb$Yr)  
    nsteps = length(steps)
    Time = (rev(rev(seq(0,end.time,time.steps))[1:nsteps]))
    ssb$Time = Time
    subset = ssb$Time%in%floor(ssb$Time)
    ssb = ssb[subset,] 
    ss3out$SpawnBio =  ssb
    ss3out$SpawnBioLower = ss3out$SpawnBioLower[subset,]
    ss3out$SpawnBioUpper = ss3out$SpawnBioUpper[subset,]
    
    ss3out$SpawnBio$Yr = ssb$Time
    ss3out$SpawnBioLower$Yr = ssb$Time
    ss3out$SpawnBioUpper$Yr = ssb$Time 
    ss3out$startyrs = rep(min(ssb$Time),ss3out$n)
    ss3out$endyrs = rep(max(ssb$Time),ss3out$n)
    # Can add F and Rec if needed
  }
  if(type=="retrocomps"){
    if(!is.null(ss3out$len)) ss3out$len =  convTY(ss3out$len,end.time,time.steps)
    if(!is.null(ss3out$age)) ss3out$len =  convTY(ss3out$age,end.time,time.steps)
    ss3out$startyrs = rep(min(ss3out$len$Time,ss3out$age$Time),ss3out$n)
    ss3out$endyrs = rep(max(ss3out$len$Time,ss3out$age$Time),ss3out$n)
  }
  return(ss3out)
}

#' Add legend to plots 
#'
#' ss3diags function to add legend to plots
#'
#' @param legendlabels Optional vector of labels to include in legend.
#' @param cumulative default for legend location set to topleft
#'
#' @export
#'
#'
add_legend <- function(legendlabels, cumulative = FALSE) {
  if(cumulative){
    legendloc="topleft"
  }
  if(is.numeric(legendloc)) {
    Usr <- par()$usr
    legendloc <- list(x = Usr[1] + legendloc[1] * (Usr[2] - Usr[1]),
                      y = Usr[3] + legendloc[2] * (Usr[4] - Usr[3]))
  }
  
  # if type input is "l" then turn off points on top of lines in legend
  legend.pch <- -1
  if(type=="l"){
    legend.pch <- rep(NA,length(pch))
  }
  legend(legendloc, legend=legendlabels[legendorder],
         col=col[legendorder], lty=lty[legendorder],seg.len = 2,
         lwd=lwd[legendorder], pch=legend.pch[legendorder], bty="n", ncol=legendncol,pt.cex=0.7,cex=legendcex,y.intersp = legendsp)
}

#' Make a vector of colors.
#'
#' A subset of rich.colors by Arni Magnusson from the gplots package, with the
#' addition of alpha transparency (which is now available in the gplots version
#' as well)
#'
#'
#' @param n Number of colors to generate.
#' @param alpha Alpha transparency value for all colors in vector. Value is
#' passed to rgb function.
#' @author Arni Magnusson, Ian Taylor
#' @export
rich.colors.short <- function(n, alpha = 1) {
  x <- seq(0, 1, length = n)
  r <- 1 / (1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15) / max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1], v[2], v[3], alpha = alpha))
}

#' extract line number from Report.sso by matching keyword
#'
#' @param string keyword
#' @param obj matrix object in which to search (always rawrep so far)
#' @param substr1 controls whether to compare subsets or the whole line
#'
#' @return line number from Report.sso (or other file)
#' @export

match_report_line <- function(string, obj = rawrep[, 1], substr1 = TRUE) {
      # return a line number from the report file (or other file)
      # substr1 controls whether to compare subsets or the whole line
      match(string, if (substr1) {
        substring(obj, 1, nchar(string))
      } else {
        obj
      })
    }


#' extract a table from Report.sso by matching a keyword
#'
#' return a subset of values from the report file (or other file)
#' subset is defined by character strings at the start and end, with integer
#' adjustments of the number of lines to above/below the two strings
#'
#'
#' @param string1 keyword near top of table
#' @param adjust1 integer for number of rows after string1 to start table
#' @param string2 keyword near bottom of table
#' (or NULL to use blank line to end table)
#' @param adjust2 integer for number of rows after string2 to end table
#' (often a negative value)
#' @param which_blank which blank line (after string1) to use as the end
#' of the table (if using string2 = NULL)
#' @param cols which columns to return, can be an integer, a vector, "all",
#' or 'nonblank' (where this last returns all columns with at least one
#' non-blank values in it)
#' @param matchcol1 which column to search for string1
#' @param matchcol2 which column to search for string2
#' @param obj matrix object in which to search (always rawrep so far)
#' @param blank_lines vector of line numbers of obj which are blank
#' (to save the time of replicating this in each function call)
#' @param substr1 allow string1 to be a substring of the text in matchcol1?
#' (It must be start at the beginning regardless)
#' @param substr2 allow string2 to be a substring of the text in matchcol2?
#' (It must be start at the beginning regardless)
#' @param header Is the first row of the table a header?
#' @param apply type.convert() function to the resulting table?
#'
#' @export
match_report_table <- function(string1,
                      adjust1,
                      string2 = NULL,
                      adjust2 = -1,
                      which_blank = 1,
                      cols = "nonblank",
                      matchcol1 = 1,
                      matchcol2 = 1,
                      obj = rawrep,
                      blank_lines = rep_blank_or_hash_lines,
                      substr1 = TRUE,
                      substr2 = TRUE,
                      header = FALSE,
                      type.convert = FALSE) {

  line1 <- match(
    string1,
    if (substr1) {
      substring(obj[, matchcol1], 1, nchar(string1))
    } else {
      obj[, matchcol1]
    }
  )
  if (is.null(string2)) {
    # get first blank or "#" line after the start
    line2 <- blank_lines[blank_lines > line1][which_blank]
    # if no remaining blank lines, use the end of the file
    if (is.na(line2)) {
      line2 <- nrow(obj)
    }
  } else {
    line2 <- match(
      string2,
      if (substr2) {
        substring(obj[, matchcol2], 1, nchar(string2))
      } else {
        obj[, matchcol2]
      }
    )
  }
  if (is.na(line1) | is.na(line2)) {
    return(NULL)
  }

  if (is.numeric(cols)) {
    out <- obj[(line1 + adjust1):(line2 + adjust2), cols]
  }
  if (cols[1] == "all") {
    out <- obj[(line1 + adjust1):(line2 + adjust2), ]
  }
  if (cols[1] == "nonblank") {
    # returns only columns that contain at least one non-empty value
    out <- obj[(line1 + adjust1):(line2 + adjust2), ]
    out <- out[, apply(out, 2, emptytest) < 1]
  }
  if (header && nrow(out) > 0) {
    out[1, out[1, ] == ""] <- "NoName"
    names(out) <- out[1, ]
    out <- out[-1, ]
  }
  if (type.convert) {
    out <- type.convert(out, as.is = TRUE)
  }
  return(out)
} # end matchfun2

#' Open png device and return info on the file being created
#'
#' this was previously contained within each of the SSplotXXX() functions.
#' It (1) translates the not-quite-matching specifications for the image to the
#' values needed by png(), then (2) returns the plotinfo data.frame
#' (which exists within each function which calls this) after adding a row
#' with the filename and caption for each new plot
#' Note: this just opens the png device which needs to be closed via dev.off()
#' outside this function.
#'
#' @param plotinfo table of information about all plots
#' @param file filename to write to (including .png extension)
#' @param plotdir directory where plots will be written
#' @template pwidth
#' @template pheight
#' @template punits
#' @template res
#' @template ptsize
#' @param caption caption for the image
#' @param alt_text alternative text for screen readers
#' (if left as NA then will be set based on the caption)
#' @author Ian G. Taylor

save_png <- function(plotinfo,
                     file,
                     plotdir,
                     pwidth,
                     pheight,
                     punits,
                     res,
                     ptsize,
                     caption = NA,
                     alt_text = NA) {

  # replace any slashes (as in 'Eggs/kg_inter_Fem')
  file <- gsub(pattern = "/", replacement = "_per_", x = file, fixed = TRUE)

  # open png device
  png(
    filename = file.path(plotdir, file),
    width = pwidth,
    height = pheight,
    units = punits,
    res = res,
    pointsize = ptsize
  )

  # change graphics parameters to input value
  if (!is.null(par)) {
    par(par)
  }

  # assemble and return info
  invisible(rbind(plotinfo, data.frame(
    file = file,
    caption = caption,
    alt_text = alt_text
  )))
}

