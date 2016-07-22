#' Create HTML files to view figures in browser.
#' 
#' Writes a set of HTML files with tabbed navigation between them.  Depends on
#' \code{\link{SS_plots}} with settings in place to write figures to PNG files.
#' Should open main file in default browser automatically.
#' 
#' 
#' @param replist Object created by \code{\link{SS_output}}
#' @param plotdir Directory where PNG files are located.
#' @param plotInfoTable CSV file with info on PNG files. By default, the
#' \code{plotdir} directory will be searched for files with name beginning
#' 'plotInfoTable*'
#' @param title Title for HTML page.
#' @param width Width of plots (in pixels).
#' @param openfile Automatically open index.html in default browser?
#' @param multimodel Override errors associated with plots from multiple model
#' runs. Only do this if you know what you're doing.
#' @param filenotes Add additional notes to home page.
#' @param verbose Display more info while running this function?
#' @note By default, this function will look in the directory where PNG files
#' were created for CSV files with the name 'plotInfoTable...' written by
#' 'SS_plots. HTML files are written to link to these plots and put in the same
#' directory. Please provide feedback on any bugs, annoyances, or suggestions
#' for improvement.
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_output}}
#' 
SS_html <- function(replist=NULL,
                    plotdir=NULL,
                    plotInfoTable=NULL,
                    title="SS Output",
                    width=500,
                    openfile=TRUE,
                    multimodel=FALSE,
                    filenotes=NULL,
                    verbose=TRUE){
  cat("Running 'SS_html':\n",
      "  By default, this function will look in the directory where PNG files were created\n",
      "  for CSV files with the name 'plotInfoTable...' written by 'SS_plots.'\n",
      "  HTML files are written to link to these plots and put in the same directory.\n\n")
  if(is.null(plotdir)){
    stop("input 'plotdir' required")
  }
  # check for table in directory with PNG files
  if(is.null(plotInfoTable)){
    if(!is.null(replist)){
      filenames <- dir(plotdir)
      # look for all files beginning with the name 'plotInfoTable'
      filenames <- filenames[grep("plotInfoTable",filenames)]
      filenames <- filenames[grep(".csv",filenames)]
      if(length(filenames)==0) stop("No CSV files with name 'plotInfoTable...'")
      plotInfoTable <- NULL
      # loop over matching CSV files and combine them
      for(ifile in 1:length(filenames)){
        filename <- file.path(plotdir,filenames[ifile])
        temp <- read.csv(filename,colClasses = "character")
        plotInfoTable <- rbind(plotInfoTable,temp)
      }
      plotInfoTable$png_time <- as.POSIXlt(plotInfoTable$png_time)
      # look for duplicate models
      runs <- unique(plotInfoTable$StartTime)
      if(length(runs)>1){
        if(multimodel){
          msg <- c("Warning!: CSV files with name 'plotInfoTable...' are from multiple model runs.\n",
                   "    Hopefully you know what you're doing, or change to 'multimodel=FALSE.\n",
                   "    Runs:\n")
          for(irun in 1:length(runs)) msg <- c(msg,paste("    ",runs[irun],"\n"))
          cat(msg)
        }else{
          msg <- c("CSV files with name 'plotInfoTable...' are from multiple model runs.\n",
                   "    Delete old files or (if you really know what you're doing) override with 'multimodel=TRUE.\n",
                   "    Runs:\n")
          for(irun in 1:length(runs)) msg <- c(msg,paste("    ",runs[irun],"\n"))
          stop(msg)
        }
      }
      # look for duplicate file names
      filetable <- table(plotInfoTable$file)
      duplicates <- names(filetable[filetable>1])
      # loop over duplicates and remove rows for older instance
      if(length(duplicates)>0){
        if(verbose) cat("Removing duplicate rows in combined plotInfoTable based on mutliple CSV files\n")
        for(idup in 1:length(duplicates)){
          duprows <- grep(duplicates[idup], plotInfoTable$file, fixed=TRUE)
          duptimes <- plotInfoTable$png_time[duprows]
          # keep duplicates with the most recent time
          dupbad <- duprows[duptimes!=max(duptimes)]
          goodrows <- setdiff(1:nrow(plotInfoTable),dupbad)
          plotInfoTable <- plotInfoTable[goodrows,]
        }
      }
    }else{
      stop("Need input for 'replist' or 'plotInfoTable'")
    }
  }
  if(!is.data.frame(plotInfoTable))
    stop("'plotInfoTable' needs to be a data frame")

  plotInfoTable$basename <- basename(as.character(plotInfoTable$file))
  plotInfoTable$dirname <- dirname(as.character(plotInfoTable$file))
  plotInfoTable$dirname2 <- basename(dirname(as.character(plotInfoTable$file)))
  plotInfoTable$path <- file.path(plotInfoTable$dirname2,plotInfoTable$basename)
  dir <- dirname(plotInfoTable$dirname)[1]

  # write unique HTML file for each category of plots (or whatever)
  categories <- unique(plotInfoTable$category)
  for(icat in 0:length(categories)){
    if(icat==0){
      category <- "Home"
      htmlfile <- file.path(plotdir,"SS_output.html")
      htmlhome <- htmlfile
      if(verbose){
        cat("Home HTML file with output will be:\n",htmlhome,'\n')
      }
    }else{
      category <- categories[icat]
      htmlfile <- file.path(plotdir,paste("SS_output_",category,".html",sep=""))
    }
    # write HTML head including some CSS stuff about fonts and whatnot
    # source for text below is http://unraveled.com/publications/css_tabs/
    cat('<html><head><title>', title, '</title>\n',
        '    <!-- source for text below is http://unraveled.com/publications/css_tabs/ -->\n',
        '    <!-- CSS Tabs is licensed under Creative Commons Attribution 3.0 - http://creativecommons.org/licenses/by/3.0/ -->\n',
        '    \n',
        '    <style type="text/css">\n',
        '    \n',
        '    body {\n',
        '    font: 100% verdana, arial, sans-serif;\n',
        '    background-color: #fff;\n',
        '    margin: 50px;\n',
        '    }\n',
        '    \n',

      #### this stuff allows scrolling while leaving the tabs in place,
      #### but I'd like to not have to set the height
      ## .container{
      ## }
      ## .panel{
      ## height: 1000px;
      ## overflow: auto;
      ## }
        
        '    /* begin css tabs */\n',
        '    \n',
        '    ul#tabnav { /* general settings */\n',
        '    text-align: left; /* set to left, right or center */\n',
        '    margin: 1em 0 1em 0; /* set margins as desired */\n',
        '    font: bold 11px verdana, arial, sans-serif; /* set font as desired */\n',
        '    border-bottom: 1px solid #6c6; /* set border COLOR as desired */\n',
        '    list-style-type: none;\n',
        '    padding: 3px 10px 2px 10px; /* THIRD number must change with respect to padding-top (X) below */\n',
        '    }\n',
        '    \n',
        '    ul#tabnav li { /* do not change */\n',
        '    display: inline;\n',
        '    }\n',
        '    \n',
        '    body#tab1 li.tab1, body#tab2 li.tab2, body#tab3 li.tab3, body#tab4 li.tab4 { /* settings for selected tab */\n',
        '    border-bottom: 1px solid #fff; /* set border color to page background color */\n',
        '    background-color: #fff; /* set background color to match above border color */\n',
        '    }\n',
        '    \n',
        '    body#tab1 li.tab1 a, body#tab2 li.tab2 a, body#tab3 li.tab3 a, body#tab4 li.tab4 a { /* settings for selected tab link */\n',
        '    background-color: #fff; /* set selected tab background color as desired */\n',
        '    color: #000; /* set selected tab link color as desired */\n',
        '    position: relative;\n',
        '    top: 1px;\n',
        '    padding-top: 4px; /* must change with respect to padding (X) above and below */\n',
        '    }\n',
        '    \n',
        '    ul#tabnav li a { /* settings for all tab links */\n',
        '    padding: 2px 4px; /* set padding (tab size) as desired; FIRST number must change with respect to padding-top (X) above */\n',
        '    border: 1px solid #6c6; /* set border COLOR as desired; usually matches border color specified in #tabnav */\n',
        '    background-color: #cfc; /* set unselected tab background color as desired */\n',
        '    color: #666; /* set unselected tab link color as desired */\n',
        '    margin-right: 0px; /* set additional spacing between tabs as desired */\n',
        '    text-decoration: none;\n',
        '    border-bottom: none;\n',
        '    }\n',
        '    \n',
        '    ul#tabnav a:hover { /* settings for hover effect */\n',
        '    background: #fff; /* set desired hover color */\n',
        '    }\n',
        '    \n',
        '    /* end css tabs */\n',
        '    \n',
        '    \n',
        '    h2 {\n',
        '    font-size: 20px;\n',
        '    color: #4c994c;\n',
        '    padding-top: 1px;\n',
        '    font-weight: bold;\n',
        '    border-bottom-width: 1px;\n',
        '    border-bottom-style: solid;\n',
        '    border-bottom-color: #6c6;\n',
        '    padding-bottom: 2px;\n',
        '    padding-left: 0px;\n',
        '    }\n',
        '    </style>',
        '</head>\n',
        sep = "", file=htmlfile, append=FALSE)

    ## # old navigation menu
    ## cat('<!-- Site navigation menu -->\n',
    ##     '  <ul class="navbar">\n',
    ##     file=htmlfile, append=TRUE)
    ## for(icat in categories)
    ##   cat('    <li><a href="#',icat,'">',icat,'</a></li>\n',sep="",
    ##       file=htmlfile, append=TRUE)

    # write navigation menu

  #### more stuff related to scroll options
  ## <div class="main">
  ##   <div class="container">

      
    cat('<!-- Site navigation menu -->\n',
        '  <ul id="tabnav">\n',
        file=htmlfile, append=TRUE)
    for(itab in 0:length(categories)){
      if(itab==0){
        tab <- "Home"
        cat('    <li class="tab1"><a href="SS_output.html">Home</a></li>\n',sep="",
            file=htmlfile, append=TRUE)
      }else{
        tab <- categories[itab]
        cat('    <li class="tab',itab+1,'"><a href="SS_output_',tab,'.html">',tab,'</a></li>\n',sep="",
            file=htmlfile, append=TRUE)
      }
    }
    cat('  </ul>\n', file=htmlfile, append=TRUE)

  #### more stuff related to scroll options
  ## <div class="panel">
    
    # add text on "Home" page
    if(category=="Home"){
      cat('\n\n<h2><a name="',category,'">',category,'</h2>\n',sep="", file=htmlfile, append=TRUE)
      if(is.null(replist)){
        cat('<p>Model info not available (need to supply "replist" input to SS_HTML function)</p>\n',
            sep="", file=htmlfile, append=TRUE)
      }else{
        cat('<p><b>SS version:</b>\n',
            replist$SS_version,'</p>\n\n',
            '<p><b>Starting time of model:</b>\n',
            substring(replist$StartTime,12),'</p>\n\n',
            sep="", file=htmlfile, append=TRUE)
        if(!is.null(filenotes)){
          for(i in 1:length(filenotes)){
            cat('<p><b>Notes:</b>\n',
                paste(filenotes,collapse='</b>\n'),
                '</p>\n\n',
                sep="", file=htmlfile, append=TRUE)
          }
        }
        nwarn <- replist$Nwarnings
        if(is.na(nwarn)){
          cat('<p><b>Warnings (from file warnings.sso):</b> NA</p>\n\n',
              sep="", file=htmlfile, append=TRUE)
        }else{
          if(nwarn==0){
            cat('<p><b>Warnings (from file warnings.sso):</b> None</p>\n\n',
                sep="", file=htmlfile, append=TRUE)
          }          
          if(nwarn > 0){
            if(nwarn <= 20){
              cat('<p><b>Warnings (from file warnings.sso):</b></p>\n\n',
                  '<pre>\n',
                  sep="", file=htmlfile, append=TRUE)
            }else{
              cat('<p><b>Warnings (first 20 from file warnings.sso):</b></p>\n\n',
                  '<pre>\n',
                  sep="", file=htmlfile, append=TRUE)
            }
            for(irow in 3:length(replist$warnings)){
              cat(replist$warnings[irow],'\n',
                  sep="", file=htmlfile, append=TRUE)
            }
            cat('</pre>\n',
                sep="", file=htmlfile, append=TRUE)
          }
        }
      }
    }else{
      plotinfo <- plotInfoTable[plotInfoTable$category==category,]
      
      cat('\n\n<h2><a name="',category,'">',category,'</h2>\n',sep="", file=htmlfile, append=TRUE)
      for(i in 1:nrow(plotinfo)){
        cat("<p align=left><a href='",plotinfo$basename[i],"'><img src='",plotinfo$basename[i],
            "' border=0 width=",width,"></a><br>",plotinfo$caption[i],"<br><i><small>file: <a href='",plotinfo$basename[i],"'>",plotinfo$basename[i],"</a></small></i>\n",
            sep="", file=htmlfile, append=TRUE)
      }
    }
  }

  #### more stuff related to scroll options
  ## </div></div>
  
  cat("\n\n</body>\n</html>", file=htmlfile, append=TRUE)

  # open HTML file automatically:
  # thanks John Wallace for finding the browseURL command
  if(openfile){
    cat("Opening HTML file in your default web-browser.\n")
    # check for presence of file
    # alternative location for file in the path is relative to the working directory
    htmlhome2 <- file.path(getwd(), htmlhome)
    if(is.na(file.info(htmlhome2)$size)){
      browseURL(htmlhome)
    }else{
      browseURL(htmlhome2) 
    }
  }
}
