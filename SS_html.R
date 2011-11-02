SS_html <- function(replist=NULL,
                    plotdir="plots",
                    plotInfoTable=NULL,
                    title="SS Output",
                    width=500,
                    openfile=TRUE,
                    verbose=TRUE){
  # check for table in directory with PNG files
  if(is.null(plotInfoTable)){
    if(!is.null(replist)){
      dir <- replist$inputs$dir
      filename <- paste(dir,plotdir,"plotInfoTable.csv",sep="/")
      fileinfo <- file.info(filename)
      if(is.na(fileinfo$size)){
        stop("File missing:",filename)
      }else{
        plotInfoTable <- read.csv(filename,colClasses = "character")
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
  plotInfoTable$path <- paste(plotInfoTable$dirname2,plotInfoTable$basename,sep="/")
  dir <- dirname(plotInfoTable$dirname)[1]

  # write unique HTML file for each category of plots (or whatever)
  categories <- unique(plotInfoTable$category)
  for(icat in 0:length(categories)){
    if(icat==0){
      category <- "Home"
      htmlfile <- paste(dir,plotdir,"SS_output.html",sep="/")
      htmlhome <- htmlfile
      if(verbose) cat("Home HTML file with output will be:\n",htmlhome,'\n')
    }else{
      category <- categories[icat]
      htmlfile <- paste(dir,"/",plotdir,"/SS_output_",category,".html",sep="")
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
        #'    margin: 0px 20px 5px 20px;\n',
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
            substring(replist$Run_time,12),'</p>\n\n',
            sep="", file=htmlfile, append=TRUE)
        nwarn <- replist$Nwarnings
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
          for(irow in 3+(1:nwarn)){
            cat(replist$warnings[irow],'\n',
                sep="", file=htmlfile, append=TRUE)
          }
          cat('</pre>\n',
              sep="", file=htmlfile, append=TRUE)
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
  if(openfile){
    if(.Platform$OS.type=="windows"){
      shell(cmd=htmlhome)
    }else{
      system(htmlhome)
    }
  }
}
