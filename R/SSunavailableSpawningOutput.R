#' Plot unavailable spawning output
#'
#' Calculate and plot the unavailable spawning output- separating out ones that
#' are unavailable because they're too small to be selected from ones that are
#' too big to be selected
#'
#' @param replist  List created by \code{\link{SS_output}}
#' @param plot Plot to active plot device?
#' @param print Print to PNG files?
#' @param plotdir Directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param pwidth Width of plot
#' @param pheight Height of plot
#' @param punits Units for PNG file
#' @param res Resolution for PNG file
#' @param ptsize Point size for PNG file
#' @param cex.main Character expansion for plot titles
#' @author Megan Stachura, Andrew Cooper, Andi Stephens, Neil Klaer, Ian G. Taylor
#' @export

SSunavailableSpawingOutput <-
  function(replist,
           plot=TRUE,print=FALSE,
           plotdir="default",
           pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,cex.main=1)
{

  # Function for plotting the pnf
  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file,caption=caption))
    return(plotinfo)
  }

  plotinfo <- NULL
  ageselex <- replist$ageselex
  accuage <- replist$accuage

  if(plotdir=="default") plotdir <- replist$inputs$dir

  # Check to make surea ll the catch units are the same
  catch.units.same <- all(replist$catch_units[1] ==
                            replist$catch_units[1:replist$nfishfleets])
  if(!catch.units.same){
    warning('Catch units for all fleets are not equal. Calculated weighted
             mean selectivity for calculating unavailable spawning
             output may not be accurate.')
  }

  # Run the code for each area
  for(area in 1:replist$nareas){

    ##########################################################################
    # step 1: calculate catch by fleet by year

    timeseries <- replist$timeseries

    # get the fishing fleets that fish in this area
    fishing.fleets.this.area <- replist$fleet_ID[which(replist$fleet_area==area)]
    fleet.catch.names <- paste("obs_cat:_",fishing.fleets.this.area,sep="")
    fleet.catch.names <- fleet.catch.names[which(fleet.catch.names %in% names(timeseries))]

    catch.by.fleet <- as.matrix(timeseries[which(timeseries$Era=='TIME' & timeseries$Area==area), fleet.catch.names])
    row.names(catch.by.fleet) <- timeseries$Yr[which(timeseries$Era=='TIME' & timeseries$Area==area)]

    # Cut the catch data to only include years with non-zero total catch
    catch.by.fleet <- as.matrix(catch.by.fleet[which(rowSums(catch.by.fleet)>0), ])
    years.with.catch <- as.numeric(row.names(catch.by.fleet))

    ##########################################################################
    # Step 2: Female numbers at age matrix by year
    num.at.age <- replist$natage
    names(num.at.age)[10] <- "BegMid"
    # old line which used "subset"
    ## num.at.age.female <- subset(num.at.age, Gender==1 & Era=="TIME" & BegMid=="B" & Area==area & Yr %in% years.with.catch)
    # replacement line without "subset"
    num.at.age.female <- num.at.age[num.at.age$Gender==1 & num.at.age$Era=="TIME" &
                                      num.at.age$BegMid=="B" & num.at.age$Area==area &
                                        num.at.age$Yr %in% years.with.catch,]
    years <- num.at.age.female$Yr
    first.col <- which(names(num.at.age.female)=='0')
    num.at.age.female <- num.at.age.female[,first.col:ncol(num.at.age.female)]
    row.names(num.at.age.female) <- years

    ##########################################################################
    # step 3: create an average derived age-based selectivity across fleets
    #         and years using a weighted average catch by fleet by year
  #   age.selectivity <- ageselex
  #   age.selectivity.female <- subset(ageselex, gender==1 &
  #                                      factor=='Asel2' &
  #                                      year==max(ageselex$year))

    mean.selectivity <- matrix(ncol=ncol(num.at.age.female),
                               nrow=nrow(num.at.age.female), NA)

    for(y in 1:nrow(catch.by.fleet)){

      # Get the female selectivity at age in this year for all fleets
      year.get <- as.numeric(row.names(catch.by.fleet))[y]
      # old line which used "subset"
      ## age.selectivity.female.year <- subset(ageselex, gender==1 &
      ##                                         factor=='Asel2' &
      ##                                           year==year.get &
      ##                                             fleet %in% fishing.fleets.this.area)
      # replacement line without "subset"
      age.selectivity.female.year <-
        ageselex[ageselex$gender==1 &
                   ageselex$factor=='Asel2' &
                     ageselex$year==year.get &
                       ageselex$fleet %in% fishing.fleets.this.area,]

      # Weight the selectivity at length by fleet for this year based on
      # the propotion of catch that came from each fleet in this year
      for(a in 8:ncol(age.selectivity.female.year)){
        mean.selectivity[y,a-7] <- sum(catch.by.fleet[y, 1:ncol(catch.by.fleet)] *
                                         age.selectivity.female.year[,a] /
                                        sum(catch.by.fleet[y, 1:ncol(catch.by.fleet)]))
      }
    }

    ##########################################################################
    # step 4: multiply the numbers at age matrix by the average selectivity
    #         curves to derive exploitable numbers at age

    exploitable.females.at.age <- num.at.age.female*mean.selectivity

    ##########################################################################
    # step 5: generate spawning output estimates from exploitable numbers at age

    # old line which used "subset"
    ## biology.at.age.female <- subset(replist$endgrowth, Gender==1)
    # replacement line without "subset"
    biology.at.age.female <- replist$endgrowth[replist$endgrowth$Gender==1,]

    exploitable.spawning.output.by.age <- matrix(nrow=nrow(exploitable.females.at.age),
                                                 ncol=ncol(exploitable.females.at.age),
                                                 NA)
    for (y in 1:nrow(exploitable.females.at.age)){
      for(a in 1:ncol(exploitable.females.at.age)){
        exploitable.spawning.output.by.age[y,a] <- biology.at.age.female$'Mat*Fecund'[a] *
                                                    exploitable.females.at.age[y,a]
      }
    }

    exploitable.spawning.output <- rowSums(exploitable.spawning.output.by.age)

    ##########################################################################
    # step 6: subtract the results of step 5 from the spawning output from
    #         the assessment to determine unavailable spawning output

    total.spawning.output.by.age <- matrix(nrow=nrow(num.at.age.female),
                                           ncol=ncol(num.at.age.female), NA)
    for (y in 1:nrow(num.at.age.female)){
      for(a in 1:ncol(num.at.age.female)){
        total.spawning.output.by.age[y,a] <- biology.at.age.female$'Mat*Fecund'[a] *
                                              num.at.age.female[y,a]
      }
    }

    total.spawning.output <- rowSums(total.spawning.output.by.age)

    # Calculate the cryptic spwning output, by age and total across ages
    cryptic.spawning.output <- total.spawning.output -
                               exploitable.spawning.output
    cryptic.spawning.output.by.age <- total.spawning.output.by.age -
                                      exploitable.spawning.output.by.age

    # If due to rounding error any of the cryptic spawning output values
    # are less than zero, change them to zero
    cryptic.spawning.output[cryptic.spawning.output < 0] <- 0
    cryptic.spawning.output.by.age[cryptic.spawning.output.by.age < 0] <- 0

    ##########################################################################
    # Step 7: Calculate unavailable mature fish with small and big fish separated

    # Get the age that corresponds to the peak mean selectivity for each year
    # If there are multiple ages with the same maximum selectivity, this will
    # give you the first (youngest) maximum
    max.selectivity.cols <- apply(mean.selectivity, 1, function(x) which.max(x))

    small.cells <- matrix(0, nrow=nrow(cryptic.spawning.output.by.age),
                          ncol=ncol(cryptic.spawning.output.by.age))
    large.cells <- matrix(0, nrow=nrow(cryptic.spawning.output.by.age),
                          ncol=ncol(cryptic.spawning.output.by.age))
    for(i in 1:nrow(small.cells)){
      small.cells[i, 1:(max.selectivity.cols[i]-1)] <- 1
      large.cells[i,max.selectivity.cols[i]:ncol(small.cells)] <- 1
    }

    small.unavailable.mature.by.age <- cryptic.spawning.output.by.age*small.cells
    large.unavailable.mature.by.age <- cryptic.spawning.output.by.age*large.cells

    # If due to rounding error any of the cryptic spawning output values
    # are less than zero, change them to zero
    small.unavailable.mature.by.age[small.unavailable.mature.by.age < 0] <- 0
    large.unavailable.mature.by.age[large.unavailable.mature.by.age < 0] <- 0

    small.unavailable.mature <- rowSums(small.unavailable.mature.by.age)
    large.unavailable.mature <- rowSums(large.unavailable.mature.by.age)

    ##########################################################################
    # Step 8:  Plot the results

    # Wrap up the plotting commands in a function
    CrypticPlots <- function(){

      ### Plot total and cryptic spawning output
      layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
      par(mar=c(5,5,1,1), oma=c(0,0,2,0))
      data <- rbind(small.unavailable.mature, large.unavailable.mature,
                    exploitable.spawning.output)
      colnames(data) <- years
  #     stackpoly(years, as.matrix(t(data)),  main='',
  #               xlab='Year', ylab='',
  #               ylim=c(0, max(total.spawning.output)*1.25),
  #                las=1, x.hash=years[1])
      stackpoly(years, as.matrix(t(data)),  main='',
                xlab='Year', ylab='',
                ylim=c(0, max(total.spawning.output)*1.25),
                col=c('red', 'green4', 'blue'),
                las=1)
      mtext('Spawning Output', 3, line=0.25)

      legend('topright',
             c('Unavailable Small', 'Unavailable Large', 'Available'),
             bty = "n", fill=c('red', 'green4', 'blue'))

      ### Plot the portion of the spawning output that is cryptic by year
      portion.unavailable <- (cryptic.spawning.output/total.spawning.output)
      portion.unavailable.small <- (small.unavailable.mature /
                                      total.spawning.output)
      portion.unavailable.large <- (large.unavailable.mature /
                                      total.spawning.output)
      plot(years, portion.unavailable, xlab='Year', ylab='',
           ylim=c(0, 1.1), type='l', lwd=2, las=1)
      mtext('Proportion of Spawing Output Unavailable', 3, line=0.25)
      lines(years, portion.unavailable.small, col='red', lwd=2)
      lines(years, portion.unavailable.large, col='green4', lwd=2)
      legend('topright', c('Unavailable Small', 'Unavailable Large',
                           'Unavailable Total'),
             bty = "n", col=c('red', 'green4', 'black'), lty=c(1,1,1))

      ### Plot cryptic spawning output by age by year as a bubble plot
      multiplier <- 2/max(cryptic.spawning.output.by.age, na.rm=TRUE)
      par(xpd=NA)
      plot(1,1, type='n', ylim=c(0,accuage+4),
           xlim=c(min(years.with.catch), max(years.with.catch)),
           las=1, ylab='Age', xlab='Year', bty='n')
      mtext('Unavailable Spawning Output by Age and Year',
            3, outer=FALSE, line=0.75)
      for(y in 1:length(years.with.catch)){
        for(a in 0:accuage){

          # plot circles for small unavailable spawning output
          radius.small <- small.unavailable.mature.by.age[y, a+1] *
                            multiplier
          symbols(x = years.with.catch[y], y = a, circles = radius.small, fg = 'black',
                  bg = 'red', lty = 1, lwd = 0.001, inches= FALSE, add=TRUE)

          # plot circles for large unavailable spawning output
          radius.large <- large.unavailable.mature.by.age[y, a+1] *
                            multiplier
          symbols(x = years.with.catch[y], y = a, circles = radius.large, fg = 'black',
                  bg = 'green4', lty = 1, lwd = 0.001, inches= FALSE, add=TRUE)

        }
      }

      # Plot the bubble plot legend
      larger.circle <- signif(max(cryptic.spawning.output.by.age), digits=2)
      symbols(x = min(years.with.catch) + 0.3*(max(years.with.catch) - min(years.with.catch)),
              y = accuage+larger.circle*multiplier,
              circles = larger.circle*multiplier,
              fg = 'black', bg = 'white', lty = 1,
              lwd = 0.001, inches= FALSE, add=TRUE)
      text(x = min(years.with.catch) + 0.3*(max(years.with.catch) - min(years.with.catch)),
           y = accuage+larger.circle*multiplier,
           labels = larger.circle,
           pos=3)

      smaller.circle <- larger.circle/2
      symbols(x = min(years.with.catch) + 0.2*(max(years.with.catch) - min(years.with.catch)),
              y = accuage+smaller.circle*multiplier,
              circles = smaller.circle*multiplier,
              fg = 'black', bg = 'white', lty = 1,
              lwd = 0.001, inches= FALSE, add=TRUE)
      text(x = min(years.with.catch) + 0.2*(max(years.with.catch) - min(years.with.catch)),
           y = accuage+smaller.circle*multiplier,
           labels = smaller.circle,
           pos=3)

      symbols(x = min(years.with.catch) + 0.65*(max(years.with.catch) - min(years.with.catch)),
              y = accuage+smaller.circle*multiplier,
              circles = smaller.circle*multiplier,
              fg = 'black', bg = 'red', lty = 1,
              lwd = 0.001, inches= FALSE, add=TRUE)
      text(x = min(years.with.catch) + 0.65*(max(years.with.catch) - min(years.with.catch)),
           y = accuage+smaller.circle*multiplier,
           labels = 'Small',
           pos=3)

      symbols(x = min(years.with.catch) + 0.85*(max(years.with.catch) - min(years.with.catch)),
              y = accuage+smaller.circle*multiplier,
              circles = smaller.circle*multiplier,
              fg = 'black', bg = 'green4', lty = 1,
              lwd = 0.001, inches= FALSE, add=TRUE)
      text(x = min(years.with.catch) + 0.85*(max(years.with.catch) - min(years.with.catch)),
           y = accuage+smaller.circle*multiplier,
           labels = 'Large',
           pos=3)

      # Add a line to show where the cut off between small and large unavailable
      lines(years.with.catch, max.selectivity.cols-1.5)

      ##### Plot mean selecitivities by length by year
      cols <- colorRampPalette(c('blue', 'orange'),
                               bias = 1, space = c("rgb", "Lab"),
                               interpolate = c("linear", "spline"),
                               alpha = FALSE)(nrow(mean.selectivity))
      cols.legend <- c(cols[1], cols[length(cols)])
      cols <- adjustcolor(cols, alpha.f = 0.3)

      plot(0,0, type='n', ylim=c(0,1.1), xlim=c(0, accuage),
           xlab='Age', ylab='Selectivity', las=1)
      mtext('Weighted Mean Selectivity by Year', 3, outer=FALSE, line=0.25)
      for(i in 1:nrow(mean.selectivity)){
        lines(0:accuage, mean.selectivity[i,], col=cols[i])
      }
      legend('bottomright', legend=c(min(years.with.catch), max(years.with.catch)), bty = "n",
             col=c(cols.legend[1], cols.legend[2]), lty=c(1,1))

      # Plot title
      mtext(paste('Unavailable Spawning Output, Area', area), side=3, outer=TRUE, line=0.5)

    }

    if(plot){
      CrypticPlots()
    }

    if(print){

      file <- paste(plotdir,"/UnavailableSpawningOutput_Area",area, ".png",sep="")
      caption <- paste('Cryptic Spawning Output, Area', area)
      plotinfo <- pngfun(file=file, caption=caption)
      CrypticPlots()
      dev.off()

    }
  }

  # Return the plot info
  if(!is.null(plotinfo)) plotinfo$category <- "UnavailableSpawningOutput"
  return(invisible(plotinfo))

}
