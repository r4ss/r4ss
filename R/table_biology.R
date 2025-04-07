#' Create a table of biology for assessment reporting:
#' length, weight, % mature, fecundity, and selectivity
#'
#' Takes the object created by [SS_output()] to create table for reporting
#' biology at age. This function was formerly known as `SStablebiology()`
#' but that older version is now deprecated.
#'
#' @inheritParams table_exec_summary
#' @template fleetnames
#' @param selexyr The year to summarize selectivity, the default is the final
#' model year.
#'
#' @family table functions
#' @return Individual .rda files containing a list of table and caption
#' @author Chantel R. Wetzel
#' @export
#'
table_biology <- function(
    replist,
    dir = NULL,
    fleetnames = NULL,
    selexyr = NULL,
    verbose = TRUE) {
  # check the input
  check_replist(replist)

  # create the rda_dir
  rda_dir <- file.path(
    ifelse(
      is.null(dir),
      yes = replist[["inputs"]][["dir"]],
      no = dir
    ),
    "tables"
  )
  dir.create(rda_dir, showWarnings = FALSE)
  check_dir(dir = rda_dir, verbose = verbose)
  # write the table to an rda file
  if (verbose) {
    cli::cli_alert_info("writing tables to {rda_dir}")
  }

  biology <- replist[["endgrowth"]] # biology at length final model year
  nsexes <- replist[["nsexes"]]
  nfleets <- replist[["nfleets"]]
  lbinspop <- replist[["lbinspop"]]
  nlbinspop <- replist[["nlbinspop"]]
  sizeselex <- replist[["sizeselex"]]
  ageselex <- replist[["ageselex"]]
  accuage <- replist[["accuage"]] # max age
  FleetNames <- replist[["FleetNames"]]

  # set fleet-specific names, and plotting parameters
  if (is.null(fleetnames)) {
    fleetnames <- FleetNames
  }

  # determine the year to summarize
  if (is.null(selexyr)) {
    selexyr <- replist[["endyr"]]
  }

  # empty list to store output
  tables <- list()

  bio_table <- TRUE
  if (replist[["wtatage_switch"]]) {
    cli::cli_alert_warning("Skipping biology table because the model uses empirical weight-at-age")
    bio_table <- FALSE
  }
  if (is.null(replist[["endgrowth"]])) {
    cli::cli_alert_warning("Skipping biology table because the model doesn't include the 'endgrowth' output")
    bio_table <- FALSE
  }
  if (bio_table) {
    if (nsexes == 1) {
      # Table
      # Age: Ave Len - Ave Wgt - % mature (by sex)
      # "Mat*Fecund" is = biology[["Fec"]] %*% alk (mat = 1, fecundity = fecundity_l * ALK)
      bio <- data.frame(
        Age = biology[, "Age_Beg"],
        Ave_Length = round(biology[, "Len_Beg"], digits = 1),
        Ave_Wght = round(biology[, "Wt_Beg"], digits = 2),
        Mature = round(biology[, "Len_Mat"], digits = 2),
        Fecund = round(biology[, "Mat*Fecund"], digits = 2)
      )
    }
    if (nsexes == 2) {
      bio <- data.frame(
        Age = biology[biology[["Sex"]] == 1, "Age_Beg"],
        Ave_Length_f = round(biology[biology[["Sex"]] == 1, "Len_Beg"], digits = 1),
        Ave_Wght_f = round(biology[biology[["Sex"]] == 1, "Wt_Beg"], digits = 2),
        Mature_f = round(biology[biology[["Sex"]] == 1, "Len_Mat"], digits = 2),
        Fecund_f = round(biology[biology[["Sex"]] == 1, "Mat*Fecund"], digits = 2),
        Ave_Length_m = round(
          biology[biology[["Sex"]] == 2, "Len_Beg"],
          digits = 1
        ),
        Ave_Wght_m = round(biology[biology[["Sex"]] == 2, "Wt_Beg"], digits = 2),
        Mature_m = round(biology[biology[["Sex"]] == 2, "Len_Mat"], digits = 2)
      )
    }

    table_biology_at_age <- list(
      cap = "Biology at age.",
      table = bio
    )
    tables[["table_biology_at_age"]] <- table_biology_at_age
    save(table_biology_at_age, file = file.path(rda_dir, "table_biology_at_age.rda"))
  }
  # Selectivity by age
  retnames <- NULL
  selex.age <- selex.age.ret <- data.frame(Age = 0:accuage)
  for (j in 1:nsexes) {
    for (i in 1:nfleets) {
      ind <- ageselex[!is.na(ageselex[["Fleet"]]), "Fleet"] == i
      find <- which(
        ageselex[ind, "Sex"] == j &
          ageselex[ind, "Yr"] == selexyr &
          ageselex[ind, "Factor"] == "Asel"
      )
      selex.age <- data.frame(
        selex.age,
        round(as.numeric(ageselex[find, 8:dim(ageselex)[2]]), digits = 2)
      )
    }

    for (i in 1:nfleets) {
      find <- which(
        ageselex[["Fleet"]] == i &
          ageselex[["Sex"]] == j &
          ageselex[["Yr"]] == selexyr &
          ageselex[["Factor"]] == "Aret"
      )
      if (length(find) != 0) {
        if (j == 1) {
          retnames <- c(retnames, FleetNames[i])
        }
        selex.age.ret <- data.frame(
          selex.age.ret,
          round(as.numeric(ageselex[find, 8:dim(ageselex)[2]]), digits = 2)
        )
      }
    }
  }
  if (nsexes == 1) {
    colnames(selex.age) <- c("Age", FleetNames)
    if (!is.null(retnames)) {
      colnames(selex.age.ret) <- c("Age", retnames)
    }
  } else {
    colnames(selex.age) <- c(
      "Age",
      paste0(FleetNames, "_f"),
      paste0(FleetNames, "_m")
    )
    if (!is.null(retnames)) {
      colnames(selex.age.ret) <- c(
        "Age",
        paste0(retnames, "_f"),
        paste0(retnames, "_m")
      )
    }
  }
  table_selectivity_at_age <- list(
    cap = "Selectivity at age for each fleet.",
    table = selex.age
  )
  tables[["table_selectivity_at_age"]] <- table_selectivity_at_age
  save(table_selectivity_at_age, file = file.path(rda_dir, "table_selectivity_at_age.rda"))

  table_retention_at_age <- list(
    cap = "Retention at age for each fleet.",
    table = selex.age.ret
  )
  tables[["table_retention_at_age"]] <- table_retention_at_age
  save(table_retention_at_age, file = file.path(rda_dir, "table_retention_at_age.rda"))

  # selecitivity and retention by length
  if (!is.null(sizeselex)) {
    retnames <- NULL
    selex.size <- selex.size.ret <- data.frame(
      Length = as.numeric(names(sizeselex[6:dim(sizeselex)[2]]))
    )
    for (j in 1:nsexes) {
      for (i in 1:nfleets) {
        find <- which(
          sizeselex[["Fleet"]] == i &
            sizeselex[["Sex"]] == j &
            sizeselex[["Yr"]] == selexyr &
            sizeselex[["Factor"]] == "Lsel"
        )
        selex.size <- data.frame(
          selex.size,
          round(as.numeric(sizeselex[find, 6:dim(sizeselex)[2]]), digits = 2)
        )

        find <- which(
          sizeselex[["Fleet"]] == i &
            sizeselex[["Sex"]] == j &
            sizeselex[["Yr"]] == selexyr &
            sizeselex[["Factor"]] == "Keep"
        )
        if (length(find) != 0) {
          if (j == 1) {
            retnames <- c(retnames, FleetNames[i])
          }
          selex.size.ret <- data.frame(
            selex.size.ret,
            round(as.numeric(sizeselex[find, 6:dim(sizeselex)[2]]), digits = 2)
          )
        }
      }
    }
    if (nsexes == 1) {
      colnames(selex.size) <- c(
        "Length",
        FleetNames
      )
    } else {
      colnames(selex.size) <- c(
        "Length",
        paste0(FleetNames, "_f"),
        paste0(FleetNames, "_m")
      )
      colnames(selex.size.ret) <- c(
        "Length",
        paste0(retnames, "_f"),
        paste0(retnames, "_m")
      )
    }
    table_selectivity_at_length <- list(
      cap = "Selectivity at length for each fleet.",
      table = selex.size
    )
    tables[["table_selectivity_at_length"]] <- table_selectivity_at_length
    save(table_selectivity_at_length, file = file.path(rda_dir, "table_selectivity_at_length.rda"))

    table_retention_at_length <- list(
      cap = "Retention at length for each fleet.",
      table = selex.size.ret
    )
    tables[["table_retention_at_length"]] <- table_retention_at_length
    save(table_retention_at_length, file = file.path(rda_dir, "table_retention_at_length.rda"))
  }
  return(invisible(tables))
}
