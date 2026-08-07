#' Create a table of biology for assessment reporting:
#' length, weight, % mature, fecundity, and selectivity
#'
#' Takes the object created by [SS_output()] to create table for reporting
#' biology at age. This function was formerly known as `SStablebiology()`
#' but that older version is now deprecated.
#'
#' @inheritParams table_exec_summary
#' @inheritParams r4ss_params
#' @param selexyr The year to summarize selectivity, NULL will use the most
#' recent year with selectivity reported in the model output.
#'
#' @family table functions
#' @return Individual .rda files containing a list of table and caption
#' @author Chantel R. Wetzel
#' @export
#'
table_biology <- function(
  replist,
  dir = replist[["inputs"]][["dir"]],
  fleetnames = replist[["FleetNames"]],
  selexyr = NULL,
  verbose = TRUE
) {
  # check the input
  check_replist(replist)

  # get vectors of reported years for age and size selectivity
  if (!is.null(replist[["ageselex"]])) {
    selexyrs_age <- unique(replist[["ageselex"]][["Yr"]])
  } else {
    selexyrs_age <- NULL
  }
  if (!is.null(replist[["sizeselex"]])) {
    selexyrs_size <- unique(replist[["sizeselex"]][["Yr"]])
  } else {
    selexyrs_size <- NULL
  }

  if (is.null(selexyr)) {
    # filter out years beyond the final year of the model
    selexyrs_age <- selexyrs_age[selexyrs_age <= replist[["endyr"]]]
    selexyrs_size <- selexyrs_size[selexyrs_size <= replist[["endyr"]]]
    if (length(selexyrs_age) == 0 && length(selexyrs_size) == 0) {
      cli::cli_abort(
        "No selectivity years found in the model output. ",
        "Check that the model output includes age and/or size selectivity."
      )
    }
    selexyr <- max(c(selexyrs_age, selexyrs_size))
    cap_selexyr <- "final model year"
  } else {
    if (!selexyr %in% c(selexyrs_age, selexyrs_size)) {
      cli::cli_abort(
        "selexyr {selexyr} is not in the model output. ",
        "Available years are {c(selexyrs_age, selexyrs_size)}"
      )
    }
    cap_selexyr <- selexyr
  }

  # create the rda_dir
  rda_dir <- file.path(
    dir,
    "tables"
  )
  check_dir(dir = rda_dir, verbose = verbose)
  # write the table to an rda file
  if (verbose) {
    cli::cli_alert_info("writing tables to {rda_dir}")
  }

  biology <- replist[["endgrowth"]] # biology at length final model year
  nsexes <- replist[["nsexes"]]
  nfleets <- replist[["nfleets"]]
  sizeselex <- replist[["sizeselex"]]
  ageselex <- replist[["ageselex"]]
  accuage <- replist[["accuage"]] # max age

  sex_labels <- c("_f", "_m")

  # Build output column names from fleet ids and sex ids.
  make_curve_names <- function(fleet_ids, sex_ids) {
    if (length(fleet_ids) == 0) {
      return(character())
    }
    if (nsexes == 1) {
      return(fleetnames[fleet_ids])
    }
    paste0(fleetnames[fleet_ids], sex_labels[sex_ids])
  }

  # Extract one selectivity or retention table for the requested year and factor.
  build_curve_table <- function(
    data,
    value_cols,
    factor_name,
    x_name,
    x_values,
    fleet_ids = seq_len(nfleets)
  ) {
    factor_rows <- data[
      data[["Yr"]] == selexyr & data[["Factor"]] == factor_name,
      ,
      drop = FALSE
    ]
    if (nrow(factor_rows) == 0 || length(fleet_ids) == 0) {
      out <- data.frame(x_values)
      names(out) <- x_name
      return(out)
    }

    combos <- expand.grid(
      Fleet = fleet_ids,
      Sex = seq_len(nsexes),
      KEEP.OUT.ATTRS = FALSE
    )
    curve_cols <- purrr::map(seq_len(nrow(combos)), function(i) {
      match_row <- factor_rows[
        factor_rows[["Fleet"]] == combos[["Fleet"]][i] &
          factor_rows[["Sex"]] == combos[["Sex"]][i],
        value_cols,
        drop = FALSE
      ]
      if (nrow(match_row) == 0) {
        return(NULL)
      }
      data.frame(
        value = round(as.numeric(match_row[1, , drop = TRUE]), digits = 2)
      )
    })
    keep_cols <- !purrr::map_lgl(curve_cols, is.null)
    out <- data.frame(x_values)
    names(out) <- x_name
    if (!any(keep_cols)) {
      return(out)
    }

    curve_names <- make_curve_names(
      fleet_ids = combos[["Fleet"]][keep_cols],
      sex_ids = combos[["Sex"]][keep_cols]
    )
    curve_cols <- purrr::map2(
      curve_cols[keep_cols],
      curve_names,
      function(curve_col, curve_name) {
        names(curve_col) <- curve_name
        curve_col
      }
    )

    out <- dplyr::bind_cols(
      out,
      dplyr::bind_cols(curve_cols)
    )
    out
  }

  # Assemble the biology-at-age table, with sex-specific columns when needed.
  build_biology_table <- function() {
    if (nsexes == 1) {
      return(data.frame(
        Age = biology[, "Age_Beg"],
        Ave_Length = round(biology[, "Len_Beg"], digits = 1),
        Ave_Wght = round(biology[, "Wt_Beg"], digits = 2),
        Mature = round(biology[, "Len_Mat"], digits = 2),
        Fecund = round(biology[, "Mat*Fecund"], digits = 2)
      ))
    }

    female <- biology[biology[["Sex"]] == 1, , drop = FALSE]
    male <- biology[biology[["Sex"]] == 2, , drop = FALSE]
    dplyr::bind_cols(
      data.frame(
        Age = female[, "Age_Beg"],
        Ave_Length_f = round(female[, "Len_Beg"], digits = 1),
        Ave_Wght_f = round(female[, "Wt_Beg"], digits = 2),
        Mature_f = round(female[, "Len_Mat"], digits = 2),
        Fecund_f = round(female[, "Mat*Fecund"], digits = 2)
      ),
      data.frame(
        Ave_Length_m = round(male[, "Len_Beg"], digits = 1),
        Ave_Wght_m = round(male[, "Wt_Beg"], digits = 2),
        Mature_m = round(male[, "Len_Mat"], digits = 2)
      )
    )
  }

  # empty list to store output
  tables <- list()

  bio_table <- TRUE
  if (replist[["wtatage_switch"]]) {
    cli::cli_alert_warning(
      "Skipping biology table because the model uses empirical weight-at-age"
    )
    bio_table <- FALSE
  }
  if (is.null(replist[["endgrowth"]])) {
    cli::cli_alert_warning(
      "Skipping biology table because the model doesn't include the 'endgrowth' output"
    )
    bio_table <- FALSE
  }
  if (bio_table) {
    # Age: Ave Len - Ave Wgt - % mature (by sex)
    # "Mat*Fecund" is = biology[["Fec"]] %*% alk (mat = 1, fecundity = fecundity_l * ALK)
    bio <- build_biology_table()

    table_biology_at_age <- list(
      cap = "Biology at age.",
      table = bio
    )
    tables[["table_biology_at_age"]] <- table_biology_at_age
    save(
      table_biology_at_age,
      file = file.path(rda_dir, "table_biology_at_age.rda")
    )
  }
  # Selectivity by age
  ageselex_clean <- ageselex[!is.na(ageselex[["Fleet"]]), , drop = FALSE]
  age_value_cols <- names(ageselex_clean)[8:ncol(ageselex_clean)]
  age_ret_fleets <- sort(unique(ageselex_clean[
    ageselex_clean[["Yr"]] == selexyr &
      ageselex_clean[["Factor"]] == "Aret" &
      ageselex_clean[["Sex"]] == 1,
    "Fleet"
  ]))
  selex.age <- build_curve_table(
    data = ageselex_clean,
    value_cols = age_value_cols,
    factor_name = "Asel",
    x_name = "Age",
    x_values = 0:accuage
  )
  selex.age.ret <- build_curve_table(
    data = ageselex_clean,
    value_cols = age_value_cols,
    factor_name = "Aret",
    x_name = "Age",
    x_values = 0:accuage,
    fleet_ids = age_ret_fleets
  )
  table_selectivity_at_age <- list(
    cap = glue::glue("Selectivity at age for each fleet in {cap_selexyr}."),
    table = selex.age
  )
  tables[["table_selectivity_at_age"]] <- table_selectivity_at_age
  save(
    table_selectivity_at_age,
    file = file.path(rda_dir, "table_selectivity_at_age.rda")
  )

  table_retention_at_age <- list(
    cap = glue::glue("Retention at age for each fleet in {cap_selexyr}."),
    table = selex.age.ret
  )
  tables[["table_retention_at_age"]] <- table_retention_at_age
  save(
    table_retention_at_age,
    file = file.path(rda_dir, "table_retention_at_age.rda")
  )

  # selectivity and retention by length
  if (!is.null(sizeselex)) {
    size_value_cols <- names(sizeselex)[6:ncol(sizeselex)]
    size_ret_fleets <- sort(unique(sizeselex[
      sizeselex[["Yr"]] == selexyr &
        sizeselex[["Factor"]] == "Keep" &
        sizeselex[["Sex"]] == 1,
      "Fleet"
    ]))
    selex.size <- build_curve_table(
      data = sizeselex,
      value_cols = size_value_cols,
      factor_name = "Lsel",
      x_name = "Length",
      x_values = as.numeric(size_value_cols)
    )
    selex.size.ret <- build_curve_table(
      data = sizeselex,
      value_cols = size_value_cols,
      factor_name = "Keep",
      x_name = "Length",
      x_values = as.numeric(size_value_cols),
      fleet_ids = size_ret_fleets
    )
    table_selectivity_at_length <- list(
      cap = glue::glue(
        "Selectivity at length for each fleet in {cap_selexyr}."
      ),
      table = selex.size
    )
    tables[["table_selectivity_at_length"]] <- table_selectivity_at_length
    save(
      table_selectivity_at_length,
      file = file.path(rda_dir, "table_selectivity_at_length.rda")
    )

    table_retention_at_length <- list(
      cap = glue::glue("Retention at length for each fleet in {cap_selexyr}."),
      table = selex.size.ret
    )
    tables[["table_retention_at_length"]] <- table_retention_at_length
    save(
      table_retention_at_length,
      file = file.path(rda_dir, "table_retention_at_length.rda")
    )
  }
  return(invisible(tables))
}
