#' Deprecated function to run a retrospective analyses, renamed to retro()
#'
#' @inheritParams r4ss_params
#' @description
#' `r lifecycle::badge("deprecated")`
#' SS_doRetro() has been renamed as [retro()]. See
#' https://github.com/r4ss/r4ss/issues/723 for more details.
#'
#' @author Ian G. Taylor
#' @export
#' @seealso [retro()]
SS_doRetro <-
  function(...) {
    lifecycle::deprecate_stop(
      when = "4.6.1",
      what = "SS_doRetro()",
      with = "retro()"
    )
  }

#' Run a retrospective analyses
#'
#' Do retrospective analyses by creating new directories, copying model files,
#' and iteratively changing the starter file to set the number of years of data
#' to exclude. Note that there was a  bug for retrospectives in 3.30.01;
#' the user should update their model to a newer version of Stock Synthesis to
#' run retrospectives. To run retrospective models in parallel, use [future::plan()]
#' before running `retro()`.
#'
#' @param dir Directory where everything takes place.
#' @param masterdir Deprecated. Use `dir` instead.
#' @param oldsubdir Subdirectory within `dir` with existing model
#' files.
#' @param newsubdir Subdirectory within `dir` where retrospectives
#' will be run. Default is 'retrospectives'.
#' @param subdirstart First part of the pattern of names for the directories in
#' which the models will actually be run.
#' @param years Vector of values to iteratively enter into the starter file for
#' retrospective year. Should be zero or negative values.
#' @param overwrite Overwrite any input files with matching names in the
#' subdirectories where models will be run.
#' @param RemoveBlocks Logical switch determining whether specifications of
#' blocks is removed from top of control file. Blocks can cause problems for
#' retrospective analyses, but the method for removing them is overly
#' simplistic and probably won't work in most cases. Default=FALSE.
#' @inheritParams r4ss_params
#' @param ... Additional arguments passed to [r4ss::run()], such as
#' `extras`, `show_in_console`, and `skipfinished`.
#'
#' @author Ian G. Taylor, James T. Thorson, Kathryn L. Doering, Kiva L. Oken
#' @export
#' @import furrr
#' @seealso [SSgetoutput()]
#' @family run functions
#' @examples
#' \dontrun{
#' # note: don't run this in your main directory--make a copy in case something
#' # goes wrong
#' mydir <- system.file("extdata", "simple_small", package = "r4ss")
#'
#' ## retrospective analyses
#' retro(
#'   dir = mydir,
#'   years = 0:-5
#' )
#'
#' retroModels <- SSgetoutput(
#'   dirvec = file.path(mydir, "retrospectives", paste("retro", 0:-5, sep = ""))
#' )
#' retroSummary <- SSsummarize(retroModels)
#' endyrvec <- retroSummary[["endyrs"]] + 0:-5
#' SSplotComparisons(retroSummary,
#'   endyrvec = endyrvec,
#'   legendlabels = paste("Data", 0:-5, "years")
#' )
#'
#' ## run retrospectives in parallel
#' ncores <- parallelly::availableCores(omit = 1)
#' future::plan(future::multisession, workers = ncores)
#' retro(
#'   dir = mydir,
#'   years = 0:-5
#' )
#' future::plan(future::sequential)
#' }
#'
retro <- function(
  dir = getwd(),
  masterdir = lifecycle::deprecated(),
  oldsubdir = "",
  newsubdir = "retrospectives",
  subdirstart = "retro",
  years = 0:-5,
  overwrite = TRUE,
  adjustments = c("recdevs", "biasadj", "blocks", "forecast", "benchmarks"),
  verbose = FALSE,
  exe = "ss3",
  ...
) {
  # deprecated variable warnings -----
  # soft deprecated for now, but fully deprecate in the future.
  if (lifecycle::is_present(masterdir)) {
    lifecycle::deprecate_warn(
      when = "1.46.0",
      what = "retro(masterdir)",
      with = "retro(dir)"
    )
    dir <- masterdir
  }

  allowed_adjustments <- c(
    "recdevs",
    "biasadj",
    "forecast",
    "benchmarks",
    "blocks"
  )
  if (is.null(adjustments)) {
    adjustments <- character(0)
  } else {
    adjustments <- match.arg(
      adjustments,
      choices = allowed_adjustments,
      several.ok = TRUE
    )
  }

  olddir <- file.path(dir, oldsubdir)
  newdir <- file.path(dir, newsubdir)

  # get model file names from olddir
  startfile <- dir(olddir)[tolower(dir(olddir)) == "starter.ss"]
  if (length(startfile) == 0) {
    cli::cli_abort("No starter.ss file found in {olddir}")
  }

  # read original input files (later written to each folder)
  inputs <- SS_read(olddir, verbose = FALSE)
  subdirnames <- paste0(subdirstart, years)

  # check for executable
  check_exe(exe = exe, dir = olddir, verbose = verbose)

  # Shift one or more scalar ctl fields by the same retrospective offset.
  shift_ctl_fields <- function(inputs, fields, delta, verbose) {
    for (field in fields) {
      if (verbose) {
        cli::cli_alert_info(
          "Adjusting {field} from {inputs[['ctl']][[field]]} to {inputs[['ctl']][[field]] + delta}"
        )
      }
      inputs[["ctl"]][[field]] <- inputs[["ctl"]][[field]] + delta
    }
    inputs
  }

  # Apply the shared interval-shift rule used for vector year pairs:
  # shift only end year for early intervals, otherwise shift start and end.
  shift_interval_pairs <- function(
    year_values,
    pair_starts,
    names,
    endyr,
    mean_year,
    delta,
    verbose,
    section_name
  ) {
    for (i in pair_starts) {
      # Interpret non-positive values as years relative to endyr.
      val <- year_values[i]
      if (val <= 0) {
        val <- val + endyr
      }
      if (val < mean_year) {
        if (verbose && delta != 0) {
          cli::cli_li(
            "{section_name} {names[[i + 1]]} from {year_values[i + 1]} to {year_values[i + 1] + delta}"
          )
        }
        year_values[i + 1] <- year_values[i + 1] + delta
      } else {
        if (verbose && delta != 0) {
          for (j in 0:1) {
            cli::cli_li(
              "{section_name} {names[[i + j]]} from {year_values[i + j]} to {year_values[i + j] + delta}"
            )
          }
        }
        year_values[i + 0:1] <- year_values[i + 0:1] + delta
      }
    }
    year_values
  }

  # Handle data-frame Fcast_years format where each row stores st_year/end_year.
  shift_fcast_years_df <- function(
    fcast_years,
    endyr,
    mean_year,
    delta,
    verbose
  ) {
    for (i in seq_len(nrow(fcast_years))) {
      val <- fcast_years[i, "st_year"]
      if (val <= 0) {
        val <- val + endyr
      }
      if (val >= mean_year) {
        if (verbose && delta != 0) {
          cli::cli_li(
            "Fcast_years [{i}, 'st_year'] from {fcast_years[i, 'st_year']} to {fcast_years[i, 'st_year'] + delta}"
          )
        }
        fcast_years[i, "st_year"] <- fcast_years[i, "st_year"] + delta
      }
      if (verbose && delta != 0) {
        cli::cli_li(
          "Fcast_years [{i}, 'end_year'] from {fcast_years[i, 'end_year']} to {fcast_years[i, 'end_year'] + delta}"
        )
      }
      fcast_years[i, "end_year"] <- fcast_years[i, "end_year"] + delta
    }
    fcast_years
  }

  # Reset to 0 all parameter-table Block columns that reference a removed block design
  reset_block_columns <- function(ctl, block_design) {
    parm_tables <- c(
      "MG_parms",
      "SR_parms",
      "Q_parms",
      "size_selex_parms",
      "age_selex_parms"
    )
    for (table_name in parm_tables) {
      ctl[[table_name]][["Block"]][
        ctl[[table_name]][["Block"]] == block_design
      ] <- 0
    }
    ctl
  }

  # Remove short time-varying parameter rows for removed block years.
  remove_block_tv_rows <- function(
    ctl,
    block_design,
    removed_start_years,
    verbose
  ) {
    tv_tables <- c(
      "MG_parms_tv",
      "SR_parms_tv",
      "Q_parms_tv",
      "size_selex_parms_tv",
      "age_selex_parms_tv"
    )
    if (length(removed_start_years) == 0) {
      return(ctl)
    }

    year_pattern <- paste0(removed_start_years, collapse = "|")
    row_pattern <- paste0("_BLK", block_design, ".*_((", year_pattern, "))$")

    for (table_name in tv_tables) {
      if (is.null(ctl[[table_name]]) || nrow(ctl[[table_name]]) == 0) {
        next
      }
      keep_rows <- !grepl(row_pattern, rownames(ctl[[table_name]]))
      n_removed <- sum(!keep_rows)
      if (n_removed > 0) {
        ctl[[table_name]] <- ctl[[table_name]][keep_rows, , drop = FALSE]
        if (verbose) {
          cli::cli_alert_info(
            "Removed {n_removed} time-varying row{?s} from {table_name} for Block_Design {block_design} start year{?s} {toString(removed_start_years)}."
          )
        }
      }
    }
    ctl
  }

  # Remove block periods that start after the modeled end year and keep ctl consistent.
  adjust_blocks_for_retro <- function(inputs, retro_endyr, verbose) {
    if (inputs[["ctl"]][["N_Block_Designs"]] <= 0) {
      return(inputs)
    }
    if (verbose) {
      cli::cli_alert_info("Adjusting blocks for retrospective:")
    }
    block_designs_used <- c(
      inputs[["ctl"]][["MG_parms"]][["Block"]],
      inputs[["ctl"]][["SR_parms"]][["Block"]],
      inputs[["ctl"]][["Q_parms"]][["Block"]],
      inputs[["ctl"]][["size_selex_parms"]][["Block"]],
      inputs[["ctl"]][["age_selex_parms"]][["Block"]]
    ) |>
      unique() |>
      sort() |>
      setdiff(0)

    if (verbose) {
      cli::cli_alert_info(
        "Block designs referenced by parameter lines in control file: {toString(block_designs_used)}"
      )
    }

    for (block_design in block_designs_used) {
      # Guard against stale references to block designs outside N_Block_Designs.
      if (block_design > inputs[["ctl"]][["N_Block_Designs"]]) {
        next
      }

      design <- inputs[["ctl"]][["Block_Design"]][[block_design]]
      # Block_Design alternates start/end years, so take odd positions as starts.
      start_years <- design[seq(1, length(design), by = 2)]
      remove_blocks <- which(start_years > retro_endyr)
      removed_start_years <- start_years[remove_blocks]

      if (length(remove_blocks) == 0) {
        if (verbose) {
          cli::cli_alert_info(
            "Block_Design {block_design} with {cli::pluralize('{inputs[['ctl']][['blocks_per_pattern']][block_design]} block{?s}')}: no change needed for retro end year {retro_endyr}."
          )
        }
        next
      }

      if (verbose) {
        cli::cli_alert_info(
          "Block_Design {block_design} with {cli::pluralize('{inputs[['ctl']][['blocks_per_pattern']][block_design]} block{?s}')}, retro end year {retro_endyr}"
        )
      }

      for (block in rev(remove_blocks)) {
        if (verbose) {
          cli::cli_alert_warning(
            "Removing block {block} from Block_Design {block_design} because start year {start_years[block]} is beyond retro end year {retro_endyr}."
          )
        }
        # Remove from highest index first so earlier indices remain valid.
        design <- design[-c((block * 2 - 1):(block * 2))]
      }

      inputs[["ctl"]] <- remove_block_tv_rows(
        ctl = inputs[["ctl"]],
        block_design = block_design,
        removed_start_years = removed_start_years,
        verbose = verbose
      )

      inputs[["ctl"]][["Block_Design"]][[block_design]] <- design
      inputs[["ctl"]][["blocks_per_pattern"]][block_design] <- length(design) /
        2

      if (verbose) {
        cli::cli_alert_info(
          "Block_Design {block_design} now has {cli::pluralize('{inputs[['ctl']][['blocks_per_pattern']][block_design]} block{?s}')}"
        )
      }

      if (inputs[["ctl"]][["blocks_per_pattern"]][block_design] == 0) {
        if (verbose) {
          cli::cli_alert_info(
            "No blocks remain in Block_Design {block_design}; resetting associated parameter Block values to 0."
          )
        }
        inputs[["ctl"]] <- reset_block_columns(inputs[["ctl"]], block_design)
        inputs[["ctl"]][["Block_Design"]][[block_design]] <- c(
          inputs[["dat"]][["styr"]],
          inputs[["dat"]][["styr"]]
        )
        inputs[["ctl"]][["blocks_per_pattern"]][block_design] <- 1
        if (verbose) {
          cli::cli_alert_info(
            "Inserted placeholder Block_Design years ({inputs[['dat']][['styr']]}, {inputs[['dat']][['styr']]}) for Block_Design {block_design}."
          )
        }
      }
    }

    inputs
  }

  # loop over retrospective years
  furrr::future_walk(seq_along(years), function(iyr) {
    newdir_iyr <- file.path(newdir, subdirnames[iyr])
    if (verbose) {
      cli::cli_alert_info("Running retrospective in {newdir_iyr}")
    }

    # copy original input files to retro folder,
    # they get overwritten later, but this will move the executable
    # (if present)
    copy_SS_inputs(
      dir.old = olddir,
      dir.new = newdir_iyr,
      create.dir = TRUE,
      recursive = TRUE,
      overwrite = TRUE,
      copy_exe = TRUE,
      verbose = FALSE
    )

    # change starter file to do retrospectives
    inputs[["start"]][["retro_yr"]] <- years[iyr]
    inputs[["start"]][["init_values_src"]] <- 0

    # change settings based on the retrospective year
    year_shift <- years[iyr]
    retro_endyr <- inputs[["dat"]][["endyr"]] + year_shift
    mean_year <- mean(c(
      inputs[["dat"]][["styr"]],
      inputs[["dat"]][["endyr"]]
    ))

    # adjust recdevs settings if requested
    if ("recdevs" %in% adjustments) {
      inputs <- shift_ctl_fields(
        inputs = inputs,
        fields = "MainRdevYrLast",
        delta = year_shift,
        verbose = verbose
      )
    }

    # adjust biasadj settings if requested
    if ("biasadj" %in% adjustments) {
      inputs <- shift_ctl_fields(
        inputs = inputs,
        fields = c("last_yr_fullbias_adj", "first_recent_yr_nobias_adj"),
        delta = year_shift,
        verbose = verbose
      )
    }

    # adjust blocks if requested
    if ("blocks" %in% adjustments) {
      inputs <- adjust_blocks_for_retro(
        inputs = inputs,
        retro_endyr = retro_endyr,
        verbose = verbose
      )
    }

    # adjust forecast values if requested
    if ("forecast" %in% adjustments) {
      #_Fcast_years for averaging:  beg_selex, end_selex, beg_relF, end_relF, beg_mean recruits, end_recruits  (enter actual year, or values of 0 or -integer to be rel. endyr)
      Fcast_years_names <- c(
        "beg_selex",
        "end_selex",
        "beg_relF",
        "end_relF",
        "beg_mean_recruits",
        "end_recruits"
      )
      if (verbose) {
        cli::cli_ul(
          id = "Fcast_years"
        )
        cli::cli_alert_info("Adjusting Fcast_years for retrospective:")
      }

      if (is.data.frame(inputs[["fore"]][["Fcast_years"]])) {
        # Newer forecast format: each row is an interval.
        inputs[["fore"]][["Fcast_years"]] <- shift_fcast_years_df(
          fcast_years = inputs[["fore"]][["Fcast_years"]],
          endyr = inputs[["dat"]][["endyr"]],
          mean_year = mean_year,
          delta = year_shift,
          verbose = verbose
        )
      } else {
        # Legacy forecast format: interval years stored in alternating vector positions.
        inputs[["fore"]][["Fcast_years"]] <- shift_interval_pairs(
          year_values = inputs[["fore"]][["Fcast_years"]],
          pair_starts = c(1, 3, 5),
          names = Fcast_years_names,
          endyr = inputs[["dat"]][["endyr"]],
          mean_year = mean_year,
          delta = year_shift,
          verbose = verbose,
          section_name = "Fcast_years"
        )
      }
      if (verbose) {
        cli::cli_end(id = "Fcast_years")
      }
    }

    if ("benchmarks" %in% adjustments) {
      # Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF, beg_recr_dist, end_recr_dist, beg_SRparm, end_SRparm (enter actual year, or values of 0 or -integer to be rel. endyr)
      Bmark_years_names <- c(
        "beg_bio",
        "end_bio",
        "beg_selex",
        "end_selex",
        "beg_relF",
        "end_relF",
        "beg_recr_dist",
        "end_recr_dist",
        "beg_SRparm",
        "end_SRparm"
      )
      if (verbose) {
        cli::cli_ul(
          id = "Bmark_years"
        )
        cli::cli_alert_info("Adjusting Bmark_years for retrospective:")
      }
      # Benchmark years currently use the same alternating vector format as legacy forecast.
      inputs[["fore"]][["Bmark_years"]] <- shift_interval_pairs(
        year_values = inputs[["fore"]][["Bmark_years"]],
        pair_starts = c(1, 3, 5, 7, 9),
        names = Bmark_years_names,
        endyr = inputs[["dat"]][["endyr"]],
        mean_year = mean_year,
        delta = year_shift,
        verbose = verbose,
        section_name = "Bmark_years"
      )
      if (verbose) {
        cli::cli_end(id = "Bmark_years")
      }
    }

    # overwrite the input files
    SS_write(
      inputlist = inputs,
      dir = newdir_iyr,
      verbose = FALSE,
      overwrite = TRUE
    )

    # delete covar file to avoid using file from previous model run
    # (not sure if this is necessary)
    if (file.exists(file.path(newdir_iyr, "covar.sso"))) {
      file.remove(file.path(newdir_iyr, "covar.sso"))
    }

    # run model
    run(dir = newdir_iyr, exe = exe, verbose = verbose, ...)

    # add rough check for if the model ran (although a report file may exist if
    # if the model only ran part of the way through). Warn the user in this case.
    if (!file.exists(file.path(newdir_iyr, "Report.sso"))) {
      cli::cli_warn("The retrospective model run failed in {newdir_iyr}")
    }
  })
  invisible(file.path(dir, newsubdir, subdirnames))
}
