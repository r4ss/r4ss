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
  adjustments <- match.arg(
    adjustments,
    choices = allowed_adjustments,
    several.ok = TRUE
  )

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
    inputs[["starter"]][["retro_yr"]] <- years[iyr]
    inputs[["starter"]][["init_values_src"]] <- 0

    # change settings based on the retrospective year
    inputs[["ctl"]]
    # adjust recdevs settings if requested
    if ("recdevs" %in% adjustments) {
      cli::cli_alert_info(
        "Adjusting the last year of main recdevs from {inputs[['ctl']][['MainRdevYrLast']]} to {inputs[['ctl']][['MainRdevYrLast']] + years[iyr]}"
      )
      inputs[["ctl"]][["MainRdevYrLast"]] <-
        inputs[["ctl"]][["MainRdevYrLast"]] + years[iyr]
    }

    # adjust biasadj settings if requested
    if ("biasadj" %in% adjustments) {
      cli::cli_alert_info(
        "Adjusting last_yr_fullbias_adj from {inputs[['ctl']][['last_yr_fullbias_adj']]} to {inputs[['ctl']][['last_yr_fullbias_adj']] + years[iyr]}"
      )
      cli::cli_alert_info(
        "Adjusting first_recent_yr_nobias_adj from {inputs[['ctl']][['first_recent_yr_nobias_adj']]} to {inputs[['ctl']][['first_recent_yr_nobias_adj']] + years[iyr]}"
      )
      inputs[["ctl"]][["last_yr_fullbias_adj"]] <-
        inputs[["ctl"]][["last_yr_fullbias_adj"]] + years[iyr]
      inputs[["ctl"]][["first_recent_yr_nobias_adj"]] <-
        inputs[["ctl"]][["first_recent_yr_nobias_adj"]] + years[iyr]
    }

    # adjust blocks if requested
    if ("blocks" %in% adjustments & inputs[["ctl"]][["N_Block_Designs"]] > 0) {
      if (verbose) {
        cli::cli_alert_info("Adjusting blocks for retrospective:")
      }
      # figure out which block patterns are actually used within a parameter line
      # check which blocks are actually used
      block_designs_used <- c(
        inputs$ctl$MG_parms$Block,
        inputs$ctl$SR_parms$Block,
        inputs$ctl$Q_parms$Block,
        inputs$ctl$size_selex_parms$Block,
        inputs$ctl$age_selex_parms$Block
      ) |>
        unique() |>
        sort() |>
        setdiff(0)
      if (verbose) {
        cli::cli_alert_info(
          "Block designs referenced by parameter lines in control file: {toString(block_designs_used)}"
        )
      }

      # check if the used blocks are still within the modeled period
      for (block_design in 1:inputs$ctl$N_Block_Designs) {
        if (
          block_design %in%
            block_designs_used
        ) {
          if (verbose) {
            cli::cli_alert_info(
              "Evaluating Block_Design {block_design} with {cli::pluralize('{inputs$ctl$blocks_per_pattern[block_design]} block{?s}')}..."
            )
          }
          for (block in seq_along(inputs$ctl$blocks_per_pattern[
            block_design
          ])) {
            # if start year of block is greater than the start year of the model, remove final block from block design
            # note block design is a vector of length 2 * number of blocks, with start and end years alternating
            if (
              inputs$ctl$Block_Design[[block_design]][block * 2 - 1] >
                inputs$dat$endyr
            ) {
              if (verbose) {
                cli::cli_alert_info(
                  "Removing block {block} from Block_Design {block_design} because start year {inputs$ctl$Block_Design[[block_design]][block * 2 - 1]} is beyond model end year {inputs$dat$endyr}."
                )
              }
              # remove block from design
              inputs$ctl$Block_Design[[
                block_design
              ]] <- inputs$ctl$Block_Design[[block_design]][
                -c((block * 2 - 1):(block * 2))
              ]
              # reduce number of blocks per design
              inputs$ctl$blocks_per_pattern[
                block_design
              ] <- inputs$ctl$blocks_per_pattern[block_design] - 1
              if (verbose) {
                cli::cli_alert_info(
                  "Block_Design {block_design} now has {cli::pluralize('{inputs$ctl$blocks_per_pattern[block_design]} block{?s}')}."
                )
              }
              # check if there are no more blocks left in the design in which case
              # any parameter lines associated with this block design should be updated
              if (inputs$ctl$blocks_per_pattern[block_design] == 0) {
                if (verbose) {
                  cli::cli_alert_info(
                    "No blocks remain in Block_Design {block_design}; resetting associated parameter Block values to 0."
                  )
                }
                # update any parameter lines associated with this block design
                # change any value in the Block column matching `block_design` to 0
                # and apply it to the following dataframes:
                inputs$ctl$MG_parms$Block <- ifelse(
                  inputs$ctl$MG_parms$Block == block_design,
                  0,
                  inputs$ctl$MG_parms$Block
                )
                inputs$ctl$SR_parms$Block <- ifelse(
                  inputs$ctl$SR_parms$Block == block_design,
                  0,
                  inputs$ctl$SR_parms$Block
                )
                inputs$ctl$Q_parms$Block <- ifelse(
                  inputs$ctl$Q_parms$Block == block_design,
                  0,
                  inputs$ctl$Q_parms$Block
                )
                inputs$ctl$size_selex_parms$Block <- ifelse(
                  inputs$ctl$size_selex_parms$Block == block_design,
                  0,
                  inputs$ctl$size_selex_parms$Block
                )
                inputs$ctl$age_selex_parms$Block <- ifelse(
                  inputs$ctl$age_selex_parms$Block == block_design,
                  0,
                  inputs$ctl$age_selex_parms$Block
                )
                # add back placeholder block values to avoid having to renumber blocks in other parameter lines
                inputs$ctl$Block_Design[[block_design]] <- c(
                  inputs$dat$styr,
                  inputs$dat$styr
                )
                inputs$ctl$blocks_per_pattern[block_design] <- 1
                if (verbose) {
                  cli::cli_alert_info(
                    "Inserted placeholder Block_Design years ({inputs$dat$styr}, {inputs$dat$styr}) for Block_Design {block_design}."
                  )
                }
              }
            } else {
              cli::cli_alert_info(
                "No change needed for Block_Design {block_design}"
              )
            }
          }
          # } else {
          # if (verbose) {
          #   cli::cli_alert_info(
          #     "Skipping Block_Design {block_design} because it is not referenced by active parameter lines."
          #   )
          # }
        }
      }
    }

    # adjust forecast values if requested
    if ("forecast" %in% adjustments) {
      mean_year <- mean(c(
        inputs[["dat"]][["styr"]],
        inputs[["dat"]][["endyr"]]
      ))
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
        # if Fcast_years is a data frame, loop over rows
        for (i in seq_len(nrow(inputs[["fore"]][["Fcast_years"]]))) {
          # dataframe has format like:
          #                MG_type method st_year end_year
          # #_Fcast_years1      10      1      -4        0
          # #_Fcast_years2      11      1      -4        0
          # #_Fcast_years3      12      1      -4        0

          # compute input year as either absolute or relative year
          val <- inputs[["fore"]][["Fcast_years"]][i, "st_year"]
          if (val <= 0) {
            val <- val + inputs[["dat"]][["endyr"]]
          }
          # check for whether the beginning year of the interval is closer to the start year of the model
          if (val > mean_year) {
            # adjust start end year of the period
            if (verbose & years[iyr] != 0) {
              cli::cli_li(
                "Adjusting Fcast_years [{i}, 'st_year'] from {inputs[['fore']][['Fcast_years']][i, 'st_year']} to {inputs[['fore']][['Fcast_years']][i, 'st_year'] + years[iyr]}"
              )
            }
            inputs[["fore"]][["Fcast_years"]][i, "st_year"] <- inputs[[
              "fore"
            ]][["Fcast_years"]][i, "st_year"] +
              years[iyr]
          }
          # always adjust the ending year of the interval
          if (verbose & years[iyr] != 0) {
            cli::cli_li(
              "Adjusting Fcast_years [{i}, 'end_year'] from {inputs[['fore']][['Fcast_years']][i, 'end_year']} to {inputs[['fore']][['Fcast_years']][i, 'end_year'] + years[iyr]}"
            )
          }
          inputs[["fore"]][["Fcast_years"]][i, "end_year"] <- inputs[["fore"]][[
            "Fcast_years"
          ]][i, "end_year"] +
            years[iyr]
        }
      } else {
        # vector format for Fcast_years, where intervals are stored sequentially
        for (i in c(1, 3, 5)) {
          # compute input year as either absolute or relative year
          val <- inputs[["fore"]][["Fcast_years"]][i]
          if (val <= 0) {
            val <- val + inputs[["dat"]][["endyr"]]
          }
          if (
            # check for whether the beginning year of the interval is closer to the start year of the model
            val < mean_year
          ) {
            if (verbose & years[iyr] != 0) {
              # adjust only the ending value
              cli::cli_li(
                "{Fcast_years_names[[i + 1]]} from {inputs[['fore']][['Fcast_years']][i + 1]} to {inputs[['fore']][['Fcast_years']][i + 1] + years[iyr]}"
              )
            }
            inputs[["fore"]][["Fcast_years"]][i + 1] <-
              inputs[["fore"]][["Fcast_years"]][i + 1] + years[iyr]
          } else {
            if (verbose & years[iyr] != 0) {
              for (j in 0:1) {
                cli::cli_li(
                  "Shifting Fcast_years {Fcast_years_names[[i + j]]} from {inputs[['fore']][['Fcast_years']][i + j]} to {inputs[['fore']][['Fcast_years']][i + j] + years[iyr]}"
                )
              }
            }
            # adjust both values
            inputs[["fore"]][["Fcast_years"]][i + 0:1] <-
              inputs[["fore"]][["Fcast_years"]][i + 0:1] + years[iyr]
          }
        }
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
      for (i in c(1, 3, 5, 7, 9)) {
        # compute input year as either absolute or relative year
        val <- inputs[["fore"]][["Bmark_years"]][i]
        if (val <= 0) {
          val <- val + inputs[["dat"]][["endyr"]]
        }
        if (
          # check for whether the beginning year of the interval is closer to the start year of the model
          val < mean_year
        ) {
          if (verbose & years[iyr] != 0) {
            # adjust only the ending value
            cli::cli_li(
              "{Bmark_years_names[[i + 1]]} from {inputs[['fore']][['Bmark_years']][i + 1]} to {inputs[['fore']][['Bmark_years']][i + 1] + years[iyr]}"
            )
          }
          # adjust only the ending year
          inputs[["fore"]][["Bmark_years"]][i + 1] <-
            inputs[["fore"]][["Bmark_years"]][i + 1] + years[iyr]
        } else {
          if (verbose & years[iyr] != 0) {
            for (j in 0:1) {
              cli::cli_li(
                "Shifting Bmark_years {Bmark_years_names[[i + j]]} from {inputs[['fore']][['Bmark_years']][i + j]} to {inputs[['fore']][['Bmark_years']][i + j] + years[iyr]}"
              )
            }
          }
          # adjust both start and end year
          inputs[["fore"]][["Bmark_years"]][i + 0:1] <-
            inputs[["fore"]][["Bmark_years"]][i + 0:1] + years[iyr]
        }
      }
      cli::cli_end(id = "Bmark_years")
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
