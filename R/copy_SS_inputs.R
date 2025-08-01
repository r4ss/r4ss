#' Copy a the Stock Synthesis input files from one directory to another
#'
#' Reads the starter.ss file to figure out the names of the control and
#' data files, than copies those files along with starter.ss, forecast.ss,
#' and wtatage.ss (if present) to a new directory, as specified.
#'
#' @param dir.old Location of model files to be copied, either an absolute
#' path or relative to the working directory.
#' @param dir.new New location to which the files should be copied,
#' either an absolute path or relative to the working directory.
#' @param create.dir Create `dir.new` directory if it doesn't exist already?
#' @template overwrite
#' @param recursive A logical value passed to the `recursive` argument of
#' [dir.create()] that specifies if elements of the path other than the last
#' be created?
#' @param use_ss_new Use .ss_new files instead of original inputs?
#' @param copy_exe Copy any executables found in `dir.old` to `dir.new` or
#' dir.exe (if provided)?
#' @param copy_par Copy any .par files found in `dir.old` to `dir.new`?
#' @param dir.exe Path to executable to copy instead of any in `dir.old`.
#' @template verbose
#' @return
#' A logical value is invisibly returned, indicating whether all input files
#' were copied successfully.
#' @author Ian G. Taylor
#' @export
#' @family run functions
#' @examples
#' \dontrun{
#' # A theoretical example if "old_model" was present
#' # but expect an error
#' copy_SS_inputs(
#'   dir.old = "c:/SS/old_model",
#'   dir.new = "c:/SS/new_model"
#' )
#' # A working example using files stored in {r4ss}
#' copy_SS_inputs(
#'   dir.old = system.file("extdata", "simple_small", package = "r4ss"),
#'   dir.new = "test"
#' )
#' unlink(test, recursive = TRUE)
#' }
#'
copy_SS_inputs <- function(
  dir.old = NULL,
  dir.new = NULL,
  create.dir = TRUE,
  overwrite = FALSE,
  recursive = FALSE,
  use_ss_new = FALSE,
  copy_exe = FALSE,
  copy_par = FALSE,
  dir.exe = NULL,
  verbose = TRUE
) {
  # check to make sure the first input is in the correct format
  if (!is.character(dir.old) | length(dir.old) != 1) {
    stop("Input 'dir.old' should be a character string for a directory")
  }

  # check for presence of old directory
  if (dir.old != "" & !dir.exists(dir.old)) {
    stop("dir.old doesn't exist:", dir.old)
  }
  # check for presence of new directory, and create if requested
  if (!dir.exists(dir.new)) {
    if (create.dir) {
      dir.create(dir.new, recursive = recursive)
    } else {
      stop("'dir.create=FALSE' and dir.new doesn't exist:", dir.new)
    }
  }
  # read starter file to figure out what other inputs are
  starter_file <- file.path(
    dir.old,
    ifelse(use_ss_new, "starter.ss_new", "starter.ss")
  )
  if (file.exists(starter_file)) {
    starter <- SS_readstarter(starter_file, verbose = FALSE)
  } else {
    warning("file not found: ", file.path(starter_file))
    return(invisible(FALSE))
  }

  # check for starter file in new location
  if (!overwrite && file.exists(file.path(dir.new, "starter.ss"))) {
    warning("overwrite = FALSE and starter.ss exists in ", dir.new)
    return(invisible(FALSE))
  }

  if (verbose) {
    message("copying files from\n ", dir.old, "\nto\n ", dir.new)
  }

  results <- rep(NA, 6)
  if (!use_ss_new) {
    # copy original input files
    results[1] <- file.copy(
      from = file.path(dir.old, starter[["ctlfile"]]),
      to = file.path(dir.new, starter[["ctlfile"]]),
      overwrite = overwrite
    )
    results[2] <- file.copy(
      from = file.path(dir.old, starter[["datfile"]]),
      to = file.path(dir.new, starter[["datfile"]]),
      overwrite = overwrite
    )
    results[3] <- file.copy(
      from = file.path(dir.old, "forecast.ss"),
      to = file.path(dir.new, "forecast.ss"),
      overwrite = overwrite
    )
    results[4] <- file.copy(
      from = file.path(dir.old, "starter.ss"),
      to = file.path(dir.new, "starter.ss"),
      overwrite = overwrite
    )
    if (file.exists(file.path(dir.old, "wtatage.ss"))) {
      results[5] <- file.copy(
        from = file.path(dir.old, "wtatage.ss"),
        to = file.path(dir.new, "wtatage.ss"),
        overwrite = overwrite
      )
    }
  } else {
    # copy ss_new files
    results[1] <- file.copy(
      from = file.path(dir.old, "control.ss_new"),
      to = file.path(dir.new, starter[["ctlfile"]]),
      overwrite = overwrite
    )
    # check data new file could be data.ss_new or data_echo.ss_new
    datname <- get_dat_new_name(dir.old)
    results[2] <- file.copy(
      from = file.path(dir.old, datname),
      to = file.path(dir.new, starter[["datfile"]]),
      overwrite = overwrite
    )
    results[3] <- file.copy(
      from = file.path(dir.old, "forecast.ss_new"),
      to = file.path(dir.new, "forecast.ss"),
      overwrite = overwrite
    )
    results[4] <- file.copy(
      from = file.path(dir.old, "starter.ss_new"),
      to = file.path(dir.new, "starter.ss"),
      overwrite = overwrite
    )
    if (file.exists(file.path(dir.old, "wtatage.ss"))) {
      results[5] <- file.copy(
        from = file.path(dir.old, "wtatage.ss_new"),
        to = file.path(dir.new, "wtatage.ss"),
        overwrite = overwrite
      )
    }
  }
  # copy executables(s) if requested
  if (copy_exe) {
    if (is.null(dir.exe)) {
      dir.exe <- dir.old
    }
    # figure out which files are executables
    if (.Platform[["OS.type"]] == "windows") {
      # "exe" column in output from file.info() only on windows
      is.exe <- file.info(dir(dir.exe, full.names = TRUE))[["exe"]]
      exefiles <- dir(dir.exe)[is.exe != "no"]
    } else {
      # Anything without an extension is listed (may need to make this more
      # specific??)
      # exefiles <- list.files(dir.exe, pattern = "^[^.]+$")

      # directories are accidentally being included using line above,
      # so trying a different way to exclude them
      exefiles <- dir(dir.exe)[
        !file.info(dir(dir.exe, full.names = TRUE))[["isdir"]]
      ]
      exefiles <- grep(pattern = "^[^.]+$", x = exefiles, value = TRUE)
      if (verbose) {
        message("Unix binaries are: ", paste0(exefiles, collapse = ", "))
      }
    }
    if (length(exefiles) == 0) {
      warning("No executable files found in ", dir.exe)
    }
    if (length(exefiles) > 1) {
      warning("Copying multiple executable files")
    }
    for (file in exefiles) {
      results[6] <- file.copy(
        from = file.path(dir.exe, file),
        to = file.path(dir.new, file),
        overwrite = overwrite
      )
    }
  }

  # copy par file(s) if requested
  if (copy_par) {
    parfile <- get_par_name(dir.old)
    # if there's no .par file then don't report results for it
    # even if copy_par == TRUE
    if (!is.na(parfile)) {
      results[7] <- file.copy(
        from = file.path(dir.old, parfile),
        to = file.path(dir.new, parfile),
        overwrite = overwrite
      )
    }
  }

  # check for successful copying
  if (all(results, na.rm = TRUE)) {
    if (verbose) {
      message("copying complete")
    }
    return(invisible(TRUE))
  } else {
    if (verbose) {
      if (overwrite) {
        warning("at least 1 file failed to copy")
      } else {
        warning("at least 1 file failed to copy, try 'overwrite = TRUE'")
      }
    }
    return(invisible(FALSE))
  }
}
