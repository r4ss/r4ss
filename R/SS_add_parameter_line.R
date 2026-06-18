#' Add parameter line
#'
#' @description Add an additional parameter line to an existing parameter data frame in a control list. The function will copy an existing line,
#' change the values in columns that the user specifies (e.g., PHASE, INIT), and place the new line below an existing line. Currently the new
#' parameter cannot be in the first row. The user must also name the new row. The name does not have to follow any naming conventions, but it
#' must be unique. Parameters can be added to either style of parameter data frame (short or long).
#'
#' @param par_df Parameter data frame from the r4ss control list that you would like to add an additional parameter line to. The parameter
#' table can by short or long.
#' @param rownum_to_copy Row number to copy from the existing parameter data frame
#' @param rowname_before Name of the row you would like to place the new parameter line after. Matching is done by `grep()` so only a
#'  uniquely identifiable string is necessary, not the full row name.
#' @param newval_df A data frame of values you would like to change from the row that was copied. The only required column is "rowname".
#'  All other column names must match existing column names in `par_df`.
#'
#' @returns A parameter data frame with the new parameter line inserted in the specified location.
#' @author Kiva L. Oken

#' @export
#' @examples
#'
#' inputs <- SS_read(
#' dir = system.file("extdata", "simple_small", package = "r4ss")
#' )
#' # Details for new row
#' newval_df <- data.frame(
#'   rowname = "P_3_FISHERY",
#'   INIT = 5,
#'   PHASE = 4,
#'   Block = 1,
#'   Block_Fxn = 1
#' )
#'
#' size_selex_new <- add_parameter_line(
#'   par_df = inputs[["ctl"]][["size_selex_parms"]],
#'   rownum_to_copy = 1,
#'   rowname_before = "P_2_FISH",
#'   newval_df = newval_df
#' )
#'
#' size_selex_new
#'
SS_add_parameter_line <- function(
  par_df,
  rownum_to_copy = 1,
  rowname_before,
  newval_df = data.frame(rowname = 'new_parameter')
) {
  if (!"rowname" %in% names(newval_df)) {
    stop("Input error: 'newval_df' must contain a column named 'rowname'.")
  }

  if (nrow(newval_df) != 1) {
    stop(paste(
      "Input error: 'newval_df' must have exactly 1 row. Current row count:",
      nrow(newval_df)
    ))
  }

  bad_cols <- setdiff(names(newval_df), c('rowname', names(par_df)))
  if (length(bad_cols) > 1) {
    stop(paste(
      "Input error: 'newval_df' contains columns that do not exist in 'par_df':",
      paste(invalid_cols, collapse = ", ")
    ))
  }

  if (newval_df[['rowname']] %in% rownames(par_df)) {
    stop("New row name must be unique")
  }

  added_par_df <- par_df |>
    tibble::rownames_to_column() |>
    slice(append(
      1:n(),
      rownum_to_copy,
      after = grep(rowname_before, rowname)
    )) |> # create new row in correct location
    mutate(across(
      .cols = !!names(newval_df),
      ~ ifelse(
        row_number() == grep(rowname_before, rowname) + 1, # test if it is the new row
        newval_df[cur_column()],
        .x # if not the new row, don't change anything
      )
    ))
  # rowname column back to actual row names, return result
  rownames(added_par_df) <- added_par_df[['rowname']]
  return(select(added_par_df, -rowname))
}
