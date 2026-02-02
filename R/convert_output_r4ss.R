#' Convert SS3 output from r4ss to stockplotr format
#'
#' Format stock assessment output to a standardized format.
#'
#' @param save_dir File path to save the converted output file.
#'
#' @author Samantha Schiano, Ian G. Taylor
#'
#' @return A reformatted and standardized version of assessment model results
#'         for application in building a stock assessment reports and to easily
#'         adapt results among regional assessments. The resulting object is
#'         simply a transformed and machine readable version of a model output file 
#'         as initially parsed by `r4ss::SS_output()`.
#'         Converted data frame is always returned. It will also be saved if save_dir
#'         is not NULL.
#'
#' @export
#'
#' @examples
#' # read model output into R
#' output <- r4ss::SS_output(system.file("extdata", "simple_small", package = "r4ss"))
#' # convert output and save to working directory
#' output_converted <- output |>
#'   convert_output_r4ss(save_dir = here::here("standard_output.rda"))

convert_output_r4ss <- function(output, save_dir = NULL) {
    # create empty tibble with all possible columns
    out_new <- tibble::tibble(
        label = character(),
        estimate = numeric(),
        year = numeric(),
        fleet = numeric(),
        sex = numeric(),
        area = numeric(),
        growth_pattern = numeric(),
        uncertainty = numeric(),
        module_name = character(),
        uncertainty_label = character(),
        time = numeric(),
        era = character(),
        month = numeric(),
        season = numeric(),
        subseason = numeric(),
        birthseas = numeric(),
        initial = numeric(),
        likelihood = numeric(),
        platoon = numeric(),
        age = numeric(),
        bio_pattern = numeric(),
        settlement = numeric(),
        morph = numeric(),
        type = character(),
        factor = character(),
        part = numeric(),
        kind = character(),
        nsim = numeric(),
        bin = numeric(),
        age_a = numeric(),
        len_bins = numeric(),
        count = numeric(),
        block = numeric()
    )

    # # get list of variable names from stockplotr
    # var_names <- read.csv(
    #     # ttps://raw.githubusercontent.com/nmfs-ost/stockplotr/refs/heads/main/inst/resources/ss3_var_names.csv"
    # ) |>
    #     dplyr::filter(alt_label != "") |> # values not currently used in stockplotr
    #     dplyr::distinct() # filter all duplicated rows

    con_file <- system.file(
        "resources",
        "ss3_var_names.csv",
        package = "stockplotr",
        mustWork = TRUE
    )
    var_names_sheet <- utils::read.csv(con_file, na.strings = "")

    # TODO: incorporate these changes into the stockplotr csv file
    var_names <- var_names |>
        dplyr::mutate(
            alt_label = ifelse(
                label == "Recr_Virgin",
                "recruitment_unfished",
                alt_label
            )
        )
    var_names <- var_names |>
        dplyr::filter(alt_label != "") |> # values not currently used in stockplotr
        dplyr::distinct() # filter all duplicated rows

    # helper function to get things from DERIVED_QUANTITIES
    get_DERIVED_QUANTITIES <- function(output, label_match) {
        output$derived_quants |>
            # extract year (will return NA if not present in label)
            dplyr::mutate(
                year = stringr::str_extract(Label, "[0-9]+$") |> as.numeric(),
                label = stringr::str_remove(Label, "_[0-9]+$"),
                module_name = "DERIVED_QUANTITIES",
                uncertainty_label = "stddev"
            ) |>
            # with year removed, label should exactly match stockplotr labels
            dplyr::filter(label == label_match) |>
            # rename stuff
            dplyr::rename(estimate = Value, uncertainty = StdDev) |>
            # select columns
            dplyr::select(tidyselect::any_of(names(out_new))) |>
            tibble::as_tibble()
    }

    # helper function to get things from DERIVED_QUANTITIES
    get_PARAMETERS <- function(output, label_match) {
        output$parameters |>
            dplyr::mutate(
                module_name = "PARAMETERS",
                uncertainty_label = "se"
            ) |>
            dplyr::rename_with(tolower) |>
            # rename stuff
            dplyr::rename(
                year = yr,
                season = seas,
                uncertainty = se
            ) |>
            tidyr::pivot_longer(
                # TODO: use tidyselect and/or add more variables
                # cols = !tidyselect::any_of(names(out_new)),
                cols = c("obs", "exp"),
                names_to = "label",
                values_to = "estimate"
            ) |>
            # uncertainty values should only apply to the observations
            dplyr::mutate(
                uncertainty = ifelse(label == "obs", uncertainty, NA)
            ) |>
            dplyr::select(tidyselect::any_of(names(out_new))) |>
            tibble::as_tibble()
    }

    # helper function to get things from CATCH
    get_CATCH <- function(output, label_match) {
        output$catch |>
            dplyr::mutate(
                module_name = "CATCH",
                uncertainty_label = "se"
            ) |>
            dplyr::rename_with(tolower) |>
            # rename stuff
            dplyr::rename(
                year = yr,
                season = seas,
                uncertainty = se
            ) |>
            tidyr::pivot_longer(
                # TODO: use tidyselect and/or add more variables
                # cols = !tidyselect::any_of(names(out_new)),
                cols = c("obs", "exp"),
                names_to = "label",
                values_to = "estimate"
            ) |>
            # uncertainty values should only apply to the observations
            dplyr::mutate(
                uncertainty = ifelse(label == "obs", uncertainty, NA)
            ) |>
            dplyr::select(tidyselect::any_of(names(out_new))) |>
            tibble::as_tibble()
    }

    # extract output from derived quantities
    df <- purrr::map_df(
        # get rows of var_names for this module with non-empty alt_label
        var_names |>
            dplyr::filter(
                module_name == "DERIVED_QUANTITIES" & alt_label != ""
            ) |>
            pull(label),
        ~ get_DERIVED_QUANTITIES(output, label_match = .x)
    )

    out_new <- dplyr::bind_rows(out_new, df)

    # subset of var_names associated with CATCH
    df <- purrr::map_df(
        # get rows of var_names for this module with non-empty alt_label
        var_names |>
            dplyr::filter(module_name == "CATCH" & alt_label != "") |>
            pull(label),
        ~ get_CATCH(output, label_match = .x)
    )
    out_new <- dplyr::bind_rows(out_new, df)

    # convert labels from SS3 to new standard
    out_new <- dplyr::left_join(
        out_new,
        var_names,
        by = c("module_name", "label")
    ) |>
        dplyr::mutate(
            label = alt_label,
        ) |>
        dplyr::select(-alt_label) |>
        dplyr::rename(length_bins = len_bins)

    # save output
    if (!is.null(save_dir)) {
        save(out_new, file = save_dir)
    }

    # return output
    return(out_new)
}
