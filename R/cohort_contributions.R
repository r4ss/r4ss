#' Plot relative cohort contributions to spawning output in ending year + 1
#'
#' Plot shows recruitment deviations and relative cohort contributions
#' (the product of numbers, maturity, and fecundity) to the spawning output in
#' the final year + 1 of the model. Figure suggested by Andre Punt at a 2025
#' PFMC Groundfish Subcommittee meeting.
#'
#' @template replist
#' @return ggplot object
#' @param min_contribution Minimum relative contribution to include in the plot
#'  (this impacts the range of years/cohorts shown).
#' @author Ian G. Taylor
#' @examples
#' \dontrun{
#' }
#' @export
cohort_contributions <- function(replist, min_contribution = 0.01, option = 1) {
    if (replist$SS_versionNumeric < 3.30) {
        cli::cli_abort(
            "This function does not work with SS3 version prior to 3.30."
        )
    }

    # get info on recdevs, filtering for only the most recent years
    # (back to first plus group cohort)
    recdev_info <- replist$recruit |>
        dplyr::filter(
            Yr > replist$endyr - replist$accuage &
                Yr <= replist$endyr + 1
        ) |>
        dplyr::mutate(
            variable = "Recruitment deviation",
            value = dev,
            x = Yr
        ) |>
        dplyr::select(x, value, variable)

    # numbers at age in final year + 1
    final_info <- replist$natage |>
        dplyr::filter(Time == replist$endyr + 1 & Sex == 1) |>
        dplyr::select(paste(0:replist$accuage)) |>
        colSums() |>
        data.frame()
    names(final_info) <- "number"
    # add info on maturity and fecundity
    if (replist$wtatage_switch) {
        # from wtatage file
        mat_x_fecund <- replist$wtatage |>
            dplyr::filter(year == replist$endyr + 1, sex == 1, fleet == -2) |>
            dplyr::select(paste(0:replist$accuage)) |>
            as.numeric()
    } else {
        # from table of biology in final year of the model
        mat_x_fecund <- replist$endgrowth |>
            dplyr::filter(Sex == 1) |>
            dplyr::pull("Mat*Fecund")
    }

    final_info <- final_info |>
        dplyr::mutate(
            age = final_info |> rownames() |> as.numeric(),
            cohort = replist$endyr + 1 - age,
            mat_x_fecund = mat_x_fecund,
            number_x_mat_x_fecund = number * mat_x_fecund,
            rel_number = number / max(number),
            rel_mat_x_fecund = mat_x_fecund / max(mat_x_fecund),
            rel_number_x_mat_x_fecund = number_x_mat_x_fecund /
                max(number_x_mat_x_fecund)
        )

    # make a plot
    # Reshape final_info to long format for faceting
    final_info_long <- final_info |>
        dplyr::select(
            cohort,
            rel_number,
            rel_mat_x_fecund,
            rel_number_x_mat_x_fecund
        ) |>
        tidyr::pivot_longer(
            cols = c(rel_number, rel_mat_x_fecund, rel_number_x_mat_x_fecund),
            names_to = "variable",
            values_to = "value"
        )

    # Combine recruitment deviations and rel_* lines into a single figure with four vertical panels

    # Prepare data for plotting
    final_info_long_plot <- final_info_long |>
        dplyr::rename(x = cohort) |>
        dplyr::select(x, value, variable)

    plot_data <- dplyr::bind_rows(recdev_info, final_info_long_plot) |>
        dplyr::arrange(x)

    unit <- if (
        !is.null(replist$SpawnOutputUnits) &&
            replist$SpawnOutputUnits == "biomass"
    ) {
        "Weight"
    } else {
        "Fecundity"
    }
    contribution_label <- glue::glue(
        "Relative cohort contribution (Numbers x Maturity x {unit})"
    )
    mat_label <- glue::glue("Maturity x {unit}")

    # Set factor levels for desired panel order
    plot_data$variable <- factor(
        plot_data$variable,
        levels = c(
            "Recruitment deviation",
            "rel_number_x_mat_x_fecund",
            "rel_number",
            "rel_mat_x_fecund"
        ),
        labels = c(
            "Recruitment deviation",
            contribution_label,
            "Numbers",
            mat_label
        )
    )

    # choose a minimum year to include based on cohort contributions
    # above the threshold (default 0.01)
    min_year <- plot_data |>
        dplyr::filter(
            variable == contribution_label,
            value > min_contribution
        ) |>
        dplyr::pull(x) |>
        min()
    # don't include years outside the time series
    if (min_year < replist$startyr) {
        min_year <- replist$startyr
    }
    plot_data <- plot_data |>
        dplyr::filter(x >= min_year)

    if (option == 1) {
        # Plot all panels in one figure
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = value)) +
            ggplot2::geom_point(
                data = dplyr::filter(
                    plot_data,
                    variable == "Recruitment deviation"
                ),
                ggplot2::aes(
                    size = dplyr::filter(
                        plot_data,
                        variable == contribution_label
                    )$value
                )
            ) +
            scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
            ggplot2::scale_size_continuous(
                name = "",
                range = c(0, 5),
                guide = "none"
            ) +
            ggplot2::facet_wrap(~variable, ncol = 1, scales = "free_y") +
            ggplot2::geom_col(
                data = dplyr::filter(
                    plot_data,
                    variable != "Recruitment deviation"
                ),
                fill = "blue4",
                alpha = 0.7
            ) +
            ggplot2::geom_hline(yintercept = 0, color = "black") +
            ggplot2::labs(
                x = "Year or Cohort",
                y = "Value relative to the maximum"
            ) +
            ggplot2::theme_minimal()
    }
    if (option == 2) {
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = value)) +
            ggplot2::geom_point(
                data = dplyr::filter(
                    plot_data,
                    variable == "Recruitment deviation"
                ),
                ggplot2::aes(
                    size = dplyr::filter(
                        plot_data,
                        variable == contribution_label
                    )$value
                )
            ) +
            scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
            ggplot2::scale_size_continuous(
                name = glue::glue("Relative cohort contribution\n(Numbers x Maturity x {unit})"),
                range = c(0, 10)
            ) +
            ggplot2::labs(
                x = "Year",
                y = "Recruitment deviation"
            ) +
            ggplot2::geom_hline(yintercept = 0, color = "black") +
            ggplot2::theme_minimal()
    }
    if (option == 3) {
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = value)) +
            ggplot2::geom_point(
                size = 3,
                data = dplyr::filter(
                    plot_data,
                    variable == "Recruitment deviation"
                ),
                ggplot2::aes(
                    color = dplyr::filter(
                        plot_data,
                        variable == mat_label
                    )$value
                )
            ) +
            scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
            ggplot2::scale_color_continuous(
                name = glue::glue("Maturity x {tolower(unit)}\nof cohort in {replist$endyr + 1}\nrelative to the max"),
                low = "lightblue",
                high = "darkblue"
            ) +
            ggplot2::labs(
                x = "Year",
                y = "Recruitment deviation"
            ) +
            ggplot2::geom_hline(yintercept = 0, color = "black") +
            ggplot2::theme_minimal()
    }
    return(p)
}
