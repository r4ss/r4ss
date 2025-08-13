new_recdev_plot <- function(replist, plot = TRUE, verbose = FALSE, ...) {
    # get info on recdevs, filtering for only the most recent years
    # (back to first plus group cohort)
    recdev_info <- replist$recruit |>
        dplyr::select(Yr, dev) |>
        dplyr::filter(
            Yr > replist$endyr - replist$accuage &
                Yr <= replist$endyr + 1
        )

    # numbers at age in final year + 1
    final_info <- replist$natage |>
        dplyr::filter(Time == replist$endyr + 1 & Sex == 1) |>
        dplyr::select(paste(0:replist$accuage)) |>
        colSums() |>
        data.frame()
    names(final_info) <- "number"
    # add info on maturity and fecundity from the table of
    # biology in final year of the model
    final_info <- final_info |>
        dplyr::mutate(
            age = final_info |> rownames() |> as.numeric(),
            cohort = replist$endyr + 1 - age,
            mat_x_fecund = replist$endgrowth |>
                dplyr::filter(Sex == 1) |>
                dplyr::pull("Mat*Fecund"),
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
    recdev_info_plot <- recdev_info |>
        dplyr::mutate(
            variable = "Recruitment deviation",
            value = dev,
            x = Yr
        ) |>
        dplyr::select(x, value, variable)
    yrange <- c(-1.05, 1.05) * max(abs(recdev_info$dev))

    final_info_long_plot <- final_info_long |>
        dplyr::rename(x = cohort) |>
        dplyr::select(x, value, variable)

    plot_data <- dplyr::bind_rows(recdev_info_plot, final_info_long_plot) |>
        dplyr::arrange(x)

    unit <- ifelse(
        replist$SpawnOutputUnits == "biomass",
        "Weight",
        "Fecundity"
    )

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
            glue::glue("Numbers x Maturity x {unit}"),
            "Numbers",
            glue::glue("Maturity x {unit}")
        )
    )

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
                    variable == "Numbers x Maturity x Fecundity"
                )$value
            )
        ) +
        ggplot2::scale_size_continuous(
            name = "Relative cohort contribution",
            range = c(0, 5)
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
            x = "Year",
            y = "Cohort contribution relative to the maximum",
            title = glue::glue(
                "Recruitment deviations and relative cohort contributions to the {replist$endyr + 1} spawning output"
            )
        ) +
        ggplot2::theme_minimal()

    print(p)
}
