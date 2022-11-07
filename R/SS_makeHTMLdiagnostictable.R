#' Make html diagnostic tables
#'
#' Creates html tables that show diagnostic outputs, including status
#' checks, gradients, and correlations.
#'
#' @template replist
#' @template plotdir
#' @param gradmax the largest gradient value for estimated parameter
#' @param ncor number of rows in tables of correlations
#' @param cormax threshold for highlighting high correlations
#' @param cormin threshold for highlighting low correlations
#'
#' @return a three-element vector; the first element is the name of the html table file, the second is the table caption, and the third is the category of output type
#' @author Christine Stawitz
#' @export
#' @seealso [SS_plots()], [SS_output()], [SS_html()]
SS_makeHTMLdiagnostictable <- function(replist,
                                       plotdir = NULL,
                                       gradmax = 0.001,
                                       ncor = 50,
                                       cormax = 0.95,
                                       cormin = 0.01) {
  # Filter out parameters with NA gradients
  parchecks <- replist[["estimated_non_dev_parameters"]]

  # convert rownames to columns to avoid Section 508 issue
  parchecks <- data.frame(Parameter = row.names(parchecks), parchecks)

  # set default directory (following convention in other plot functions,
  # not bothering to create "plots" subfolder if called indepenently of
  # SS_plots)
  if (is.null(plotdir)) {
    plotdir <- replist[["inputs"]][["dir"]]
  }

  # Filter out columns related to priors if there aren't any non-uniform priors
  if (all(parchecks[["Pr_type"]] == "No_prior")) {
    parchecks[["Prior"]] <- NULL
    parchecks[["Pr_SD"]] <- NULL
    parchecks[["Pr_Like"]] <- NULL
  }

  # Highlight high gradients
  if (!is.null(parchecks[["Gradient"]]) &&
    all(!is.nan(parchecks[["Gradient"]]))) {
    parchecks <- parchecks[!is.na(parchecks[["Gradient"]]), ]
    parchecks[["Gradient"]] <-
      kableExtra::cell_spec(parchecks[["Gradient"]],
        "html",
        color = ifelse(abs(parchecks[["Gradient"]]) >
          gradmax, "darkred", "black")
      )
  }
  # Format table with parameter checks so high gradients or parameters
  # on bounds are shown in red
  parchecks[["Afterbound"]] <-
    kableExtra::cell_spec(parchecks[["Afterbound"]],
      "html",
      color = ifelse(parchecks[["Afterbound"]] == "OK", "black", "darkred")
    )
  parchecks[["Status"]] <-
    kableExtra::cell_spec(parchecks[["Status"]],
      "html",
      color = ifelse(parchecks[["Status"]] == "OK", "black", "darkred")
    )

  # print 800px tall with scroll bar if more than 40 rows
  table_height <- ifelse(nrow(parchecks) > 40,
    "500px",
    paste0(200 + nrow(parchecks) * 20, "px")
  )

  # Write out table
  parchecks <- kableExtra::kable(parchecks,
    format = "html",
    escape = FALSE,
    row.names = FALSE
  ) %>%
    kableExtra::kable_styling() %>%
    kableExtra::scroll_box(width = "100%", height = table_height)

  # filename and cpation will be vectors with values for each table
  filename <- caption <- NULL
  filename <- c(filename, "parameterchecks.html")

  write(parchecks,
    file = file.path(plotdir, filename[1])
  )
  caption <- c(caption, paste(
    "<b>Estimated parameters (excluding deviation parameters)</b><br>",
    "Any parameter with a gradient value with an absolute value above",
    gradmax,
    "(for SS 3.30 models) or a parameter on bounds is colored in red."
  ))

  # calculate parameter pairs with high correlations
  if (!is.null(replist[["CoVar"]]) && replist[["N_estimated_parameters"]] > 1) {
    # names of active parameters
    activepars <- replist[["parameters"]] %>%
      dplyr::filter(!is.na(Active_Cnt)) %>%
      dplyr::pull(Label)
    # Remove forecast recdevs which are typically all zero and
    # uncorrelated with everything else
    activepars <- activepars[!grepl("ForeRecr", activepars)]
    # filter covar.sso file for pairs of active parameters
    # (ignoring forecast recruitment deviations)
    cor_table <- dplyr::filter(
      replist[["CoVar"]],
      all.i != all.j &
        Par..i == "Par" &
        Par..j == "Par" &
        label.i %in% activepars &
        label.j %in% activepars
    )

    # create table of parameter pairs with highest correlations
    high_cor_table <- cor_table %>%
      dplyr::select(label.i, label.j, corr) %>%
      dplyr::arrange(dplyr::desc(abs(corr))) %>%
      head(ncor)

    high_cor_table[["corr"]] <-
      kableExtra::cell_spec(high_cor_table[["corr"]],
        "html",
        color = ifelse(abs(high_cor_table[["corr"]]) >
          cormax, "darkred", "black")
      )
    high_cor_table <- kableExtra::kable(high_cor_table,
      format = "html",
      escape = FALSE,
      row.names = FALSE
    ) %>%
      kableExtra::kable_styling() %>%
      kableExtra::scroll_box(width = "100%", height = "200px")

    # save table to file
    filename <- c(filename, "correlationcheck.html")
    write(high_cor_table,
      file = file.path(plotdir, "correlationcheck.html")
    )
    # create caption
    caption <- c(caption, paste(
      "<b>Highly correlated parameter pairs</b><br>",
      "Pairs of parameters with the ", ncor, " highest correlations,",
      "sorted by absolute value of the correlation. Correlations",
      "above", cormax, "(in absolute value) are colored in red.",
      "These parameters may be confounded or may cause convergence issues."
    ))
  }

  if (!is.null(replist[["CoVar"]]) && replist[["N_estimated_parameters"]] > 1) {
    # Create and format low correlations table
    low_cor_table <- data.frame(Parameter = activepars, max_correlation = NA)
    for (par in activepars) {
      correlations <-
        c(
          abs(cor_table[["corr"]][cor_table[["label.i"]] == par]),
          abs(cor_table[["corr"]][cor_table[["label.j"]] == par])
        )
      if (length(correlations) > 0) {
        low_cor_table[["max_correlation"]][which(low_cor_table[["Parameter"]] == par)] <-
          max(correlations)
      }
    }
    low_cor_table <- low_cor_table %>%
      dplyr::arrange(abs(max_correlation)) %>%
      head(ncor)

    low_cor_table[["max_correlation"]] <-
      kableExtra::cell_spec(low_cor_table[["max_correlation"]],
        "html",
        color = ifelse(abs(low_cor_table[["max_correlation"]]) <
          cormin, "darkred", "black")
      )
    low_cor_table <- kableExtra::kable(low_cor_table,
      format = "html",
      escape = FALSE,
      row.names = FALSE
    ) %>%
      kableExtra::kable_styling() %>%
      kableExtra::scroll_box(width = "100%", height = "200px")

    # save table to file
    filename <- c(filename, "lowcorrelationcheck.html")
    write(low_cor_table,
      file = file.path(plotdir, "lowcorrelationcheck.html")
    )
    # create caption
    caption <- c(caption, paste(
      "<b>Parameters with low correlations</b><br>",
      "Estimated parameters that are the least correlated with all other",
      "parameters. The correlation value represents the maximum correlation",
      "between the parameter in question and all other estimated parameters.",
      "Uncorrelated parameters may not be playing a significant role in",
      "the model. Maximum correlations below", cormin, "are colored in red."
    ))
  }
  # Create data frame of filenames and captions to add to PlotInfoTable
  # to be used by SS_html()
  outtable_df <- data.frame(
    "file" = filename,
    "caption" = caption,
    "alt_text" = NA,
    "category" = rep("DiagnosticTables", length(filename))
  )
  return(outtable_df)
}
