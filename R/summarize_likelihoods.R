#' Summarize likelihood tables in output from [SSsummarize]
#'
#' Extract and summarize the likelihood tables from multiple SS models,
#' where the input a list of models as returned by [SSsummarize].
#'
#' @param slist A list returned from [SSsummarize], which can be any length.
#' @param type The likelihood type that you want to summarize. This can only
#' be a single character value. See below for a list of possible values, where
#' the first value, `likelihoods_used` is the default table to be summarized.
#' @return A data frame of likelihoods (rows) by model (columns).
#' @seealso [SSsummarize], which uses this function to get the likelihood values.
summarize_likelihoods <- function(slist,
                                  type = c("likelihoods_used",
                                           "likelihoods_laplace",
                                           "likelihoods_by_fleet")) {
  type <- match.arg(type, several.ok = FALSE)

  if (type == "likelihoods_by_fleet") {
    likelihoods_by_fleet <- purrr::map_df(slist, ~{.x[[type]]},
      .id = "model")
    return(list(likelihoods_by_fleet = likelihoods_by_fleet))
  }
  purrr::map_df(slist, ~{.x[[type]]},
      .id = "id") %>% tibble::rownames_to_column(var = "Label") %>%
  mutate(Label = sub("\\.*\\d+$", "", .data[["Label"]]))

  # todo: fill in information for missing models b/c not all Report.sso
  # files have likelihoods_laplace
  liketemp <- purrr::map_df(slist, ~{.x[[type]]}, .id = "id") %>%
    tibble::rownames_to_column(var = "Label") %>%
    dplyr::mutate(Label = sub("\\.*\\d+$", "", .data[["Label"]]))
  # So we know how to order the final data frame
  likenames <- liketemp[["Label"]][!duplicated(liketemp[["Label"]])]
  likelihoods <- dplyr::select(liketemp, -.data[["lambdas"]]) %>%
    tidyr::spread(key = "id", value = "values") %>%
    dplyr::arrange(match(.data[["Label"]], likenames))
  likelambdas <- dplyr::select(liketemp, -.data[["values"]]) %>%
    tidyr::spread(key = "id", value = "lambdas") %>%
    dplyr::arrange(match(.data[["Label"]], likenames))
  return(list(likelihoods = likelihoods, likelambdas = likelambdas))
}
