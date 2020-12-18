#' @param file Filename either with full path or relative to working directory.
#' <% if (exists('file_t') && file_t == "posteriors") {%>
#'   Contents of the file that is referenced here should contain posterior samples
#'   for nuisance parameters, e.g., posteriors.sso or
#'   something written by \code{\link{SSgetMCMC}}.
#' <%}%>
#' <% if (exists('file_t') && file_t == "write.table") {%>
#'   See \code{\link[utils]{write.table}} for more information because a file,
#'   a connection, or \code{""} are allowed.
#' <%}%>
#' <% if (!exists('file_t')) {%>
#'   See the formal arguments for a possible default filename.
#' <%}%>
