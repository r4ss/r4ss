#' Download SS3 test models
#'  
#' Download and unzip the models folder stored on GitHub within the 
#' nmfs-stock-synthesis/test-models repository.
#' @param dir A file path where the downloaded `"models"` subfolder will be written to.
#' @param branch A string specifying the desired branch of
#' [nmfs-stock-synthesis/test-models repository](https://github.com/nmfs-stock-synthesis/test-models#test-models)
#' that you want to download. The default is `"main"`, which is the 
#' stable/default branch.
#' @template overwrite
#' @return Invisibly return a logical revealing whether the files were copied 
#'  (TRUE) or not (FALSE). This function is used for its side effects of
#'  downling SS3 test models.
#' @examples
#'   download_models(dir = getwd())
#'   model_name <- list.files("models") # see the model names
#'   # remove files
#'   unlink(file.path("models"), recursive = TRUE)
#' @author Kathryn Doering
#' @references [nmfs-stock-synthesis/test-models repository](https://github.com/nmfs-stock-synthesis/test-models#test-models)
#' @export
download_models <- function(dir = file.path("inst", "extdata"), 
  branch = "main", overwrite = FALSE) {
  # checks
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  zip_file_path <- file.path(dir, "test-models.zip")
  result <- tryCatch(utils::download.file(
      url = paste0("https://github.com/nmfs-stock-synthesis/test-models/archive/",
        branch, ".zip"),
      destfile = zip_file_path),
       error = function(e) e)
  if("simpleError" %in% class(result)) {
    stop("The test models could not be downloaded. Does the branch exist?")
  }
  list_files <- utils::unzip(list = TRUE, zipfile = zip_file_path)
  save_files <- list_files[grep("/models/", list_files$Name, fixed = TRUE), ]
  utils::unzip(zipfile = zip_file_path, files = save_files[["Name"]], 
    exdir = dir)
  if (dir.exists(file.path(dir, "models")) & overwrite == FALSE) {
    warning("The model directory ", file.path(dir, "models"),  " already exists ",
      "\nand overwrite is FALSE. So, no new files will be written.")
  }
  dir.create(file.path(dir, "models"), showWarnings = FALSE)
  copy_status <- file.copy(from = file.path(dir, paste0("test-models-", branch), "models"), 
            to = file.path(dir), recursive = TRUE, overwrite = overwrite)
  # clean up
  unlink(zip_file_path)
  unlink(file.path(dir, paste0("test-models-", branch)),
    recursive = TRUE)
  invisible(copy_status)
  }
