#' Download SS3 test models
#'  
#' Function to download the models folder within the 
#' nmfs-stock-synthesis/test-models repsitory
#' @param dir The folder where the models subfolder should be written to.
#' @param branch The name of the github branch to download.
#' @template overwrite
#' @return Invisibly return a value revealing whether the files were copied 
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
    stop("dir ", dir, " does not exist")
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
      "\nand overwrite is FALSE, so no new files will be written." )
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