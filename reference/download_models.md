# Download SS3 test models

Download and unzip the models folder stored on GitHub within the
nmfs-ost/ss3-test-models repository.

## Usage

``` r
download_models(
  dir = file.path("inst", "extdata"),
  branch = "main",
  overwrite = FALSE
)
```

## Arguments

- dir:

  A file path where the downloaded `"models"` subfolder will be written
  to.

- branch:

  A string specifying the desired branch of [nmfs-ost/ss3-test-models
  repository](https://github.com/nmfs-ost/ss3-test-models#test-models)
  that you want to download. The default is `"main"`, which is the
  stable/default branch.

- overwrite:

  A logical value specifying if the existing file(s) should be
  overwritten. The default value is `overwrite = FALSE`.

## Value

Invisibly return a logical revealing whether the files were copied
(TRUE) or not (FALSE). This function is used for its side effects of
downloading SS3 test models.

## References

[nmfs-ost/ss3-test-models
repository](https://github.com/nmfs-ost/ss3-test-models#test-models)

## Author

Kathryn Doering

## Examples

``` r
download_models(dir = getwd())
model_name <- list.files("models") # see the model names
# remove files
unlink(file.path("models"), recursive = TRUE)
```
