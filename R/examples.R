#' Import examplary data
#'
#' optimLanduse comes bundled with exemplary land-use options and indicator values. The
#' files can also be found on your computer in the package folder `inst/extdata`. Aim is to
#' enable quick application of the package and to view the expected structure of the data.
#'
#' This function is adopted from the example function of the
#' readxl pakage (1.3.1-2).
#'
#'
#' @param fileName Name of example file. If `NULL`, a list of all example files will provided.
#' @export
#' @examples
#' exampleData()
#' exampleData("dataset.xlsx")
exampleData <- function(fileName = NULL) {
  if (is.null(fileName)) {
    dir(system.file("extdata", package = "optimLanduse"))
  } else {
    system.file("extdata", fileName, package = "optimLanduse", mustWork = FALSE)
  }
}
