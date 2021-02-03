#' Attach examplary data
#'
#' optimLanduse comes bundled with exemplary data for land-use optimization. The
#' files can also be found on your computer in the package folder `./extdata`. These examples
#' provide some quick applications of the package for demonstration and an example of the expected structure
#' of the data. Consider also the
#' \href{https://gitlab.gwdg.de/forest_economics_goettingen/optimlanduse}{GitLab project page} for
#' exemplary applications of the package.
#'
#' \emph{database.xlsx} and \emph{databaseShrinked.xlsx} are excerpts grom Claff et al .... tbd. Volker
#'
#' @param fileName Name of example file. See 'details' section for further explanation of all provided examples.

#' @return The path to the example file on your computer.
#'
#' @examples
#' require(readxl)
#' path <- exampleData()
#' read_xlsx(path, col_names = FALSE)
#' path <- exampleData("database.xlsx")
#' read_xlsx(path, col_names = FALSE)
#' path <- exampleData("databaseShrinked.xlsx")
#' read_xlsx(path, col_names = FALSE)
#'
#' @export
exampleData <- function(fileName = "database.xlsx") {
  possibleFiles <- dir(system.file("extdata", package = "optimLanduse"))
  if(fileName %in% possibleFiles) {
    system.file("extdata", fileName, package = "optimLanduse", mustWork = FALSE)
  } else {
    warning(paste0(c("Example not found. Possible file names are ", paste(possibleFiles, collapse = ", "), ".")))
  }
}
