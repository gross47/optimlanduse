#' Import examplary data
#'
#' optimLanduse comes bundled with exemplary land-use options and indicator values. The
#' files can also be found on your computer in the package folder `inst/extdata`. Aim is to
#' enable quick application of the package and to view the expected structure of the data.
#'
#' \emph{database.xlsx} and \emph{databaseShrinked.xlsx} are exerpts grom Claff et al .... tbd. Volker
#'
#' @param fileName Name of example file. See 'details' section for further explanation.
#' @export
#' @examples
#' require(readxl)
#' path <- exampleData()
#' read_xlsx(path, col_names = FALSE)
#' path <- exampleData("database.xlsx")
#' read_xlsx(path, col_names = FALSE)
#' path <- exampleData("databaseShrinked.xlsx")
#' read_xlsx(path, col_names = FALSE)
exampleData <- function(fileName = "database.xlsx") {
  possibleFiles <- dir(system.file("extdata", package = "optimLanduse"))
  if(fileName %in% possibleFiles) {
    system.file("extdata", fileName, package = "optimLanduse", mustWork = FALSE)
  } else {
    warning(paste0(c("Example not found. Possible file names are ", paste(possibleFiles, collapse = ", "), ".")))
  }
}
