##--#####################################################--##
#### Attach portfolio performance and distance to target ####
##--#####################################################--##
# Wed Jan 29 16:19:22 2020 ------------------------------
# Maintainer: Kai Husmann
# Developer: Kai Husmann, Kai Bödecker

#' Attach portfolio performance and distance to target tbd. Kai B.
#'
#' The function calculates and attaches the portfolio performance and distance to target. See Gosling et al. Formula S5 (supplementary).
#' In der Abbildung werden die Beta-Werte nach ihrem Indikator auf der X-Achse gruppiert. Jedes Beta beschreibt den relativen Anteil zum maximal Erreichbaren (dem "target") innerhalb seines Indikators, bei der aktuellen Landnutzungsverteilung und dem gesetzten Unsicherheits-Scenario. Das niedrigste Beta aller Indikatoren (markiert durch die rote, horizontale Linie) garantiert, dass bei einem Worst-Case-Scenario, mindestens dieser Anteil über alle Indikatoren hinweg erreicht wird. Durch die solveScenario() Funktion wird die garantierte Performance maximiert, bzw. die Distanz zum maximal Möglichen minimiert.
#' tbd. translate
#' @param x An optimized optimLanduse object.
#' @return An optimized optimLanduse object with attached portfolio performance.
#' @references Gosling, E., Reith, E., Knoke T., Gerique, A., Paul, C. (2020): Exploring
#' farmer perceptions of agroforestry via multi-objective optimisation: a test application
#' in Eastern Panama. \emph{Agroforestry Systems} \strong{94}. \url{https://doi.org/10.1007/s10457-020-00519-0}
#' @examples
#' require(ggplot2)
#' require(readxl)
#'
#' dat <- read_xlsx(exampleData("databaseShrinked.xlsx"),
#'                  col_names = FALSE)
#' dat <- dataPreparation(dat)
#' init <- initScenario(dat, uValue = 2,
#'                      optimisticRule = "expectation",
#'                      fixDistance = NULL)
#' result <- solveScenario(x = init)
#' performance <- calcDistanceToPerformanceScenario(result)
#'
#' # Visualize the distance
#' ggplot(performance$scenarioTable,
#'        aes(x = indicator,
#'            y = distanceToTargetPerformance,
#'            color = indicator)) +
#' geom_point() +
#' geom_hline(yintercept =
#'            min(result$scenarioTable$distanceToTargetPerformance),
#'           linetype = "dashed", color = "red") +
#' ylim(0, 1)

#' @importFrom utils type.convert

#'@export
calcDistanceToPerformanceScenario <- function(x) {
  # tbd. Umschreiben: den Scenario Table einmal am Anfang in eine Variable schreiben, um das x$ zu vermeiden. Diese 3 Stufen Indizierung wird vom rCheck angekreidet.
  # Oder noch besser: Ganz ohne dpyr

  if(!all(names(x$scenarioTable[, startsWith(names(x$scenarioTable), "adj")]) ==
          paste0("adjSem", names(x$landUse)))) {
    stop ("Error: Unexpected variables in the scenario table.")
  }

  if(!x$status == "optimized") {cat("Error: No optimim found. Did you call solveScenario?")}
  #---------------------------------#
  #### Add portfolio performance ####
  #---------------------------------#

  # See e.g. Gosling et al. Eq. 10

  #rep(averageNomimalIndicatorValue[1 ,], each = dim(scenarioTable)[1])

  x$scenarioTable$portfolioPerformance <- apply(do.call(rbind, replicate(dim(x$scenarioTable)[1],
                                                                         x$landUse[1 ,], simplify = FALSE)) *
                                                  x$scenarioTable[, startsWith(names(x$scenarioTable), "adj")], 1, sum)

  #------------------------------------------#
  #### Add distance to target performance ####
  #------------------------------------------#

  # See. e.g. Gosling et al. Eq. 11
  x$scenarioTable <- x$scenarioTable %>% mutate(distanceToTargetPerformance = 1 - ifelse(direction == "more is better",
                                                                                           ((portfolioPerformance - minAdjSem) / diffAdjSem),
                                                                                           ((maxAdjSem - portfolioPerformance) / diffAdjSem)))

  x$status <- "optimized - information updated" # function will show an Error if it is run twice

  return(x)

}
