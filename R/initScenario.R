##--#####################################################--##
#### Transform the input table in an optimLanduse object ####
##--#####################################################--##

# Fri Jan 24 23:53:50 2020 ------------------------------

#' Initialize the robust optimization
#'
#' The function is used to initialize a \emph{optimLanduse} S3 object from the formated
#' coefficients table.
#'
#' The expected format is explained in the example on
#'  \href{https://gitlab.gwdg.de/forest_economics_goettingen/optimlanduse}{GitLab}.
#'  Usage of \code{\link{dataPreparation}} is recommended to ensure that
#'  the format requirements are met.
#'
#'  Aim of the separation of the initialization and the optimization is to save
#'  calculation time. The separated function calls allow the user to perform multiple
#'  optimization-runs from one initialized object. This could save time in batch
#'  applications.

#'
#' @param coefTable Coefficient table in the specific optimLanduse format.
#' @param uValue u Value.
#' @param optimisticRule Either \emph{expectation} or \emph{uncertaintyAdjustedExpectation}.
#' It indicates whether the optimistic outcomes of an indicator are directly
#' reflected by the expectation or if the indicator is adjusted by expectation +
#' uncertainty in case "more is better", expectation - uncertainty respectively when "less is better".
#' @param fixDistance tbd. Kai B.
#' @return An initialized optimLanduse S3 object ready for optimization.
#' @examples
#' require(readxl)
#' dat <- read_xlsx(exampleData("databaseShrinked.xlsx"), col_names = FALSE)
#' dat <- dataPreparation(dat)
#' init <- initScenario(dat, uValue = 2, optimisticRule = "expectation", fixDistance = NULL)



#' @import dplyr
#' @import tidyr
#' @importFrom stats setNames
#'
#' @export
initScenario <- function(coefTable,  uValue = 3, optimisticRule = "expectation", fixDistance = NULL) {

  #-----------------------------------------#
  #### Check the format of the coefTable ####
  #-----------------------------------------#

  if (!all(c("indicator", "direction", "landUse", "indicatorValue", "indicatorUncertainty") %in% names(coefTable))) {
    stop ("At least one necessary variable for the optimization is not available. Are the requirements of the data structure met? Check the variable names.")
  }

  indicatorNames <- as.character(unique(coefTable$indicator))

  # all(indicatorNamesCheck %in% coefTable$indicator[coefTable$landUse == "Forest"]) # useless
  testLandUseIndicators <- function (x) {
    all(indicatorNames %in% x)
  }

  if (!coefTable %>% group_by(landUse) %>% summarise(checkLanduse = testLandUseIndicators(indicator)) %>% pull(checkLanduse) %>% all()) {
    stop ("At least one indicator is not available for at least one land-use option.")
  }
  if (!length(indicatorNames) * length(unique(coefTable$landUse)) == nrow(coefTable)) {
    stop ("The indicator names are not unique. Have you assigned an indicator name twice?")
  }

  #----------------------------#
  #### Initialise the table ####
  #----------------------------#

  landUse <- as.character(unique(coefTable$landUse))

  expandList <- list()
  expandList[landUse] <- list(c("High", "Low"))

  expandMatrix1 <- as.matrix(expand.grid(expandList, stringsAsFactors = FALSE))
  expandMatrix2 <- do.call(rbind, replicate(length(indicatorNames), expandMatrix1, simplify = FALSE))
  scenarioTable <- tibble(indicator = rep(indicatorNames, each = dim(expandMatrix1)[1])) %>%
    bind_cols(as_tibble(expandMatrix2))
  # scenarioTable <- tibble(indicator = rep(indicatorNames, each = dim(expandMatrix1)[1])) %>%
  #   left_join(indicatorNames, by = "indicator") %>% bind_cols(as_tibble(expandMatrix2))
  # Alter Version. Evtl relevant bei Fehlersuche. Ich weiß nicht mehr was ich mir bei dem left join gedacht habe.
  # tbd. Tidy raus
  # scenarioTable <- scenarioTable %>% rename_at(.vars = vars(!!landUse[1] : !!landUse[length(landUse)]),
  #                                              .funs = funs(paste0("outcome", .))) #.funs deprecated

  names(scenarioTable)[names(scenarioTable) %in% landUse] <- paste0("outcome",names(scenarioTable)[names(scenarioTable) %in% landUse])

  #--------------------#
  ## Attach direction ##
  #--------------------#

  scenarioTableTemp1 <- scenarioTable
  scenarioTable <- merge(scenarioTable, unique(coefTable[, c("indicator","direction")]), by = "indicator")
  if(!dim(scenarioTableTemp1)[1] == dim(scenarioTable)[1]) {cat("Error: Direction mising or wrong.")}

  #---------------------------------------------#
  ## Attach indicator values and uncertainties ##
  #---------------------------------------------#

  scenarioTableTemp2 <- scenarioTable

  # Das muss noch umgeschrieben werden, da funs "deprecated" ist: Am besten ganz ohne tidy ...

  spread1 <- coefTable %>% select(-indicatorUncertainty) %>% spread(key = landUse, value = indicatorValue)
  names(spread1)[names(spread1) %in% eval(landUse)] <- paste0("mean", names(spread1)[names(spread1) %in% eval(landUse)])

  spread2 <- coefTable %>% select(-indicatorValue) %>% spread(key = landUse, value = indicatorUncertainty)
  names(spread2)[names(spread2) %in% eval(landUse)] <- paste0("sem", names(spread2)[names(spread2) %in% eval(landUse)])

  for(i in landUse) {
    byIndicator <- c("indicator")
    names(byIndicator) <- "indicator"
    scenarioTable <- left_join(scenarioTable, spread1[, c("indicator", paste0("mean", i))], by = byIndicator)
    scenarioTable <- left_join(scenarioTable, spread2[, c("indicator", paste0("sem", i))], by = byIndicator)
  }

  # scenarioTable <- scenarioTable %>% select(-contains("mean"), everything()) # Order the variables, such that the means and uncertainties follow in direct succession
  scenarioTable <- scenarioTable %>% select(-contains("sem"), everything()) # Alternatively, but slower, a second loop would be suitable

  if(!dim(scenarioTableTemp1)[1] == dim(scenarioTable)[1]) {cat("Error: Attaching expectation or uncertainty failed.")}

  #--------------------------------------------#
  ## Calculate indicator uncertainty adjusted ##
  #--------------------------------------------#

  scenarioTableTemp3 <- scenarioTable


  newColumnNames <- paste0("adjSem", landUse)
  scenarioTable[, newColumnNames] <- NA # Initialise empty

  for(i in landUse) {
    # Ugly. But fast and less error-prone
    scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
      scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] +
      scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * uValue

    scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "Low", paste0("adjSem", i)] <-
      scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "Low", paste0("mean", i)] -
      scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "Low", paste0("sem", i)] * uValue

    if(optimisticRule == "uncertaintyAdjustedExpectation") {
      scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
        scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] -
        scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * uValue

      scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
        scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)] +
        scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("sem", i)] * uValue
    }
    if(optimisticRule == "expectation") {
      scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
        scenarioTable[scenarioTable$direction == "less is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]

      scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("adjSem", i)] <-
        scenarioTable[scenarioTable$direction == "more is better" & scenarioTable[, paste0("outcome", i)] == "High", paste0("mean", i)]
    }
  }

  if(!optimisticRule %in% c("uncertaintyAdjustedExpectation", "expectation")) {cat("optimisticRule must be uncertaintyAdjustedExpectation or expectation")}
  if(!dim(scenarioTableTemp3)[1] == dim(scenarioTable)[1] | any(is.na(scenarioTable))) {cat("Error: Calculation of adjusted uncertainty.")}

  #--------------------------#
  ## calculate Min Max Diff ##
  #--------------------------#
  if(is.null(fixDistance)){
    scenarioTable[, c("minAdjSem", "maxAdjSem", "diffAdjSem")] <-
      apply(scenarioTable[, startsWith(names(scenarioTable), "adjSem")], 1,
            function(x) {c(min(x), max(x), (max(x) - min(x)))}) %>% t()
  } else if (length(fixDistance) == dim(scenarioTable)[1]) {
    scenarioTable[, c("minAdjSem", "maxAdjSem")] <-
    apply(scenarioTable[, startsWith(names(scenarioTable), "adjSem")], 1,
          function(x) {c(min(x), max(x))}) %>% t()
    scenarioTable$diffAdjSem <- fixDistance
  } else {stop("The dimension of the fixed distance does not fit the dimension of the scenario table.")}


  #-------------------------------------------------------------#
  ## Define the coefficients for the linear objective function ##
  #-------------------------------------------------------------#

  #and the restrictions. (Simplify the scenario to a row problem)
  coefObjective <- defineObjectiveCoefficients(scenarioTable)

  #-------------------------------------#
  #### Define the constraints matrix ####
  #-------------------------------------#

  constraintCoefficients <- defineConstraintCoefficients(scenarioTable)

  retList <- list(scenarioSettings = data.frame(uValue = uValue,
                              optimisticRule = optimisticRule, stringsAsFactors = FALSE),
                  scenarioTable = scenarioTable,
                  coefObjective = coefObjective,
                  coefConstraint = constraintCoefficients,
                  distance = scenarioTable$diffAdjSem,
                  status = "initialized",
                  beta = NA,
                  landUse = setNames(data.frame(matrix(rep(NA, length(landUse)), ncol = length(landUse), nrow = 1)), landUse),
                  optimDetails = list()
)
  class(retList) <- "optimLanduse"
  return(retList)
}


