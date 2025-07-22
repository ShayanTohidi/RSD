#' A wrapper for computing inefficient and efficient sets
#'
#' @seealso [sd.screen(), asd.screen()]
#'
#' @importFrom dplyr setdiff
#' @importFrom stats setNames
#'
#' @param data A data frame, including variable pairs, distributions, and
#' results of the tests.
#' @param test A string that indicates the name of the test.
#' @param epsilon.threshold A number that indicates the threshold for considering
#' the result to be valid or not for ASD tests.
#' @returns A list of two elements, efficient and inefficient sets.
#'
screen = function(data, test, epsilon.threshold){

  if (test == 'fsd' | test == 'ssd') {
    inefficient.set = sd.screen(data, test)
  } else {
    inefficient.set = asd.screen(data, test, epsilon.threshold)
  }

  variables = unique(append(data$variable1, data$variable2))
  efficient.set = setdiff(variables, inefficient.set)

  result = list(
    setNames(list(inefficient.set), paste0(test, '.inefficient')),
    setNames(list(efficient.set), paste0(test, '.efficient'))
  )

  return(unlist(result, recursive = F))
}

#' Create the inefficient set by an SD rule
#'
#' It uses the test result to create the inefficient set.
#'
#' @details
#' The inefficient set includes all variable names that are dominated by at
#' least one other variable. The domination defines as the test result of the
#' corresponding `test` column.
#'
#' @importFrom dplyr filter distinct pull sym
#' @importFrom magrittr "%>%"
#'
#' @param data A data frame, including variable pairs, distributions, and
#' results of the tests.
#' @param test A string that indicates the name of the test.
#' @returns A character vector as the inefficient set of variables based on the
#' `test` result.
#'
sd.screen = function(data, test){

  sd.inefficient = data %>%
    filter(!!sym(test) == 1) %>%
    distinct(variable2) %>%
    pull(variable2)

  return(sd.inefficient)
}

#' Create the inefficient set by an ASD rule
#'
#' It uses the test result, corresponding epsilon, and the epsilon threshold to
#' create the inefficient set.
#'
#' @details
#' The inefficient set includes all variable names that are dominated by at
#' least one other variable. The domination defines as the test result where
#' the corresponding epsilon is smaller than or equal to epsilon threshold
#' (`epsilon`). If the epsilon is smaller than the threshold, it is assumed that
#' by some relaxation the domination exists because a very small ratio of the
#' decision-makers do not agree with this result.
#'
#' @importFrom dplyr filter distinct pull sym
#' @importFrom magrittr "%>%"
#'
#' @param data A data frame, including variable pairs, distributions, and
#' results of the tests.
#' @param test A string that indicates the name of the test.
#' @param epsilon.threshold A number that indicates the threshold for considering
#' the result to be valid or not.
#' @returns A character vector as the inefficient set of variables based on the
#' `test` result and `epsilon.threshold`.
#'
asd.screen = function(data, test, epsilon.threshold){

  epsilon.name = paste0(test, '.eps')

  asd.inefficient = data %>%
    filter(!!sym(test) == 1 & !!sym(epsilon.name) <= epsilon.threshold) %>%
    distinct(variable2) %>%
    pull(variable2)

  return(asd.inefficient)
}
