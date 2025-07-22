screen = function(data, test, epsilon){

  variables = unique(append(data$variable1, data$variable2))

  if (test == 'fsd' | test == 'ssd') {
    inefficient.set = sd.screen(data, variables, test)
  } else {
    eps.name = paste0(test, '.eps')
    inefficient.set = asd.screen(data, variables, test, epsilon, eps.name)
  }

  efficient.set = setdiff(variables, inefficient.set)

  result = list(
    setNames(list(inefficient.set), paste0(test, '.inefficient')),
    setNames(list(efficient.set), paste0(test, '.efficient'))
  )

  return(unlist(result, recursive = F))
}

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
#' @param epsilon A number that indicates the threshold for considering the
#' result valid or not.
#' @returns A character vector.
#'
asd.screen = function(data, test, epsilon.threshold){

  epsilon.name = paste0(test, '.eps')

  asd.inefficient = data %>%
    filter(!!sym(test) == 1 & !!sym(epsilon.name) <= epsilon.threshold) %>%
    distinct(variable2) %>%
    pull(variable2)

  return(asd.inefficient)
}
