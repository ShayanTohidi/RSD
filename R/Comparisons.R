#' Comparing all pairs by all rules, and finding the sets
#'
#' It creates a data frame of unique unordered pairs of variables, collects
#' corresponding distributions, tests all SD and ASD rules on every pairs, and
#' finds the efficient and inefficient sets for all those rules.
#'
#' @details
#' If the type of each input parameter is incorrect, it will raise an error.
#'
#' The output is a list that contains 6 elements: `data` is a data frame including
#' all unique unordered pairs of variables, corresponding distributions, result
#' of each rule, and epsilon of ASD rule. The rest 5 elements are lists as well,
#' corresponding to each rule that includes the efficient and inefficient sets.
#'
#' The `data` is a nested data frame, and stores the distribution of each variable
#' in a single cell. The distribution contains two columns: probability and
#' outcome.
#'
#' @seealso [compare.paired.distributions(), screen.by.sd(), screen.by.asd()]
#'
#' @param variable A character vector containing the name of all variables.
#' @param probability A numeric vector containing the probability for each
#' variable to achieve a particular outcome.
#' @param outcome A numeric vector containing the outcome values.
#' @param afsd.epsilon.threshold A number that shows the upper limit for epsilon
#' of afsd rule. If the epsilon is smaller than or equal to this value, the result
#' is accepted. The default value is `0.1`.
#' @param assd.ll.epsilon.threshold A number that shows the upper limit for epsilon
#' of assd.ll rule. If the epsilon is smaller than or equal to this value, the result
#' is accepted. The default value is `0.1`.
#' @param assd.ths.epsilon.threshold A number that shows the upper limit for epsilon
#' of assd.ths rule. If the epsilon is smaller than or equal to this value, the result
#' is accepted. The default value is `0.1`.
#' @param include.details A logical that affects the output data to contain
#' distributions. The default value is `TRUE`.
#' @returns A list of 6 elements, including a data frame and the efficient and
#' inefficient sets of all rules.
#' @examples
#' result = compare.all(data_ex$gen, rep(1/29,377), data_ex$yield)
#'
#' @export
compare.all = function(variable, probability, outcome, afsd.epsilon.threshold = 0.1,
                       assd.ll.epsilon.threshold = 0.1,
                       assd.ths.epsilon.threshold = 0.1, include.details = TRUE){

  if (!(is.numeric(afsd.epsilon.threshold) & is.numeric(assd.ll.epsilon.threshold) &
        is.numeric(assd.ths.epsilon.threshold))){
    stop("Expected types: afsd.epsilon.threshold, assd.ll.epsilon.threshold, and
         assd.ths.epsilon.threshold: (numeric)")
  }

  data = compare.paired.distributions(variable, probability, outcome, include.details)

  sd = screen.by.sd(data)
  asd = screen.by.asd(data, afsd.epsilon.threshold, assd.ll.epsilon.threshold,
                      assd.ths.epsilon.threshold)

  return(list(data = data, fsd.sets = sd$fsd.sets, afsd.sets = asd$afsd.sets,
              ssd.sets = sd$ssd.sets, assd.ll.sets = asd$assd.ll.sets,
              assd.ths.sets = asd$assd.ths.sets))
}

#' Creating paired distributions and execute all SD and ASD rules on them
#'
#' It creates a data frame including all unordered pairs of the variables, where
#' the first variable has higher or equal mean outcome. Then, all available SD
#' and ASD rules are tested on them.
#'
#' @details
#' If the type of each input parameter is incorrect, it will raise an error.
#'
#' The length of the input parameters `variable`, `probability`, and `outcome`
#' must be equal, otherwise it will raise an error.
#'
#' @seealso [create.paired.distributions(), sd.asd.test.all()]
#'
#' @param variable A character vector containing the name of all variables.
#' @param probability A numeric vector containing the probability for each
#' variable to achieve a particular outcome.
#' @param outcome A numeric vector containing the outcome values.
#' @param include.details A logical that affects the output data to contain
#' distributions. The default value is `TRUE`.
#' @returns A data frame, including all distribution pairs and the results of
#' the tests.
#' @examples
#' result = compare.paired.distributions(data_ex$gen, rep(1/29,377), data_ex$yield)
#'
#' @export
compare.paired.distributions = function(variable, probability, outcome,
                                      include.details = TRUE){

  if (!(is.character(variable) & is.numeric(probability) & is.numeric(outcome) &
        is.logical(include.details))){
    stop("Expected types: variable (character); probability, outcome,
         include.details: (logical)")
  }

  len = length(variable)
  if (length(probability) != len | length(outcome) != len) {
    stop("All input vectors must have the same length.")
  }

  paired.dists = create.paired.distributions(variable, probability, outcome)
  data = sd.asd.test.all(paired.dists, include.details)

  return(data)
}

#' Screening by all SD rules
#'
#' It checks all available SD rules (i.e. fsd and ssd) based on their results,
#' and creates the corresponding efficient and inefficient sets.
#'
#' @details
#' The input parameter `data` must contain columns corresponding to SD rules.
#' These columns contain the index of the dominated variable, where 1 or 2 means
#' the first or the second variable dominates, and 0 means the domination does
#' not exist. The best practice is to use the output of `compare.paired.distributions`.
#'
#' @seealso [screen()]
#'
#' @param data A data frame, including the results of all SD rules.
#' @returns A list, including two elements corresponding to each SD rule.
#' @examples
#' data = compare.paired.distributions(data_ex$gen, rep(1/29,377), data_ex$yield)
#' sd.sets = screen.by.sd(data)
#'
#' @export
screen.by.sd = function(data){

  fsd = screen(data, 'fsd', 0)
  ssd = screen(data, 'ssd', 0)

  return(list(fsd.sets = fsd, ssd.sets = ssd))
}

screen.by.asd = function(data, afsd.epsilon.threshold = 0.1,
                         assd.ll.epsilon.threshold = 0.1,
                         assd.ths.epsilon.threshold = 0.1){

  afsd = screen(data, 'afsd', afsd.epsilon.threshold)
  assd.ll = screen(data, 'assd.ll', assd.ll.epsilon.threshold)
  assd.ths = screen(data, 'assd.ths', assd.ths.epsilon.threshold)

  return(list(afsd.sets = afsd, assd.ll.sets = assd.ll, assd.ths.sets = assd.ths))
}
