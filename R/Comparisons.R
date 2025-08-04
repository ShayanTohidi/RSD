#' Comparing all pairs by all rules, and finding the sets
#'
#' It creates a data frame of unique unordered pairs of variables, collects
#' corresponding distributions, tests all SD and ASD rules on every pairs, and
#' finds the efficient and inefficient sets for all those rules.
#'
#' @details
#' If the type of each input parameter is incorrect, it will raise an error.
#'
#' The length of the input parameters `variable`, `probability`, and `outcome`
#' must be equal, otherwise it will raise an error.
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
#' @seealso [create.paired.distributions(), sd.asd.test.all(), screen()]
#'
#' @param variable A character vector containing the name of all variables.
#' @param probability A numeric vector containing the probability for each
#' variable to achieve a particular outcome.
#' @param outcome A numeric vector containing the outcome values.
#' @param afsd.epsilon.threshold A number that shows the upper limit for epsilon
#' of afsd rule. If the epsilon is smaller than or equal to this value, the result
#' is accepted.
#' @param assd.ll.epsilon.threshold A number that shows the upper limit for epsilon
#' of assd.ll rule. If the epsilon is smaller than or equal to this value, the result
#' is accepted.
#' @param assd.ths.epsilon.threshold A number that shows the upper limit for epsilon
#' of assd.ths rule. If the epsilon is smaller than or equal to this value, the result
#' is accepted.
#' @param include.details A logical that affects the output data to contain
#' distributions.
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

  return(list(data = data, fsd.sets = sd$fsd, afsd.sets = asd$afsd,
              ssd.sets = sd$ssd, assd.ll.sets = asd$assd.ll,
              assd.ths.sets = asd$assd.ths))
}

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

screen.by.sd = function(data){

  fsd = screen(data, 'fsd', 0)
  ssd = screen(data, 'ssd', 0)

  return(list(fsd = fsd, ssd = ssd))
}

screen.by.asd = function(data, afsd.epsilon.threshold = 0.1,
                         assd.ll.epsilon.threshold = 0.1,
                         assd.ths.epsilon.threshold = 0.1){

  afsd = screen(data, 'afsd', afsd.epsilon.threshold)
  assd.ll = screen(data, 'assd.ll', assd.ll.epsilon.threshold)
  assd.ths = screen(data, 'assd.ths', assd.ths.epsilon.threshold)

  return(list(afsd = afsd, assd.ll = assd.ll, assd.ths = assd.ths))
}
