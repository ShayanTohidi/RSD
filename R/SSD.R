#' Compares random prospects by SSD
#'
#'  It compares two random prospects by the second-order stochastic dominance (SSD),
#'  given the distributions.
#'
#' @details
#' If neither prospect dominates the other, it returns 0.
#'
#'  A prospect dominates when its SSD is below the other one. It means that all
#'  element of the SSD vector must be equal or smaller, and at least one element
#'  should be smaller for the dominant prospect.
#'
#' @seealso [ssd.calc()] for the parameters.
#'
#' @param dists.obj Distributions object.
#' @returns A list. Two numeric vectors of corresponding SSD values, and an
#' integer, indicating of the dominant prospect index.
#' @examples
#' dists = createDistributions(outcome1 = c(1,4,7),
#'                             outcome2 = c(2,3,5),
#'                             prob1 = c(1/3,1/3,1/3),
#'                             prob2 = c(1/6,1/6,2/3))
#'  ssd.test(dists)
#'
#' @export
ssd.test = function(dists.obj){

  if(!is(dists.obj, 'Distributions')){
    stop("Input must be of class 'Distributions'.")
  }

  ssd.result = ssd.calc(dists.obj)

  winner = comparison(ssd.result$ssd1, ssd.result$ssd2)

  return(list(ssd1 = ssd.result$ssd1, ssd2 = ssd.result$ssd2, winner = winner))
}

#' Calculates the SSD values for a pair of prospects.
#'
#' @param dists.obj A Distributions object.
#' @returns A list, containing two SSD vectors.
#' @examples
#' dists = createDistributions(outcome1 = c(1,4,7),
#'                             outcome2 = c(2,3,5),
#'                             prob1 = c(1/3,1/3,1/3),
#'                             prob2 = c(1/6,1/6,2/3))
#' ssd.calc(dists)
#'
ssd.calc = function(dists.obj){

  outcome = dists.obj@outcome
  cdf1 = dists.obj@cum.prob1
  cdf2 = dists.obj@cum.prob2

  ssd1 = cumsum(lag(cdf1, default = 0) * (outcome - lag(outcome, default = 0)))
  ssd2 = cumsum(lag(cdf2, default = 0) * (outcome - lag(outcome, default = 0)))

  return(list(ssd1 = ssd1, ssd2 = ssd2))
}
