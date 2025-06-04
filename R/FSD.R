#'  Compares random prospects by FSD
#'
#'  It compares two random prospects by the first-order stochastic dominance (FSD),
#'  given the distributions.
#'
#'  @details
#'  If neither prospect dominates the other, it returns 0.
#'
#'  A prospect dominates when its CDF is below the other one. It means that all
#'  element of the CDF vector must be equal or smaller, and at least one element
#'  should be smaller for the dominant prospect.
#'
#'  @param dists.obj Distributions object.
#'  @returns An integer, indicating the index of the dominant prospect.
#'  @examples
#' dists = createDistributions(outcome1 = c(1,4,7),
#'                             outcome2 = c(2,3,5),
#'                             prob1 = c(1/3,1/3,1/3),
#'                             prob2 = c(1/6,1/6,2/3))
#'  fsd.test(dists)
#'
#' @export
fsd.test = function(sd.obj){

  if(!is(sd.obj, 'StochasticDominance')){
    stop("Input must be of class 'StochasticDominance'.")
  }

  winner = comparison(sd.obj@cdf1, sd.obj@cdf1)

  return(winner)
}
