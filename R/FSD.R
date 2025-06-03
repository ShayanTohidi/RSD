#'  Compares random prospects by FSD
#'
#'  It compares two random prospects by the first-order stochastic dominance (FSD),
#'  given the distributions.
#'
#'  @details
#'  If neither prospect dominates the other, it returns 0.
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
fsd.test = function(dists.obj){

  if(!is(dists.obj, 'Distributions')){
    stop("Input must be of class 'Distributions'.")
  }

  env = new.env()
  sys.source('R/Utils.R', envir = env)

  winner = env$comparison(dists.obj@cum.prob1, dists.obj@cum.prob2)

  return(winner)
}
