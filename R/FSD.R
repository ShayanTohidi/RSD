#'  Calculates the joint distribution and checks for the FSD
#'
#'  This function takes two random prospects by their outcomes and
#'  probabilities, and calculates the joint distributions in ascending order. It
#'  also compares the CDFs to find the dominant prospect by FSD, if exists.
#'
#'  @details
#'  The parameters that end with the same number are corresponding to the same
#'  prospect, e.g., `outcome1` and `prob1` are the outcome and probability
#'  vectors of the first prospect.
#'
#'  The length of `outcome1` and `prob1` should be the same. This is true for
#'  the other two parameters. If not, an error is raised.
#'
#'  The `prob1` and `prob2`, each must adds up to one. If not, an error is
#'  raised.
#'
#'  The output includes four elements: `outcome`: a combined version of
#'  `outcome1` and `outcome2` in ascending order, `cdf1`, `cdf2`: the marginal
#'  cumulative distribution function (CDF) of the corresponding prospect, and
#'  `winner`: an integer corresponding to the dominant prospect (1 or 2). If there
#'  is no domination, it will be 0.
#'
#'  @param outcome1,outcome2 Numeric vectors, including outcome values.
#'  @param prob1,prob2 Numeric vectors, including probability values.
#'  @returns A list of three numeric vectors corresponding to the joint
#'  distribution, and an integer value, indicating the dominant prospect.
#'  @examples
#'  outcome1 = c(1,4,7)
#'  outcome2 = c(2,3,5)
#'  prob1 = c(1/3,1/3,1/3)
#'  prob2 = c(1/6,1/6,2/3)
#'  fsd(outcome1, outcome2, prob1, prob2)
#'
#' @export
fsd.test = function(dists){

  if(!is(dists, 'Distributions')){
    stop("Input must be of class 'Distributions'.")
  }

  env = new.env()
  sys.source('R/Utils.R', envir = env)

  return(winner = env$comparison(dists@cum.prob1, dists@cum.prob2))
}
