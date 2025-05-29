#' Calculates the SSD vectors and checks for domination
#'
#' This function takes three parameters corresponding to the joint distribution
#' and returns the SSD vectors for each prospect, with their comparison to find
#' the dominant one.
#'
#' @details
#' The length of `outcome`, `cdf1`, and `cdf2` must be equal. If not, an error
#' is raised.
#'
#' The output includes three elements: `ssd1`, `ssd2`: two vectors containing
#' ssd values for the corresponding prospects, and `winner`: an integer that
#' indicates the dominant prospect (1 or 2), and 0 if the domination fails.
#'
#' @seealso [fsd()] for the parameters.
#'
#' @param outcome A numeric vector, including all outcomes together in ascending
#' order.
#' @param cdf1,cdf2 Numeric vectors, including marginal CDF corresponding to
#' each prospect.
#' @returns A list of two numeric vectors corresponding to SSD values of each
#' prospect, and an integer value, indicating the dominant prospect.
#' @examples
#' outcome1 = c(1,4,7)
#' outcome2 = c(2,3,5)
#' prob1 = c(1/3,1/3,1/3)
#' prob2 = c(1/6,1/6,2/3)
#' obj = fsd(outcome1, outcome2, prob1, prob2)
#' ssd(obj$outcome, obj$cdf1, obj$cdf2)
#'
#' @export
ssd = function(outcome, cdf1, cdf2){

  if(!is.numeric(c(outcome, cdf1, cdf2))){
    stop("Error: all arguments should be numeric.")
  }

  if(length(outcome) != length(cdf1)){
    stop("Error: The length of 'outcome' and 'cdf1' must be equal.")
  }

  if(length(outcome) != length(cdf2)){
    stop("Error: The length of 'outcome' and 'cdf2' must be equal.")
  }

  ssd1 = cumsum(lag(cdf1, default = 0) * (outcome - lag(outcome, default = 0)))
  ssd2 = cumsum(lag(cdf2, default = 0) * (outcome - lag(outcome, default = 0)))

  env = new.env()
  sys.source('R/Utils.R', envir = env)

  return(list(ssd1 = ssd1, ssd2 = ssd2, winner = env$comparison(ssd1, ssd2)))
}
