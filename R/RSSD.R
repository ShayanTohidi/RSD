#' Compares two random prospects by RSSD
#'
#' It compares two random prospects by RSSD, which is a relaxed version of the
#' classical SSD. In this method, instead of comparing all SSD values, the
#' summation of them will be checked, and the prospect having the smaller value
#' dominates by RSSD.
#'
#' @details
#' If no prospect dominates, the returned index for the winner will be 0.
#'
#' @importFrom methods is
#'
#' @param sd.obj StochasticDominance object.
#' @returns A list of three elements, the first two are float, indicating the
#' RSSD values, and the last one is an integer, representing the index of the
#' dominant prospect.
#' @examples
#' sd = createStochasticDominance(outcome1 = c(1,4,7),
#'                                outcome2 = c(2,3,5),
#'                                prob1 = c(1/3,1/3,1/3),
#'                                prob2 = c(1/6,1/6,2/3))
#' rssd.test(sd)
#'
#' @export
rssd.test = function(sd.obj){

  if(!is(sd.obj, 'StochasticDominance')){
    stop("Input must be of class 'StochasticDominance'.")
  }

  rssd1 = sum(sd.obj@ssd1)
  rssd2 = sum(sd.obj@ssd2)

  if(rssd1 < rssd2){
    winner = 1
  } else if(rssd1 > rssd2){
    winner = 2
  } else {
    winner = 0
  }

  return(list(rssd1 = rssd1, rssd2 = rssd2, winner = winner))
}
