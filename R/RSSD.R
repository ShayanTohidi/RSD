#' Calculates the RSSD values and compare them to find the dominant prospect.
#'
#' This function takes two numeric vectors, representing SSD values for the two
#' prospects, and computes the RSSD values with a comparison showing which one
#' dominates the other.
#'
#' @details
#' The length of `ssd1` and `ssd2` must be equal. Otherwise, an error will be
#' raised.
#'
#' @seealso [ssd()] for the parameters.
#'
#' @param ssd1,ssd2 Numeric vectors, including SSD values.
#' @returns A list of three elements, the first two are float, indicating the
#' RSSD values, and the last one is integer, representing the index of the
#' dominant prospect.
#' @examples
#' outcome1 = c(1,4,7)
#' outcome2 = c(2,3,5)
#' prob1 = c(1/3,1/3,1/3)
#' prob2 = c(1/6,1/6,2/3)
#' obj.fsd = fsd(outcome1, outcome2, prob1, prob2)
#' obj.ssd = ssd(obj.fsd$outcome, obj.fsd$cdf1, obj.fsd$cdf2)
#' rssd(obj.ssd$ssd1, obj.ssd$ssd2)
#'
#' @export
rssd = function(ssd1, ssd2){

  if(length(ssd1) != length(ssd2)){
    stop("Error: The length of 'ssd1' and 'ssd2' must be equal.")
  }

  rssd1 = sum(ssd1)
  rssd2 = sum(ssd2)

  if(rssd1 < rssd2){
    winner = 1
  } else if(rssd1 > rssd2){
    winner = 2
  } else {
    winner = 0
  }

  return(list(rssd1 = rssd1, rssd2 = rssd2, winner = winner))
}
