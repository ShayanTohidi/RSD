#' Compares two random prospects by RSSD
#'
#' This function takes two numeric vectors, representing SSD values for the two
#' prospects, and computes the RSSD values with a comparison showing which one
#' dominates the other.
#'
#' @details
#' The length of `ssd1` and `ssd2` must be equal. Otherwise, an error will be
#' raised.
#'
#' @seealso [ssd(), rssd.calc()] for the parameters.
#'
#' @param ssd1,ssd2 Numeric vectors, including SSD values.
#' @returns A list of three elements, the first two are float, indicating the
#' RSSD values, and the last one is integer, representing the index of the
#' dominant prospect.
#' @examples
#' dists = createDistributions(outcome1 = c(1,4,7),
#'                             outcome2 = c(2,3,5),
#'                             prob1 = c(1/3,1/3,1/3),
#'                             prob2 = c(1/6,1/6,2/3))
#' ssd.obj = ssd.calc(dists)
#' rssd.test(ssd.obj$ssd1, ssd.obj$ssd2)
#'
#' @export
rssd.test = function(sd.obj){

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
