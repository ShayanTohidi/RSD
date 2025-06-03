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
rssd.test = function(ssd1, ssd2){

  if(!is.numeric(c(ssd1, ssd2))){
    stop("Error: all arguments should be numeric.")
  }

  if(length(ssd1) != length(ssd2)){
    stop("Error: The length of 'ssd1' and 'ssd2' must be equal.")
  }

  rssd.result = rssd.calc(ssd1, ssd2)

  if(rssd.result$rssd1 < rssd.result$rssd2){
    winner = 1
  } else if(rssd.result$rssd1 > rssd.result$rssd2){
    winner = 2
  } else {
    winner = 0
  }

  return(list(rssd1 = rssd.result$rssd1, rssd2 = rssd.result$rssd2,
              winner = winner))
}

#' Calculates RSSD values
#'
#' It takes SSD vectors and add them up separately to calculate the RSSD values.
#'
#' @param ssd1,ssd2 Numeric vectors, including the SSD values.
#' @returns A list of two numbers, indicating the RSSD values.
#' @examples
#' dists = createDistributions(outcome1 = c(1,4,7),
#'                             outcome2 = c(2,3,5),
#'                             prob1 = c(1/3,1/3,1/3),
#'                             prob2 = c(1/6,1/6,2/3))
#' ssd.obj = ssd.calc(dists)
#' rssd.calc(ssd.obj$ssd1, ssd.obj$ssd2)
#'
rssd.calc = function(ssd1, ssd2){

  rssd1 = sum(ssd1)
  rssd2 = sum(ssd2)

  return(list(rssd1 = rssd1, rssd2 = rssd2))
}
