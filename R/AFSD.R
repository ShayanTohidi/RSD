#' Compares prospects based on AFSD
#'
#' It compares two prospects using AFSD criteria, that is the prospect having
#' the minimum violation area from a classic FSD.
#'
#' @details
#' The length of `outcome`, `cdf1`, and `cdf2` must be equal. If not, an error
#' is raised.
#'
#' The returned list has six elements: `winner` indicates the dominant prospect
#' index. It will be zero if neither dominates the other. `epsilon` is the ratio
#' of violated area to the total area between the CDFs. `area` is a vector, where
#' the values show the area between the CDFs correspond to each segment.
#' `total.area` is the total area between the CDFs. `positive.area` is the
#' amount of area where the `area` vector is positive, meaning the `cdf1` is
#' larger than `cdf2`. `negative.area` is like `positive.area` for negative
#' values.
#'
#' @seealso [fsd()] for the parameters, [calc.betw.cdf.area()] for area
#' calculations.
#'
#' @param outcome A numeric vector, including all outcomes in ascending order.
#' @param cdf1,cdf2 Numeric vectors, including marginal CDF corresponding to
#' each prospect.
#' @returns A list, including all the calculation details.
#' @examples
#' outcome1 = c(1,4,7)
#' outcome2 = c(2,3,5)
#' prob1 = c(1/3,1/3,1/3)
#' prob2 = c(1/6,1/6,2/3)
#' obj = fsd(outcome1, outcome2, prob1, prob2)
#' afsd(obj$outcome, obj$cdf1, obj$cdf2)
#'
#' @export
afsd = function(outcome, cdf1, cdf2){

  if(!is.numeric(c(outcome, cdf1, cdf2))){
    stop("Error: all arguments should be numeric.")
  }

  if(length(outcome) != length(cdf1)){
    stop("Error: The length of 'outcome' and 'cdf1' must be equal.")
  }

  if(length(outcome) != length(cdf2)){
    stop("Error: The length of 'outcome' and 'cdf2' must be equal.")
  }

  area = calc.betw.cdf.area(outcome, cdf1, cdf2)
  total.area = sum(abs(area))
  neg.area = abs(sum(area[area < 0]))
  pos.area = sum(area[area > 0])

  if(isTRUE(all.equal(neg.area, pos.area))){
    winner = 0
    epsilon = 0.5
  } else if(neg.area > pos.area){
    winner = 1
    epsilon = round(pos.area/total.area, 3)
  } else if(neg.area < pos.area){
    winner = 2
    epsilon = round(neg.area/total.area, 3)
  }

  return(list(winner = winner, epsilon = epsilon, area = area,
              total.area = total.area, positive.area = pos.area,
              negative.area = neg.area))
}

#' Calculates area between CDFs
#'
#' It calculates the area between the CDFs of two prospects divided by the CDFs
#' and outcomes segments.
#'
#' @param sd.obj StochasticDominance object.
#' @returns A numeric vector, including the area between the segments of CDFs.
#' @examples
#' sd = createStochasticDominance(outcome1 = c(1,4,7),
#'                                outcome2 = c(2,3,5),
#'                                prob1 = c(1/3,1/3,1/3),
#'                                prob2 = c(1/6,1/6,2/3))
#' area.btwn.cdfs.calc(sd)
#'
area.btwn.cdfs.calc = function(sd.obj){

  outcome = sd.obj@outcome
  cdf1 = sd.obj@cdf1
  cdf2 = sd.obj@cdf2

  y = (outcome - lag(outcome))[2:length(outcome)]
  cdf1.mod = cdf1[-length(cdf1)]
  cdf2.mod = cdf2[-length(cdf2)]

  return(y * (cdf1.mod - cdf2.mod))
}
