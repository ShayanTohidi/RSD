#' Compares prospects based on AFSD
#'
#' @export
afsd = function(outcome, cdf1, cdf2){

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
#' It calculates the area between the CDFs of two prospects as the segments
#' of the combined CDFs, which is matched with the outcomes.
#'
#' @seealso [fsd()] for the parameters.
#'
#' @param outcome A numeric vector, including all outcomes in ascending order.
#' @param cdf1,cdf2 Numeric vectors, including marginal CDF corresponding to
#' each prospect.
#' @returns A numeric vector, including the area between the segments of CDFs.
#' @examples
#' outcome1 = c(1,4,7)
#' outcome2 = c(2,3,5)
#' prob1 = c(1/3,1/3,1/3)
#' prob2 = c(1/6,1/6,2/3)
#' obj = fsd(outcome1, outcome2, prob1, prob2)
#' calc.betw.cdf.area(obj$outcome, obj$cdf1, obj$cdf2)
#'
calc.betw.cdf.area = function(outcome, cdf1, cdf2){
  y = (outcome - lag(outcome))[2:length(outcome)]
  cdf1.mod = cdf1[-length(cdf1)]
  cdf2.mod = cdf2[-length(cdf2)]

  return(y * (cdf1.mod - cdf2.mod))
}
