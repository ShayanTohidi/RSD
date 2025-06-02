#' Compares prospects based on AFSD
#'
#' @export
afsd = function(yield, cdf1, cdf2){

  area = calc.betw.cdf.area(yield, cdf1, cdf2)
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

calc.betw.cdf.area = function(yield, cdf1, cdf2){
  y = (yield - lag(yield))[2:length(yield)]
  cdf1.mod = cdf1[-length(cdf1)]
  cdf2.mod = cdf2[-length(cdf2)]

  return(y * (cdf1.mod - cdf2.mod))
}
