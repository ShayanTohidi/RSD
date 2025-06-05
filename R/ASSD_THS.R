#'
assd.ths.test = function(sd.obj){

  area = area.btwn.ssd.calc(sd.obj)

  exp.val = expected.values(sd.obj)
  mean1 = exp.val$mean1
  mean2 = exp.val$mean2

  total.area = sum(abs(area))
  neg.area = abs(sum(area[area < 0]))
  pos.area = sum(area[area > 0])

  if(neg.area/total.area < 0.5 & mean2 >= mean1){
    winner = 2
    epsilon = round(neg.area/total.area,3)
  } else if(pos.area/total.area < 0.5 & mean2 <= mean1){
    winner = 1
    epsilon = round(pos.area/total.area,3)
  } else {
    winner = 0
    epsilon = round(min(pos.area,neg.area)/total.area,3)
  }

  return(list(winner = winner, epsilon = epsilon, area = area,
              total.area = total.area, positive.area = pos.area,
              negative.area = neg.area))
}

#' Calculates area between SSD functions
#'
#' For every segments, the area between SSD functions will be computed.
#'
#' @param sd.obj StochasticDominance object.
#' @returns Numeric vector, including area differences in every segments.
#' @examples
#' # example code
#'
area.btwn.ssd.calc = function(sd.obj){

  new.outcome.ssd = modif.outcome.ssd.calc(sd.obj)

  area1 = area.below.ssd.calc(new.outcome.ssd$outcome, new.outcome.ssd$ssd1)
  area2 = area.below.ssd.calc(new.outcome.ssd$outcome, new.outcome.ssd$ssd2)

  return(area1 - area2)
}

#' Calculates area below SSD function
#'
#' For every segment, the area below SSD function will be computed.
#'
#' @seealso [calc.area.below.line()] for area below line.
#'
#' @param outcome Numeric vector, including outcome values.
#' @param ssd Numeric vector, including SSD values.
#' @returns Numeric vector, including area below every segment of SSD function.
#' @examples
#' # example code
#'
area.below.ssd.calc = function(outcome, ssd){

  area = c()
  n = length(outcome)
  for (i in 1:(n-1)) {
    a = calc.area.below.line(outcome[i],outcome[i+1],ssd[i],ssd[i+1])
    area = append(area, a)
  }

  return(area)
}
