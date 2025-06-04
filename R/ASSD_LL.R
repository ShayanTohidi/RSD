
assd.ll.test = function(sd.obj){

  if(!is(sd.obj, 'StochasticDominance')){
    stop("Input must be of class 'StochasticDominance'.")
  }

  exp.val = expected.values(sd.obj)
  mean1 = exp.val$mean1
  mean2 = exp.val$mean2

  pos.neg.areas = pos.neg.area.assd.ll(sd.obj)
  pos.area = pos.neg.areas$positive.area
  neg.area = pos.neg.areas$negative.area

  total.area = afsd.test(sd.obj)$total.area

  if(pos.area/total.area < 0.5 & mean1 >= mean2){
    winner = 1
    epsilon = round(pos.area/total.area,3)
  } else if(neg.area/total.area < 0.5 & mean1 <= mean2){
    winner = 2
    epsilon = round(neg.area/total.area,3)
  } else {
    winner = 0
    epsilon = round(min(neg.area,pos.area)/total.area,3)
  }

  return(list(winner = winner, epsilon = epsilon))
}

#' Calculates positive and negative area between CDFs for ASSD-LL
#'
#' It calculates the positive and negative areas between CDFs. The positive is
#' where both CDF and SSD of the first prospect is larger, and vice versa for
#' the negative case.
#'
#' @param sd.obj StochasticDominance object.
#' @returns A list, including two numbers corresponding to the positive and
#' negative areas, respectively.
#' @examples
#' sd = createStochasticDominance(outcome1 = c(1,4,7),
#'                                outcome2 = c(2,3,5),
#'                                prob1 = c(1/3,1/3,1/3),
#'                                prob2 = c(1/6,1/6,2/3))
#' pos.neg.area.assd.ll(sd)
#'
pos.neg.area.assd.ll = function(sd.obj){

  new.outcome.ssd = modif.outcome.ssd.calc(sd.obj)

  pos = 0
  neg = 0

  i=1
  j=1
  while (i < length(cdf.res$outcome)) {
    y1 = new.outcome.ssd$outcome.new[j]
    cdf1 = sd.obj@cdf1[i]
    cdf2 = sd.obj@cdf2[i]
    if(sd.obj@outcome[i+1] == new.outcome.ssd$outcome.new[j+1]){
      y2 = sd.obj@outcome[i+1]
      ssd1 = new.outcome.ssd$ssd1.new[j+1]
      ssd2 = new.outcome.ssd$ssd2.new[j+1]
      i = i + 1
      j = j + 1
    } else {
      y2 = new.outcome.ssd$outcome.new[j+1]
      ssd1 = new.outcome.ssd$ssd1.new[j]
      ssd2 = new.outcome.ssd$ssd2.new[j]
      j = j + 1
    }
    area = (y2-y1)*(cdf1-cdf2)
    ssd.diff = ssd1 - ssd2
    if(area > 0 & ssd.diff > 0) pos = pos + area
    if(area < 0 & ssd.diff < 0) neg = neg + abs(area)
  }

  return(list(positive.area = pos, negative.area = neg))
}
