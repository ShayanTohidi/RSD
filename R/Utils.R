#' Calculating Expected value
#'
#' It calculates the expected value of both prospects given their
#' outcomes and probabilities.
#'
#' @details
#' All parameters corresponding to the first prospect must have the same length,
#' meaning `outcome1` and `prob1`. It has to be true for the second prospect as
#' well. Otherwise, an error will be raised.
#'
#' The summation of each probability vector, e.g., `prob1` and `prob2` must be
#' one. Otherwise, an error will be raised.
#'
#' @param outcome1,outcome2 Numeric vectors, including outcome values.
#' @param prob1,prob2 Numeric vectors, including probability values.
#' @returns A list, including two float elements as the expected value of each
#' prospect.
#' @examples
#'  outcome1 = c(1,4,7)
#'  outcome2 = c(2,3,5)
#'  prob1 = c(1/3,1/3,1/3)
#'  prob2 = c(1/6,1/6,2/3)
#'  sd_expected.values(outcome1, outcome2, prob1, prob2)
#'
sd_expected.values = function(outcome1, outcome2, prob1, prob2){

  if(!is.numeric(c(outcome1, outcome2, prob1, prob2))){
    stop("Error: all arguments should be numeric.")
  }

  if(length(outcome1) != length(prob1)){
    stop("Error: The length of 'outcome1' and 'prob1' must be equal.")
  }

  if(length(outcome2) != length(prob2)){
    stop("Error: The length of 'outcome2' and 'prob2' must be equal.")
  }

  if(sum(prob1) != 1 | sum(prob2) != 1){
    stop("Error: The summation of each 'prob1' and 'prob2' must be one.")
  }

  return(list(mean1 = sum(outcome1*prob1),
              mean2 = sum(outcome2*prob2)))
}

comparison = function(x, y){

  if(all(x <= y) & any(x < y)){
    winner = 1
  } else if(all(x >= y) & any(x > y)){
    winner = 2
  } else{
    winner = 0
  }

  return(winner)
}

calc.area.below.line = function(x1, x2, y1, y2){

  area.rect = y1*(x2-x1)
  area.trngl = (y2-y1)*(x2-x1)/2

  return(area.trngl + area.rect)
}

has.intersect = function(x1,x2,y11,y12,y21,y22){
  if((y11<y21 & y12>y22) | (y11>y21 & y12<y22)){
    return(TRUE)
  }
  return(FALSE)
}

calc.intrsect = function(x1,x2,y11,y12,y21,y22){

  m1 = (y12-y11)/(x2-x1)
  m2 = (y22-y21)/(x2-x1)
  b1 = y11 - m1*x1
  b2 = y21 - m2*x1
  xis = (b2-b1)/(m1-m2)
  yis = m1*xis + b1

  return(list(x.intersect = xis, y.intersect = yis))
}

calc.mod.yield.ssd = function(yield, ssd1, ssd2){
  new.yield = yield
  new.ssd1 = ssd1
  new.ssd2 = ssd2
  n = length(yield)
  for (i in 1:(n-1)) {
    if(has.intersect(yield[i], yield[i+1], ssd1[i], ssd1[i+1], ssd2[i],
                     ssd2[i+1])){
      point = calc.intrsect(yield[i], yield[i+1], ssd1[i], ssd1[i+1], ssd2[i],
                            ssd2[i+1])
      new.yield = append(new.yield, point$x.intersect)
      new.ssd1 = append(new.ssd1, point$y.intersect)
      new.ssd2 = append(new.ssd2, point$y.intersect)
    }
  }

  return(list(outcome = sort(new.yield), ssd1 = sort(new.ssd1),
              ssd2 = sort(new.ssd2)))
}
