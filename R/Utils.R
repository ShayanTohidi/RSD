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
#'  expected.values(outcome1, outcome2, prob1, prob2)
#'
expected.values = function(dists){

  if(!is(x, Distributions)){
    stop("Input must be of class 'Distributions'.")
  }

  return(list(mean1 = sum(dists@outcome * dists@prob1),
              mean2 = sum(dists@outcome * dists@prob2)))
}

#' Comparing two numeric vectors
#'
#' This function compares two numeric vectors. The vector whose all elements
#' smaller or equal to all elements of the other one where at least one element
#' is smaller, will be the winner.
#'
#' @details
#' The function returns the index of the winner, meaning 1 or 2, corresponding
#' to parameters `x` and `y`, respectively. If no vector meets the condition, 0
#' will be returned.
#'
#' @param x,y Numeric vectors.
#' @returns An integer, among 0, 1, 2.
#' @examples
#' x = c(1, 2, 3)
#' y = c(2, 3, 4)
#'
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

#' Calculates the area between x-axis and a straight line
#'
#' It takes four parameters as the coordinates of two points at the beginning
#' and end of a straight line, and calculates the area exactly below the line
#' (between the line and the x-axis).
#'
#' @param x1,x2 Float values, indicating the first coordinates of start and end
#' points, respectively.
#' @param y1,y2 Float values, indicating the second coordinates of start and end
#' points, respectively.
#' @returns A float.
#' @examples
#' x1 = 2
#' y1 = 2
#' x2 = 4
#' y2 = 5
#' calc.area.below.line(x1, x2, y1, y2)
#'
calc.area.below.line = function(x1, x2, y1, y2){

  area.rect = y1*(x2-x1)
  area.trngl = (y2-y1)*(x2-x1)/2

  return(area.trngl + area.rect)
}

#' If two lines have intersection or no.
#'
#' It determines if two lines have intersection point or not. The start and
#' end points of both lines have equal first coordinate value (the one
#' corresponds to the x-axis).
#'
#' @param x1,x2 Float values.
#' @param y11,y12 Float values, corresponding to the first line.
#' @param y21,y22 Float values, corresponding to the second line.
#' @returns A Boolean.
#' @examples
#' x1 = 1
#' x2 = 5
#' y11 = 1
#' y12 = 6
#' y21 = 3
#' y22 = 4
#' has.intersection(x1, x2, y11, y12, y21, y22)
#'
has.intersection = function(x1, x2, y11, y12, y21, y22){
  if((y11<y21 & y12>y22) | (y11>y21 & y12<y22)){
    return(TRUE)
  }
  return(FALSE)
}

#' Calculates the intersection point of two lines.
#'
#' Given the start and end coordinates of two straight lines, we calculate the
#' intersection point of them.
#'
#' @details
#' Having intersection point has been checked before.
#'
#' @seealso [has.intersect()]
#'
#' @param x1,x2 Float values.
#' @param y11,y12 Float values, corresponding to the first line.
#' @param y21,y22 Float values, corresponding to the second line.
#' @returns A list, including the coordinates of the intersection point.
#' @examples
#' x1 = 1
#' x2 = 5
#' y11 = 1
#' y12 = 6
#' y21 = 3
#' y22 = 4
#' calc.intersection(x1, x2, y11, y12, y21, y22)
#'
calc.intersection = function(x1,x2,y11,y12,y21,y22){

  m1 = (y12-y11)/(x2-x1)
  m2 = (y22-y21)/(x2-x1)
  b1 = y11 - m1*x1
  b2 = y21 - m2*x1
  xis = (b2-b1)/(m1-m2)
  yis = m1*xis + b1

  return(list(x.intersect = xis, y.intersect = yis))
}

#' Modify outcome vector.
#'
#' It modify outcome vector to have all original values plus all intersection
#' points in ascending order. The corresponding SSD vectors will also be
#' returned.
#'
#' @param outcome A numeric vector.
#' @param ssd1,ssd2 Numeric vectors.
#' @returns A list of three elements.
#' @examples
#' outcome1 = c(1,4,7)
#' outcome2 = c(2,3,5)
#' prob1 = c(1/3,1/3,1/3)
#' prob2 = c(1/6,1/6,2/3)
#' obj.fsd = fsd(outcome1, outcome2, prob1, prob2)
#' obj.ssd = ssd(obj.fsd$outcome, obj.fsd$cdf1, obj.fsd$cdf2)
#' calc.mod.outcome(obj.fsd$outcome, obj.ssd$ssd1, obj.ssd$ssd2)
#'
calc.mod.outcome = function(outcome, ssd1, ssd2){
  new.outcome = outcome
  new.ssd1 = ssd1
  new.ssd2 = ssd2
  n = length(outcome)
  for (i in 1:(n-1)) {
    if(has.intersect(outcome[i], outcome[i+1], ssd1[i], ssd1[i+1], ssd2[i],
                     ssd2[i+1])){
      point = calc.intrsect(outcome[i], outcome[i+1], ssd1[i], ssd1[i+1], ssd2[i],
                            ssd2[i+1])
      new.outcome = append(new.outcome, point$x.intersect)
      new.ssd1 = append(new.ssd1, point$y.intersect)
      new.ssd2 = append(new.ssd2, point$y.intersect)
    }
  }

  return(list(outcome = sort(new.outcome), ssd1 = sort(new.ssd1),
              ssd2 = sort(new.ssd2)))
}
