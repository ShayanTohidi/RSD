
calc.exp.val = function(y1,y2,prob1,prob2){

  return(list(mean1 = sum(y1*prob1), mean2 = sum(y2*prob2)))
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
