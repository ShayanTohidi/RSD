calc.assd.ths = function(area, exp.val){
  total.area = sum(abs(area))
  neg.area = abs(sum(area[area < 0]))
  pos.area = sum(area[area > 0])

  if(neg.area/total.area < 0.5 & exp.val$mean2 >= exp.val$mean1){
    winner = 2
    epsilon = round(neg.area/total.area,3)
  } else if(pos.area/total.area < 0.5 & exp.val$mean2 <= exp.val$mean1){
    winner = 1
    epsilon = round(pos.area/total.area,3)
  } else {
    winner = 0
    epsilon = round(min(pos.area,neg.area)/total.area,3)
  }

  return(list(winner = winner, epsilon = epsilon, negative.area = neg.area,
              positive.area = pos.area))
}

calc.area.betw.ssd = function(new.yield.ssd){
  yield = new.yield.ssd$outcome
  ssd1 = new.yield.ssd$ssd1
  ssd2 = new.yield.ssd$ssd2

  area1 = calc.area.below.ssd(yield, ssd1)
  area2 = calc.area.below.ssd(yield, ssd2)

  return(area1 - area2)
}

calc.area.below.ssd = function(outcome, ssd){
  area = c()
  n = length(outcome)
  for (i in 1:(n-1)) {
    a = calc.area.below.line(outcome[i],outcome[i+1],ssd[i],ssd[i+1])
    area = append(area, a)
  }

  return(area)
}
