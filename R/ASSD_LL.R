
calc.assd.ll = function(area, pos.neg.area.assd.ll, exp.val){

  total.area = sum(abs(area))

  pos.area = pos.neg.area.assd.ll$positive.area
  neg.area = pos.neg.area.assd.ll$negative.area

  if(pos.area/total.area < 0.5 & exp.val$mean1 >= exp.val$mean2){
    winner = 1
    epsilon = round(pos.area/total.area,3)
  } else if(neg.area/total.area < 0.5 & exp.val$mean1 <= exp.val$mean2){
    winner = 2
    epsilon = round(neg.area/total.area,3)
  } else {
    winner = 0
    epsilon = round(min(neg.area,pos.area)/total.area,3)
  }

  return(list(winner = winner, epsilon = epsilon))
}

calc.pos.neg.assd.ll = function(cdf.res, new.yield.ssd){
  pos = 0
  neg = 0

  i=1
  j=1
  while (i < length(cdf.res$outcome)) {
    y1 = new.yield.ssd$outcome[j]
    cdf1 = cdf.res$cdf1[i]
    cdf2 = cdf.res$cdf2[i]
    if(cdf.res$outcome[i+1] == new.yield.ssd$outcome[j+1]){
      y2 = cdf.res$outcome[i+1]
      ssd1 = new.yield.ssd$ssd1[j+1]
      ssd2 = new.yield.ssd$ssd2[j+1]
      i = i + 1
      j = j + 1
    } else {
      y2 = new.yield.ssd$outcome[j+1]
      ssd1 = new.yield.ssd$ssd1[j]
      ssd2 = new.yield.ssd$ssd2[j]
      j = j + 1
    }
    area = (y2-y1)*(cdf1-cdf2)
    ssd.diff = ssd1 - ssd2
    if(area > 0 & ssd.diff > 0) pos = pos + area
    if(area < 0 & ssd.diff < 0) neg = neg + abs(area)
  }

  return(list(positive.area = pos, negative.area = neg))
}
