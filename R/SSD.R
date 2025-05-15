
calc.ssd = function(yield, cdf1, cdf2){
  ssd1 = cumsum(lag(cdf1, default = 0) * (yield - lag(yield, default = 0)))
  ssd2 = cumsum(lag(cdf2, default = 0) * (yield - lag(yield, default = 0)))

  return(list(ssd1 = ssd1, ssd2 = ssd2))
}
