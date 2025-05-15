
calc.rssd = function(ssd1, ssd2){
  rssd1 = sum(ssd1)
  rssd2 = sum(ssd2)

  if(rssd1 < rssd2){
    winner = 1
  } else if(rssd1 > rssd2){
    winner = 2
  } else {
    winner = 0
  }

  return(list(winner = winner, rssd1 = rssd1, rssd2 = rssd2))
}
