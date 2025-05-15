
calc.cdf = function(y1, y2, prob1, prob2){

  df1 = data.frame(Yield = y1, f = prob1)
  df2 = data.frame(Yield = y2, g = prob2)

  df = df1 %>%
    full_join(df2, by = 'Yield') %>%
    replace_na(list(f=0,g=0)) %>%
    arrange(Yield) %>%
    mutate('F' = cumsum(f), G = cumsum(g))

  return(list(outcome = df$Yield, cdf1 = df$F, cdf2 = df$G))
}
