setClass(
  'Distributions',
  slots = list(outcome = 'numeric', prob1 = 'numeric', prob2 = 'numeric',
               cum.prob1 = 'numeric', cum.prob2 = 'numeric'),
  validity = function(object){
    if(is.unsorted(object@outcome)) return('Outcome must be sorted in ascending order.')
    if(sum(object@prob1) != 1) return('Prob1 must add up to one.')
    if(sum(object@prob2) != 1) return('Prob2 must add up to one.')
    if(length(object@outcome) != length(object@prob1) |
       length(object@outcome) != length(object@prob2) |
       length(object@outcome) != length(object@cum.prob1) |
       length(object@outcome) != length(object@cum.prob2)) return('Length of the input arguments must be equal.')
    if(any(cumsum(object@prob1) != object@cum.prob1) |
       any(cumsum(object@prob2) != object@cum.prob2)) return('Probability and cumulative arguments do not match.')
  }
)

createDistributions = function(outcome1, outcome2, prob1, prob2){

  if(length(outcome1) != length(prob1)){
    stop("Error: The length of 'outcome1' and 'prob1' must be equal.")
  }

  if(length(outcome2) != length(prob2)){
    stop("Error: The length of 'outcome2' and 'prob2' must be equal.")
  }

  if(sum(prob1) != 1 | sum(prob2) != 1){
    stop("Error: The summation of each 'prob1' and 'prob2' must be one.")
  }

  df1 = data.frame(Yield = outcome1, f = prob1)
  df2 = data.frame(Yield = outcome2, g = prob2)

  library(dplyr)
  library(tidyr)

  df = df1 %>%
    full_join(df2, by = 'Yield') %>%
    replace_na(list(f=0,g=0)) %>%
    arrange(Yield) %>%
    mutate('F' = cumsum(f), G = cumsum(g))

  new('Distributions', outcome = df$Yield, prob1 = df$f, prob2 = df$g,
      cum.prob1 = df$F, cum.prob2 = df$G)
}
