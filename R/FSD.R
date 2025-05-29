#'  Calculate the joint distribution
#'
#'  This function takes two random prospects by their outcomes and
#'  probabilities, and calculates the joint distributions in ascending order.
#'
#'  @details
#'  The parameters that end with the same number are corresponding to the same
#'  prospect, e.g., `outcome1` and `prob1` are the outcome and probability
#'  vectors of the first prospect.
#'
#'  The length of `outcome1` and `prob1` should be the same. This is true for
#'  the other two parameters. If not, the function raise an error.
#'
#'  The `prob1` and `prob2`, each must adds up to one. If not, the function
#'  raise an error.
#'
#'  The output includes three elements: outcome: a combined version of
#'  `outcome1` and `outcome2` in ascending order, cdf1, cdf2: the marginal
#'  cumulative distribution function (CDF) of the corresponding prospect.
#'
#'  @param outcome1,outcome2 Numeric vectors, including outcome values.
#'  @param prob1,prob2 Numeric vectors, including probability values.
#'  @returns A list of three numeric vectors.
#'  @examples
#'  outcome1 = c(1,4,7)
#'  outcome2 = c(2,3,5)
#'  prob1 = c(1/3,1/3,1/3)
#'  prob2 = c(1/6,1/6,2/3)
#'  sd_jointdist(outcome1, outcome2, prob1, prob2)
#'
#' @export
sd_jointdist = function(outcome1, outcome2, prob1, prob2){

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

  df1 = data.frame(Yield = outcome1, f = prob1)
  df2 = data.frame(Yield = outcome2, g = prob2)

  df = df1 %>%
    full_join(df2, by = 'Yield') %>%
    replace_na(list(f=0,g=0)) %>%
    arrange(Yield) %>%
    mutate('F' = cumsum(f), G = cumsum(g))

  return(list(outcome = df$Yield, cdf1 = df$F, cdf2 = df$G))
}
