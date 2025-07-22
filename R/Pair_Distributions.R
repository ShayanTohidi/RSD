create.paired.distributions = function(variable, probability, outcome){

  org.data = create.dataframe(variable, probability, outcome)

  sorted.vars = sort.variables(variable, outcome)
  paired.vars = pair.variables(sorted.vars)

  paired.dists = pair.distributions(org.data, paired.vars)

  return(paired.dists)
}

create.dataframe = function(variable, probability, outcome){

  if (!(is.character(variable) & is.numeric(probability) & is.numeric(outcome))){
    stop("Expected types: variable (character), probability and outcome: (numeric)")
  }

  len = length(variable)
  if (length(probability) != len | length(outcome) != len) {
    stop("All input vectors must have the same length.")
  }

  data = data.frame(variable = variable,
                    probability = probability,
                    outcome = outcome)
  return(data)
}

sort.variables = function(variable, outcome){

  df = data.frame(variable = variable, outcome = outcome)

  variables = df %>%
    group_by(variable) %>%
    summarise(avg = mean(outcome)) %>%
    arrange(desc(avg)) %>%
    pull(variable)

  return(variables)
}

pair.variables = function(variables){

  n = length(variables)
  data = data.frame(variable1 = variables[rep(1:(n-1), (n-1):1)],
                    variable2 = variables[unlist(sapply(seq(2,n),
                                                        function(x) seq(x,n)))])

  return(data)
}

#' Paring all the distributions
#'
#' It uses paired variables and collects all the related information (probability
#' and outcome) corresponding to each variable of every pairs.
#'
#' @importFrom dplyr nest_join rename
#' @importFrom magrittr "%>%"
#'
#' @param org.data A data frame that includes all variables, probabilities, and
#' outcomes.
#' @param paired.vars A data frame that includes all unique pairs of variables.
#' @returns A data frame that includes all unique pairs of variables with their
#' corresponding distributions in the nested format.
#'
pair.distributions = function(org.data, paired.vars){

  data = paired.vars %>%
    nest_join(org.data, by = c('variable1' = 'variable')) %>%
    rename(data1 = org.data) %>%
    nest_join(org.data, by = c('variable2' = 'variable')) %>%
    rename(data2 = org.data)

  return(data)
}
