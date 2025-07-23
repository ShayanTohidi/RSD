#' Creating paired distributions
#'
#' with the input parameters and four other functions, it will create a data frame
#' that includes all pairs of variables and their corresponding probability and
#' outcome values.
#'
#' @details
#' Here, we first sort variables based on their outcome in ascending order, and
#' then create unordered unique pairs. So, the first element (variable) of any
#' pair has larger mean outcome than the second element.
#'
#' The output has four columns. The first two represent the two elements of each
#' pair. The last two are the corresponding probability and outcome for the
#' elements, respectively. These are in nested format, meaning each cell of the
#' last two columns includes a data frame with two columns: probability and
#' outcome.
#'
#' @seealso [create.dataframe(), sort.variables(), pair.variables(), pair.distributions()]
#'
#' @param variable A character vector.
#' @param probability A numeric vector.
#' @param outcome A numeric vector.
#' @returns A data frame with four columns.
#'
create.paired.distributions = function(variable, probability, outcome){

  sorted.vars = sort.variables(variable, outcome)
  paired.vars = pair.variables(sorted.vars)

  org.data = create.dataframe(variable, probability, outcome)
  paired.dists = pair.distributions(org.data, paired.vars)

  return(paired.dists)
}

#' create a data frame from input parameters
#'
#' @param variable A character vector.
#' @param probability A numeric vector.
#' @param outcome A numeric vector.
#' @returns A data frame with three columns.
#'
create.dataframe = function(variable, probability, outcome){

  data = data.frame(variable = variable,
                    probability = probability,
                    outcome = outcome)
  return(data)
}

#' Sorting all variables based on their outcomes
#'
#' @importFrom dplyr group_by summarise arrange pull desc
#' @importFrom magrittr "%>%"
#'
#' @param variable A character vector, includes variable names.
#' @param outcome A numeric vector, includes outcome values.
#' @returns A character vector, includes all variable names sorted by their
#' outcomes.
#'
#' @method sort variables
sort.variables = function(variable, outcome){

  df = data.frame(variable = variable, outcome = outcome)

  variables = df %>%
    group_by(variable) %>%
    summarise(avg = mean(outcome)) %>%
    arrange(desc(avg)) %>%
    pull(variable)

  return(variables)
}

#' Paring all the variables
#'
#' It uses a vector of variables and creates unique unordered pairs.
#'
#' @details
#' These pairs are unique unordered pairs, meaning the order of variables does
#' not matter. So if we have pair \{x,y\} we do not create another pair \{y,x\},
#' because they are technically the same. If the number of variables is n, the
#' number of pairs is n(n-1)/2.
#'
#' @param variables A character vector, including the variable names.
#' @returns A data frame, that includes two columns; every row represents a pair
#' of variables.
#'
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
