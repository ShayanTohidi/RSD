compare.all = function(variable, probability, outcome, afsd.epsilon = 0.1,
                       assd.epsilon = 0.1, include.details = TRUE){

  paired.dists = create.paired.distributions(variable, probability, outcome)

  data = sd.test.all(paired.dists, include.details)


  return(data)
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
