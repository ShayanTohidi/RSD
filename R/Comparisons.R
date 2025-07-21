compare.all = function(variable, probability, outcome, afsd.epsilon = 0.1,
                       assd.epsilon = 0.1, include.details = TRUE){

  paired.dists = create.paired.distributions(variable, probability, outcome)

  data = sd.test.all(paired.dists, include.details)


  return(data)
}
