compare.all = function(variable, probability, outcome, afsd.epsilon = 0.1,
                       assd.epsilon = 0.1, include.details = TRUE){

  paired.dists = create.paired.distributions(variable, probability, outcome)

  data = sd.test.all(paired.dists, include.details)


  return(data)
}

#### perform sd and asd tests on all pairs ####

sd.test = function(data1, data2){

  sd.obj = createStochasticDominance(data1$outcome, data2$outcome,
                                     data1$probability, data2$probability)
  fsd = fsd.test(sd.obj)
  ssd = ssd.test(sd.obj)
  afsd = afsd.test(sd.obj)
  assd.ll = assd.test(sd.obj, 'll')
  assd.ths = assd.test(sd.obj, 'ths')

  return(list(fsd = fsd, ssd = ssd,
              afsd = afsd$winner, afsd.eps = afsd$epsilon,
              assd.ll = assd.ll$winner, assd.ll.eps = assd.ll$epsilon,
              assd.ths = assd.ths$winner, assd.ths.eps = assd.ths$epsilon))
}

#### create distribution pairs ####

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

pair.distributions = function(org.data, paired.vars){

  data = paired.vars %>%
    nest_join(org.data, by = c('variable1' = 'variable')) %>%
    rename(data1 = org.data) %>%
    nest_join(org.data, by = c('variable2' = 'variable')) %>%
    rename(data2 = org.data)

  return(data)
}
