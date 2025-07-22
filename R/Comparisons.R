compare.all = function(variable, probability, outcome, afsd.epsilon = 0.1,
                       assd.ll.epsilon = 0.1, assd.ths.epsilon = 0.1,
                       include.details = TRUE){

  paired.dists = create.paired.distributions(variable, probability, outcome)

  data = sd.test.all(paired.dists, include.details)

  fsd = screen(data, fsd, afsd, afsd.epsilon)


  return(list(data = data, fsd.sets = fsd))
}

#### screen: build efficient and inefficient sets ####

screen = function(data, test, epsilon, type){

  variables = unique(append(data$variable1, data$variable2))
  test.name = as_label(enquo(test))

  if (type == 'sd') {
    sets = sd.screen(data, variables, test)
  } else if (type == 'asd') {
    eps.name = paste0(test.name, '.eps')
    sets = asd.screen(data, variables, test, epsilon, eps.name)
  }

  result = list(
    setNames(list(sets$inefficient), paste0(test.name, '.inefficient')),
    setNames(list(sets$efficient), paste0(test.name, '.efficient'))
  )

  return(unlist(result, recursive = F))
}

sd.screen = function(data, variables, test){

  sd.inefficient = data %>%
    filter({{test}} == 1) %>%
    distinct(variable2) %>%
    pull(variable2)
  sd.efficient = setdiff(variables, sd.inefficient)

  return(list(inefficient = sd.inefficient, efficient = sd.efficient))
}

asd.screen = function(data, variables, test, epsilon, epsilon.name){

  asd.inefficient = data %>%
    filter({{test}} == 1 & !!sym(epsilon.name) <= epsilon) %>%
    distinct(variable2) %>%
    pull(variable2)
  asd.efficient = setdiff(variables, asd.inefficient)

  return(list(inefficient = asd.inefficient, efficient = asd.efficient))
}

#### perform sd and asd tests on all pairs ####

sd.test.all = function(paired.dists, include.details){

  result = paired.dists %>%
    rowwise() %>%
    mutate(res = list(sd.test(data1, data2))) %>%
    unnest_wider(res)

  if (!include.details) {
    result = result %>%
      select(-c(data1, data2))
  }

  return(result)
}

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
