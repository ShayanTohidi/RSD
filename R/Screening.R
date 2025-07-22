screen = function(data, test, epsilon){

  variables = unique(append(data$variable1, data$variable2))
  # test.name = as_label(enquo(test))

  if (test == 'fsd' | test == 'ssd') {
    sets = sd.screen(data, variables, test)
  } else {
    eps.name = paste0(test, '.eps')
    sets = asd.screen(data, variables, test, epsilon, eps.name)
  }

  result = list(
    setNames(list(sets$inefficient), paste0(test, '.inefficient')),
    setNames(list(sets$efficient), paste0(test, '.efficient'))
  )

  return(unlist(result, recursive = F))
}

sd.screen = function(data, variables, test){

  sd.inefficient = data %>%
    filter(!!sym(test) == 1) %>%
    distinct(variable2) %>%
    pull(variable2)
  sd.efficient = setdiff(variables, sd.inefficient)

  return(list(inefficient = sd.inefficient, efficient = sd.efficient))
}

asd.screen = function(data, variables, test, epsilon, epsilon.name){

  asd.inefficient = data %>%
    filter(!!sym(test) == 1 & !!sym(epsilon.name) <= epsilon) %>%
    distinct(variable2) %>%
    pull(variable2)
  asd.efficient = setdiff(variables, asd.inefficient)

  return(list(inefficient = asd.inefficient, efficient = asd.efficient))
}
