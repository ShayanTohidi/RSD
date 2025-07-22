screen = function(data, test, epsilon){

  variables = unique(append(data$variable1, data$variable2))

  if (test == 'fsd' | test == 'ssd') {
    inefficient.set = sd.screen(data, variables, test)
  } else {
    eps.name = paste0(test, '.eps')
    inefficient.set = asd.screen(data, variables, test, epsilon, eps.name)
  }

  efficient.set = setdiff(variables, inefficient.set)

  result = list(
    setNames(list(inefficient.set), paste0(test, '.inefficient')),
    setNames(list(efficient.set), paste0(test, '.efficient'))
  )

  return(unlist(result, recursive = F))
}

sd.screen = function(data, variables, test){

  sd.inefficient = data %>%
    filter(!!sym(test) == 1) %>%
    distinct(variable2) %>%
    pull(variable2)

  return(sd.inefficient)
}

asd.screen = function(data, variables, test, epsilon, epsilon.name){

  asd.inefficient = data %>%
    filter(!!sym(test) == 1 & !!sym(epsilon.name) <= epsilon) %>%
    distinct(variable2) %>%
    pull(variable2)

  return(asd.inefficient)
}
