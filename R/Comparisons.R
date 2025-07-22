compare.all = function(variable, probability, outcome, afsd.epsilon.threshold = 0.1,
                       assd.ll.epsilon.threshold = 0.1,
                       assd.ths.epsilon.threshold = 0.1, include.details = TRUE){

  if (!(is.character(variable) & is.numeric(probability) & is.numeric(outcome))){
    stop("Expected types: variable (character), probability and outcome: (numeric)")
  }

  len = length(variable)
  if (length(probability) != len | length(outcome) != len) {
    stop("All input vectors must have the same length.")
  }

  paired.dists = create.paired.distributions(variable, probability, outcome)

  data = sd.asd.test.all(paired.dists, include.details)

  fsd = screen(data, 'fsd', 0)
  afsd = screen(data, 'afsd', afsd.epsilon.threshold)
  ssd = screen(data, 'ssd', 0)
  assd.ll = screen(data, 'assd.ll', assd.ll.epsilon.threshold)
  assd.ths = screen(data, 'assd.ths', assd.ths.epsilon.threshold)


  return(list(data = data, fsd.sets = fsd, afsd.sets = afsd,
              ssd.sets = ssd, assd.ll.sets = assd.ll, assd.ths.sets = assd.ths))
}
