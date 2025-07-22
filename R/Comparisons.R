compare.all = function(variable, probability, outcome, afsd.epsilon = 0.1,
                       assd.ll.epsilon = 0.1, assd.ths.epsilon = 0.1,
                       include.details = TRUE){

  paired.dists = create.paired.distributions(variable, probability, outcome)

  data = sd.test.all(paired.dists, include.details)

  fsd = screen(data, 'fsd', 0)
  afsd = screen(data, 'afsd', afsd.epsilon)
  ssd = screen(data, 'ssd', 0)
  assd.ll = screen(data, 'assd.ll', assd.ll.epsilon)
  assd.ths = screen(data, 'assd.ths', assd.ths.epsilon)


  return(list(data = data, fsd.sets = fsd, afsd.sets = afsd,
              ssd.sets = ssd, assd.ll.sets = assd.ll, assd.ths.sets = assd.ths))
}
