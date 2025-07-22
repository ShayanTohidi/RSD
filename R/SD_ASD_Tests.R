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
