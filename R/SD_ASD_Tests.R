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

#' Performing all SD and ASD methods on a pair
#'
#' @details
#' The input parameters are two data frames corresponding to each element of a
#' pair, respectively. Each data frame has two columns: probability and outcome,
#' which represents the distribution of the element (variable).
#'
#' First, a stochastic dominance object is created using the columns of the two
#' input parameters. Then, all available SD and ASD tests that are implemented
#' inside the package are performed.
#'
#' The output list contains 8 elements: `fsd`, `ssd`, `afsd`, `assd.ll`, and
#' `assd.ths` indicate the index of the dominant element (variable). If the first
#' variable dominates, they show 1, else they show 2. If neither dominates they
#' return 0. The other output parameters that end with 'eps' (e.g. `afsd.eps`,
#' `assd.ll.eps`, and `assd.ths.eps`) show the epsilon corresponding to each
#' of those ASD tests.
#'
#' @seealso [createStochasticDominance(), fsd.test(), ssd.test(), afsd.test(), assd.test()]
#'
#' @param data1,data2 Data frames corresponding to each element of a pair,
#' respectively.
#' @returns A list of 8 elements indicating the result of each test and the
#' epsilon values for almost tests.
#'
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
