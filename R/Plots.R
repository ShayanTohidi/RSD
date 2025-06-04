#' Drawing the CDFs
#'
#' It visualizes the CDFs of both prospects.
#'
#' @details
#' The parameter `names` only accepts character vector, otherwise an error will
#' be raised.
#'
#' The function shows a step plot that includes both CDFs, and return the plot
#' object for further modifications.
#'
#' @param sd.obj StochasticDominance object.
#' @param names A character vector, including the names of prospects in order.
#' @returns A list, including plot elements.
#' @examples
#' sd = createStochasticDominance(outcome1 = c(1,4,7),
#'                                outcome2 = c(2,3,5),
#'                                prob1 = c(1/3,1/3,1/3),
#'                                prob2 = c(1/6,1/6,2/3))
#'  fsd.plot(sd, names = c('First', 'Second'))
#'
#' @export
fsd.plot = function(sd.obj, names = c('1', '2')){

  if(!is(sd.obj, 'StochasticDominance')){
    stop("Input must be of class 'StochasticDominance'.")
  }

  if(!is.character(names)){
    stop("Error: argument 'names' must be character.")
  }

  outcome = sd.obj@outcome
  cdf1 = sd.obj@cdf1
  cdf2 = sd.obj@cdf2
  name1 = names[1]
  name2 = names[2]

  data = data.frame('Outcomes' = outcome, 'cdf_1' = cdf1, 'cdf_2' = cdf2)

  library(dplyr)
  library(tidyr)

  df = data %>%
    pivot_longer(cols = starts_with('cdf'), names_to = 'prospects',
                 values_to = 'CDF') %>%
    separate(prospects, sep = '_', into = c('cdf', 'prospects')) %>%
    mutate(Prospects = case_when(prospects == 1 ~ name1, TRUE ~ name2)) %>%
    select(-cdf, -prospects)

  library(ggplot2)

  plot = ggplot(df, mapping = aes(x=Outcomes, y=CDF)) +
    geom_step(aes(color = Prospects), linewidth = 1.5, alpha = 0.7) +
    theme(axis.title = element_text(size = 18, face = 'bold'),
          axis.text = element_text(size = 14),
          legend.title = element_text(face = 'bold', size = 18),
          legend.text = element_text(size = 14),
          legend.position = 'bottom',
          legend.background = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 1.5, size = 10)))

  show(plot)

  return(plot)
}

#' Drawing the SSD
#'
#' It takes three parameters to visualize the SSD values of both prospects.
#'
#' @details
#' The length of all parameters must be equal. Otherwise, an error will be
#' raised.
#'
#' The function shows the plot and return a plot object for further modification.
#'
#' @seealso [ssd()] for the parameters.
#'
#' @param outcome A numeric vector, including all outcomes in ascending order.
#' @param ssd1,ssd2 Numeric vectors, including SSD values corresponding to each
#' prospects.
#' @returns A list, including plot elements.
#' @examples
#' outcome1 = c(1,4,7)
#' outcome2 = c(2,3,5)
#' prob1 = c(1/3,1/3,1/3)
#' prob2 = c(1/6,1/6,2/3)
#' obj.fsd = fsd(outcome1, outcome2, prob1, prob2)
#' obj.ssd = ssd(obj.fsd$outcome, obj.fsd$cdf1, obj.fsd$cdf2)
#' plot_ssd(obj.fsd$outcome, obj.ssd$ssd1, obj.ssd$ssd2)
#'
#' @export
ssd.plot = function(outcome, ssd1, ssd2, names = c('1', '2')){

  if(!is.numeric(c(outcome, ssd1, ssd2))){
    stop("Error: all arguments should be numeric.")
  }

  if(length(outcome) != length(ssd1)){
    stop("Error: The length of 'outcome' and 'ssd1' must be equal.")
  }

  if(length(outcome) != length(ssd2)){
    stop("Error: The length of 'outcome' and 'ssd2' must be equal.")
  }

  if(!is.character(names)){
    stop("Error: argument 'names' must be character.")
  }

  data = data.frame('Outcome' = outcome, 'ssd_1' = ssd1, 'ssd_2' = ssd2)
  name1 = names[1]
  name2 = names[2]

  library(dplyr)
  library(tidyr)

  df = data %>%
    pivot_longer(cols = starts_with('ssd_'), names_to = 'prospects',
                 values_to = 'SSD') %>%
    separate(prospects, sep = '_', into = c('ssd', 'prospects')) %>%
    mutate(Prospects = case_when(prospects == 1 ~ name1, TRUE ~ name2)) %>%
    select(-cdf, -prospects)

  library(ggplot2)

  plot = ggplot(df, mapping = aes(x=Outcome, y=SSD)) +
    geom_line(aes(color = Prospects), linewidth = 1.5, alpha = 0.7) +
    theme(axis.title = element_text(size = 18, face = 'bold'),
          axis.text = element_text(size = 14),
          legend.title = element_text(face = 'bold', size = 18),
          legend.text = element_text(size = 14),
          legend.position = 'bottom',
          legend.background = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 1.5, size = 10)))

  show(plot)

  return(plot)
}
