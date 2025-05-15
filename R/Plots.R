plot.fsd = function(yield, cdf1, cdf2){
  data = data.frame(yield, cdf_F = cdf1, cdf_G = cdf2)
  df = data %>%
    # select(yield, starts_with('cdf-')) %>%
    pivot_longer(cols = starts_with('cdf_'), names_to = 'Cultivars',
                 values_to = 'CDF') %>%
    separate(Cultivars, sep = '_', into = c('chert', 'Cultivars')) %>%
    select(-chert)
  plot = ggplot(df, mapping = aes(x=yield, y=CDF)) +
    geom_step(aes(color = Cultivars), linewidth = 1, alpha = 0.8) +
    # scale_color_discrete(labels = unique(df$Cultivars))+
    theme(axis.title = element_text(size = 22, face = 'bold'),
          # axis.title.x = element_blank(),
          axis.text = element_text(size = 18),
          legend.title = element_text(face = 'bold', size = 22),
          legend.text = element_text(size = 18),
          legend.position = 'bottom',
          legend.background = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, size = 10)))
  # scale_x_continuous(breaks = seq(0,8,1))
  show(plot)
  return(plot)
}

plot.ssd = function(yield, ssd1, ssd2){
  data = data.frame(yield, ssd_F = ssd1, ssd_G = ssd2)
  df = data %>%
    # select(yield, starts_with('ssd-')) %>%
    pivot_longer(cols = starts_with('ssd_'), names_to = 'Cultivars',
                 values_to = 'SSD') %>%
    separate(Cultivars, sep = '_', into = c('chert', 'Cultivars')) %>%
    select(-chert)
  plot = ggplot(df, mapping = aes(x=yield, y=SSD)) +
    geom_line(aes(color = Cultivars), linewidth = 1, alpha = 0.8) +
    # scale_color_discrete(labels = c('f','g'))+
    theme(axis.title = element_text(size = 22, face = 'bold'),
          # axis.title.x = element_blank(),
          axis.text = element_text(size = 18),
          legend.title = element_text(face = 'bold', size = 22),
          legend.text = element_text(size = 18),
          legend.position = 'bottom',
          legend.background = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, size = 10)))
  # if (length(xlimit) == 2 & length(ylimit) == 2){
  #   plot = plot +
  #     xlim(xlimit[1], xlimit[2]) +
  #     ylim(ylimit[1], ylimit[2])
  # }
  show(plot)
  return(plot)
}
