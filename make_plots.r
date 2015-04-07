
#only converged runs
hake <- subset(ssb.ts.long, converged == 'yes' & species == 'hake-age')
yellow <- subset(ssb.ts.long, converged == 'yes' & species == 'yellow-age')

#End year biomass
g <- plot_scalar_boxplot(data = hake, x = 'data.desc', y = 'end.b_re', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + labs(x = 'data type', y = 'relative error in end year ssb',
  title = 'hake') + theme_bw()
ggsave('results_figures/hake_end_ssb.png', g)

g <- plot_scalar_boxplot(data = yellow, x = 'data.desc', y = 'end.b_re', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + labs(x = 'data type', y = 'relative error in end year ssb',
  title = 'yelloweye') + theme_bw()
ggsave('results_figures/yellow_end_ssb.png', g)

#last 10 year relative errors
g <- plot_scalar_boxplot(data = hake, x = 'data.desc', y = 'last.10.b_re', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + labs(x = 'data type', y = 'relative error in end year ssb',
  title = 'hake') + theme_bw()
ggsave('results_figures/hake_last10_ssb.png', g)

g <- plot_scalar_boxplot(data = yellow, x = 'data.desc', y = 'last.10.b_re', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + labs(x = 'data type', y = 'relative error in end year ssb',
  title = 'yelloweye') + theme_bw()
ggsave('results_figures/yellow_last10_ssb.png', g)

#last 25 year relative errors
g <- plot_scalar_boxplot(data = hake, x = 'data.desc', y = 'last.25.b_re', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + labs(x = 'data type', y = 'relative error in end year ssb',
  title = 'hake') + theme_bw()
ggsave('results_figures/hake_last25_ssb.png', g)

g <- plot_scalar_boxplot(data = yellow, x = 'data.desc', y = 'last.25.b_re', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + labs(x = 'data type', y = 'relative error in end year ssb',
  title = 'yelloweye') + theme_bw()
ggsave('results_figures/yellow_last25_ssb.png', g)





# #Make Plots
# g <- plot_ts_lines(ssb.ts.e, y = 'value', vert = 'D',
#   horiz = 'G', horiz2 = 'species', rel = TRUE, color = 'log_max_grad',
#   print = TRUE)
# ggsave("figs/b_re_length_comps.png", g, width = width, height = height)


# g <- plot_ts_lines(ssb.ts.x, y = 'value', vert = 'X',
#   horiz = 'G', horiz2 = 'species', rel = TRUE, color = 'log_max_grad', 
#   print = TRUE)
# ggsave("figs/b_re_wtatage.png", g, width = width, height = height)


# plot_sampled_wtatage(scenarios[1])

