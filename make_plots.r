
#----------------------------------------
#Check growth variation in models
g <- plot_ts_lines(checks.e, y = 'value', vert = 'data.amount', 
  horiz = 'G', horiz2 = 'species', print = TRUE, rel = TRUE) + labs(x = 'year',
  y = 'relativer error in SSB', title = 'Hake -- WtAtAge')

g <- plot_ts_lines(checks.x, y = 'value', vert = 'data.amount', 
  horiz = 'G', horiz2 = 'species', print = TRUE, rel = TRUE) + labs(x = 'year',
  y = 'relativer error in SSB', title = 'Hake -- WtAtAge')
# ggsave('results_figures/hake_wtatage_ssb.png', g)

#----------------------------------------
#Hake

#Mare plots
g <- plot_scalar_boxplot(data = hake, x = 'data.desc', y = 'mare', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + theme_bw() + labs(x = 'year', y = 'MARE', 
  title = 'Hake')
ggsave('results_figures/hake_mare.png', g)

#Mare plots
g <- plot_scalar_boxplot(data = hake, x = 'data.desc', y = 'mre', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + theme_bw() + labs(x = 'year', y = 'MRE', 
  title = 'Hake')
ggsave('results_figures/hake_mre.png', g)

#Yelloweye
g <- plot_scalar_boxplot(data = yellow, x = 'data.desc', y = 'mare', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + labs(x = 'data type', y = 'MARE',
  title = 'Yelloweye') + theme_bw() 
ggsave('results_figures/yellow_mare.png', g)

g <- plot_scalar_boxplot(data = yellow, x = 'data.desc', y = 'mre', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + labs(x = 'data type', y = 'MRE',
  title = 'Yelloweye') + theme_bw() 
ggsave('results_figures/yellow_mre.png', g)

#Biomass Trajectries
#hake----------------------------
#wtatage
g <- plot_ts_lines(hake.w, y = 'value', vert = 'data.amount',
  horiz = 'g.desc', print = TRUE, rel = TRUE) + labs(x = 'year',
  y = 'relativer error in SSB', title = 'Hake -- WtAtAge')
ggsave('results_figures/hake_wtatage_ssb.png', g)

#length-at-age
g <- plot_ts_lines(hake.l, y = 'value', vert = 'data.amount',
  horiz = 'g.desc', print = TRUE, rel = TRUE)+ labs(x = 'year',
  y = 'relativer error in SSB', title = 'Hake -- A + L')
ggsave('results_figures/hake_a_l_ssb.png', g)

#yellow----------------------------
#wtatage
g <- plot_ts_lines(yellow.w, y = 'value', vert = 'data.amount',
  horiz = 'g.desc', print = TRUE, rel = TRUE) + labs(x = 'year',
  y = 'relativer error in SSB', title = 'Yelloweye -- WtAtAge')
ggsave('results_figures/yellow_wtatage_ssb.png', g)

#length-at-age
g <- plot_ts_lines(yellow.l, y = 'value', vert = 'data.amount',
  horiz = 'g.desc', print = TRUE, rel = TRUE) + labs(x = 'year',
  y = 'relativer error in SSB', title = 'Yelloweye -- A + L')
ggsave('results_figures/yellow_a_l_ssb.png', g)



#------------------------------------------------------------------------
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
  rel = TRUE) + labs(x = 'data type', y = 'relative error in last 10 year ssb',
  title = 'hake') + theme_bw()
ggsave('results_figures/hake_last10_ssb.png', g)

g <- plot_scalar_boxplot(data = yellow, x = 'data.desc', y = 'last.10.b_re', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + labs(x = 'data type', y = 'relative error in last 10 year ssb',
  title = 'yelloweye') + theme_bw()
ggsave('results_figures/yellow_last10_ssb.png', g)

#last 25 year relative errors
g <- plot_scalar_boxplot(data = hake, x = 'data.desc', y = 'last.25.b_re', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + labs(x = 'data type', y = 'relative error in last 25 year ssb',
  title = 'hake') + theme_bw()
ggsave('results_figures/hake_last25_ssb.png', g)

g <- plot_scalar_boxplot(data = yellow, x = 'data.desc', y = 'last.25.b_re', 
  vert = 'data.amount', horiz = 'g.desc', print = TRUE, 
  rel = TRUE) + labs(x = 'data type', y = 'relative error in last 25 year ssb',
  title = 'yelloweye') + theme_bw()
ggsave('results_figures/yellow_last25_ssb.png', g)

#Biomass trajectories
# g <- plot_ts_lines



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

