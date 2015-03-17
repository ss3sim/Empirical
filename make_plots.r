
#Make Plots
g <- plot_ts_lines(ssb.ts.e, y = 'value', vert = 'D',
  horiz = 'G', horiz2 = 'species', rel = TRUE, color = 'log_max_grad',
  print = TRUE)
ggsave("figs/b_re_length_comps.png", g, width = width, height = height)


g <- plot_ts_lines(ssb.ts.x, y = 'value', vert = 'X',
  horiz = 'G', horiz2 = 'species', rel = TRUE, color = 'log_max_grad', 
  print = TRUE)
ggsave("figs/b_re_wtatage.png", g, width = width, height = height)


plot_sampled_wtatage(scenarios[1])

