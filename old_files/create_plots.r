
ggwidth <- 9
ggheight <- 7
# library(ggplot2)
# library(plyr)

### For effect binning
  g <- plot_scalar_boxplot(results.sc.long.growth, x="variable", y='value',
               vert2='species', vert="D", rel=TRUE, horiz = "G",
               print=FALSE) +
    theme(axis.text.x=element_text(angle=90))
  ggsave("plots/growth_errors.png",g, width=ggwidth, height=ggheight)
  

  g <- plot_scalar_boxplot(results.sc.long.selex, x="variable", y='value', vert2='species', vert="D", 
    rel=TRUE, horiz = "G", print=FALSE)+
    theme(axis.text.x=element_text(angle=90))
  ggsave("plots/selex_errors.png", g, width=ggwidth, height=ggheight)



  g <- plot_scalar_boxplot(results.sc.long.management, x="variable",
      y='value', vert2='species', vert="D", rel=TRUE, horiz = "G",
               print=FALSE)+
    theme(axis.text.x=element_text(angle=90))
  ggsave("plots/management_errors.png",g, width=ggwidth, height=ggheight)






# #calc relative error in spawnbio 
# results.ts$spawnbio_re <- (results.ts$SpawnBio_em - results.ts$SpawnBio_om) / results.ts$SpawnBio_om

# # temp <- subset(results.ts, scenario == 'D1-F0-G0-X1-hake-age')
# # plot(temp$year, temp$spawnbio_re, pch = 19)


# ggplot(results.ts, aes(x = year, y = spawnbio_re)) + geom_boxplot() + 
#     facet_wrap( ~ scenario)




#   g <- ggplot(results.sc, aes(x=D, y=log_max_grad, color=runtime, size=params_on_bound_em,))+
#     geom_jitter()+
#     facet_grid(species~dat.bin+pop.bin)+
#       geom_hline(yintercept=log(.01), col='red')
#   ggsave("plots/convergence.png",g, width=ggwidth, height=ggheight)
#   g <- ggplot(results.sc, aes(x=D, y=runtime, size=params_on_bound_em, color=converged))+
#     geom_jitter()+ ylab("Runtime (minutes)")+
#     facet_grid(species~dat.bin+pop.bin)
#   ggsave("plots/runtime.png",g, width=ggwidth, height=ggheight)
#   ## table of convergence
#   plyr::ddply(results.sc.long, .(species, D, B), summarize,
#         median.logmaxgrad=round(median(log_max_grad),2),
#         max.stuck.on.bounds=max(params_on_bound_em))

      
# ##### For tail compression

#   g <- plot_scalar_boxplot(results.sc.tcomp.long.growth, x="variable", y='value',
#                vert2='species', vert="dat.bin", rel=TRUE,
#                horiz="tcomp", print=FALSE) +
#     theme(axis.text.x=element_text(angle=90))
#   ggsave("plots/growth_errors_tcomp.png",g, width=ggwidth, height=ggheight)
#   g <- plot_scalar_boxplot(results.sc.tcomp.long.selex, x="variable", y='value', vert2='species', vert="dat.bin", rel=TRUE, horiz="tcomp", print=FALSE)+
#     theme(axis.text.x=element_text(angle=90))
#   ggsave("plots/selex_errors_tcomp.png", g, width=ggwidth, height=ggheight)
#   g <- plot_scalar_boxplot(results.sc.tcomp.long.management, x="variable",
#       y='value', vert2='species', vert="dat.bin", rel=TRUE,
#                horiz="tcomp", print=FALSE)+
#     theme(axis.text.x=element_text(angle=90))
#   ggsave("plots/management_errors_tcomp.png",g, width=ggwidth, height=ggheight)
#   g <- ggplot(results.sc.tcomp, aes(x=dat.bin, y=log_max_grad, color=runtime, size=params_on_bound_em,))+
#     geom_jitter()+
#     facet_grid(species~dat.bin+tcomp)+
#       geom_hline(yintercept=log(.01), col='red')
#   ggsave("plots/convergence_tcomp.png",g, width=ggwidth, height=ggheight)
#   g <- ggplot(results.sc.tcomp, aes(x=dat.bin, y=runtime, size=params_on_bound_em, color=converged))+
#     geom_jitter()+ ylab("Runtime (minutes)")+
#     facet_grid(species~dat.bin+tcomp)
#   ggsave("plots/runtime_tcomp.png",g, width=ggwidth, height=ggheight)
#   ## table of convergence
#   plyr::ddply(results.sc.tcomp.long, .(species, dat.bin, B), summarize,
#         median.logmaxgrad=round(median(log_max_grad),2),
#         max.stuck.on.bounds=max(params_on_bound_em))

        
      
# ##### For robustification constant

#   g <- plot_scalar_boxplot(results.sc.robust.long.growth, x="variable", y='value',
#                vert2='species', vert="dat.bin", rel=TRUE,
#                horiz="robust", print=FALSE) +
#     theme(axis.text.x=element_text(angle=90))
#   ggsave("plots/growth_errors_robust.png",g, width=ggwidth, height=ggheight)
#   g <- plot_scalar_boxplot(results.sc.robust.long.selex, x="variable", y='value', vert2='species', vert="dat.bin", rel=TRUE, horiz="robust", print=FALSE)+
#     theme(axis.text.x=element_text(angle=90))
#   ggsave("plots/selex_errors_robust.png", g, width=ggwidth, height=ggheight)
#   g <- plot_scalar_boxplot(results.sc.robust.long.management, x="variable",
#       y='value', vert2='species', vert="dat.bin", rel=TRUE, 
#                horiz="robust", print=FALSE)+
#     theme(axis.text.x=element_text(angle=90))
#   ggsave("plots/management_errors_robust.png",g, width=ggwidth, height=ggheight)
#   g <- ggplot(results.sc.robust, aes(x=dat.bin, y=log_max_grad, color=runtime, size=params_on_bound_em,))+
#     geom_jitter()+
#     facet_grid(species~dat.bin+robust)+
#       geom_hline(yintercept=log(.01), col='red')
#   ggsave("plots/convergence_robust.png",g, width=ggwidth, height=ggheight)
#   g <- ggplot(results.sc.robust, aes(x=dat.bin, y=runtime, size=params_on_bound_em, color=converged))+
#     geom_jitter()+ ylab("Runtime (minutes)")+
#     facet_grid(species~dat.bin+robust)
#   ggsave("plots/runtime_robust.png",g, width=ggwidth, height=ggheight)
#   ## table of convergence
#   plyr::ddply(results.sc.robust.long, .(species, dat.bin, B), summarize,
#         median.logmaxgrad=round(median(log_max_grad),2),
#         max.stuck.on.bounds=max(params_on_bound_em))

#         
