
#------------------------------------------------------------------------
#plot time-varying growth scenarios
scenarios <- c(scenarios, scenariosLength)
source('plotting_functions.r')
apply(as.data.frame(scenarios), MAR = 1, FUN =  function(x) make_wtatage_png(x))

#------------------------------------------------------------------------
#Haven't done this yet
##Read in Results
get_results_all(dir=getwd(), user_scenarios=scenarios, parallel=TRUE, over=TRUE)

#Move Results to empirical_results folder




results.ts <- read.csv('ss3sim_ts.csv')
results.sc <- read.csv('ss3sim_scalar.csv')

#---------------------------------------
#Change so results.ts and results.sc handle hyphens 
#Time series results
age.ind <- grep('age', results.ts$scenario)
results.ts$scenario <- as.character(results.ts$scenario)
results.ts$species <- as.character(results.ts$species)

spsp <- ldply(strsplit(results.ts[age.ind, 'scenario'], '-'))
results.ts[age.ind, 'species'] <- paste(spsp$V5, spsp$V6, sep = '-')

#scalar results
age.ind <- grep('age', results.sc$scenario)
results.sc$scenario <- as.character(results.sc$scenario)
results.sc$species <- as.character(results.sc$species)

spsp <- ldply(strsplit(results.sc[age.ind, 'scenario'], '-'))
results.sc[age.ind, 'species'] <- paste(spsp$V5, spsp$V6, sep = '-')


#---------------------------------------
results.sc <- calculate_re(results.sc, add = TRUE)
results.ts <- calculate_re(results.ts, add=TRUE)

results.sc$log_max_grad <- log(results.sc$max_grad)
re.names <- names(results.sc)[grep("_re", names(results.sc))]

growth.names <- re.names[grep("GP_", re.names)]
growth.names <- c(growth.names, 'D', 'G', 'X', 'scenario')
selex.names <- re.names[grep("Sel_", re.names)]
selex.names <- c(selex.names, 'D', 'G', 'X', 'scenario')
management.names <- c("SSB_MSY_re", "depletion_re", "SSB_Unfished_re", "Catch_endyear_re")
management.names <- c(management.names, 'D', 'G', 'X', 'scenario')

results.sc.long <-
    melt(results.sc, measure.vars=re.names,
         id.vars= c("species", "replicate",
         "log_max_grad", "params_on_bound_em", 'D', 'G', 'X', 'scenario'))


results.sc.long.growth <- droplevels(subset(results.sc.long, variable %in% growth.names))
results.sc.long.selex <- droplevels(subset(results.sc.long, variable %in% selex.names))

#Remove Zeroes
results.sc.long.selex <- subset(results.sc.long.selex, value != 0)

results.sc.long.management <- droplevels(subset(results.sc.long, variable %in% management.names))
results.sc.long.management <- subset(results.sc.long.management, value != 0)
#---------------------------------------
#Check to see if estimation is turned on for scenarios
# check.est <- results.sc.long %>% group_by(scenario, variable) %>% 
#     summarise(length.val = length(unique(value)))
# write.csv(subset(check.est, length.val == 4), file = '/Users/peterkuriyama/Desktop/check_est.csv', 
#     row.names = FALSE)

#---------------------------------------
#Check relative errors
#Evaluate Growth Estimation - Not interested in this

#Only plot values with non-zero relative error

#Plot SELECTIVITY
g <- plot_scalar_points(results.sc.long.selex, x="variable", y='value',
                   horiz='species', vert = "D", rel=TRUE, color='log_max_grad') + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/D_sc_selex.png', g, width = 9, height = 7)

g <- plot_scalar_points(results.sc.long.selex, x="variable", y='value',
                   horiz='species', vert = "G", rel=TRUE, color='log_max_grad') + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/G_sc_selex.png', g, width = 9, height = 7)

#Plot MANAGEMENT REFERENCE POINTS
g <- plot_scalar_points(results.sc.long.management, x="variable", y='value',
                   horiz='species', vert = "D", rel=TRUE, color='log_max_grad') + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/D_sc_management.png', g, width = 9, height = 7)

g <- plot_scalar_points(results.sc.long.management, x="variable", y='value',
                   horiz='species', vert = "G", rel=TRUE, color='log_max_grad') + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/G_sc_management.png', g, width = 9, height = 7)


#Exploratory Boxplots, will be more informative with more information
z <- plot_scalar_boxplot(results.sc.long.management, x = 'variable', y = 'value', 
    horiz = 'species', vert = 'D', rel = TRUE) + theme(axis.text.x = element_text(angle = 90))

z <- plot_scalar_boxplot(results.sc.long.management, x = 'variable', y = 'value', 
    horiz = 'species', vert = 'G', rel = TRUE) + theme(axis.text.x = element_text(angle = 90))




subset(results.sc.long.growth, )

g <- plot_scalar_points(results.sc.long.growth, x="variable", y='value',
                   horiz='species', vert = "D", rel=TRUE, color='log_max_grad')
    # theme(axis.text.x=element_text(angle=90))
g <- plot_scalar_points(results.sc.long.growth, x="variable", y='value',
                   horiz='species', vert = "G", rel=TRUE, color='log_max_grad')







#calc relative error in spawnbio 
results.ts$spawnbio_re <- (results.ts$SpawnBio_em - results.ts$SpawnBio_om) / results.ts$SpawnBio_om

temp <- subset(results.ts, scenario == 'D1-F0-G0-X1-hake-age')
plot(temp$year, temp$spawnbio_re, pch = 19)


ggplot(results.ts, aes(x = year, y = spawnbio_re)) + geom_boxplot() + 
    facet_wrap( ~ scenario)



results <- read.csv("results/bin_fla_scalar.csv")
results <- within(results,{
    CV_old_re <- (CV_old_Fem_GP_1_em-CV_old_Fem_GP_1_om)/CV_old_Fem_GP_1_om
    CV_young_re <- (CV_young_Fem_GP_1_em-CV_young_Fem_GP_1_om)/CV_young_Fem_GP_1_om
    L_at_Amin_re <- (L_at_Amin_Fem_GP_1_em-L_at_Amin_Fem_GP_1_om)/L_at_Amin_Fem_GP_1_om
    L_at_Amax_re <- (L_at_Amax_Fem_GP_1_em-L_at_Amax_Fem_GP_1_om)/L_at_Amax_Fem_GP_1_om
    VonBert_K_re <- (VonBert_K_Fem_GP_1_em-VonBert_K_Fem_GP_1_om)/VonBert_K_Fem_GP_1_om
    depletion_re <- (depletion_em-depletion_om)/depletion_om
})
results_re <- calculate_re(results, FALSE)
results_re <- results[, grep("_re", names(results))]
results_re$B <- results$B;results_re$D <- results$D;results_re$E <- results$E
results_re$replicate <- results$replicate
results_long <- reshape2::melt(results_re, c("E","B", "D","replicate"))
results_long <- merge(scen.df, results_long)
results_long$B.value <- factor(results_long$B.value, levels=bin.seq)
## Make exploratory plots
library(ggplot2)
plot_scalar_boxplot(subset(results_long, E=="E0"), x="B.value", y="value", vert="variable", horiz2="D",
                   horiz="E", rel=F, axes.free=TRUE) + xlab("# of length bins") +
                                         ylab("relative error")+ ylim(-.7, .-3)
ggsave("plots/bin_fla.png", width=9, height=5)








#Check Relative error in biomass trajectories
#Errors in selectivity
#Check Cole's email

get_results_all(dir = getwd(), user_scenarios = scenarios)


results <- read.csv('ss3sim_scalar.csv')
results <- within(results,{
    CV_old_re <- (CV_old_Fem_GP_1_em-CV_old_Fem_GP_1_om)/CV_old_Fem_GP_1_om
    CV_young_re <- (CV_young_Fem_GP_1_em-CV_young_Fem_GP_1_om)/CV_young_Fem_GP_1_om
    L_at_Amin_re <- (L_at_Amin_Fem_GP_1_em-L_at_Amin_Fem_GP_1_om)/L_at_Amin_Fem_GP_1_om
    L_at_Amax_re <- (L_at_Amax_Fem_GP_1_em-L_at_Amax_Fem_GP_1_om)/L_at_Amax_Fem_GP_1_om
    VonBert_K_re <- (VonBert_K_Fem_GP_1_em-VonBert_K_Fem_GP_1_om)/VonBert_K_Fem_GP_1_om
    depletion_re <- (depletion_em-depletion_om)/depletion_om
    SSB_MSY_re <- (SSB_MSY_em - SSB_MSY_om) / SSB_MSY_om
    SSB_Unfished_re <- (SSB_Unfished_em - SSB_Unfished_om) / SSB_Unfished_om
    max_grad_re <- max_grad
})
results_re <- calculate_re(results, FALSE)

calculate_re(read.csv('ss3sim_scalar.csv'), add = T)


#------------------------------------------------------------------------


