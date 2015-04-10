#------------------------------------------------------------------------
#Set results working directory, all stored in drive
setwd('/Users/peterkuriyama/Google Drive/Empirical_results')
# setwd("/Users/peterkuriyama/School/Research/empirical_runs")

# files <- list.files()

#read in scalar files
# sc <- read.csv('ss3sim_scalar.csv', stringsAsFactors = FALSE)
# sc_mac <- read.csv('ss3sim_scalar_mymac.csv', stringsAsFactors = FALSE)
# sc_227 <- read.csv('ss3sim_sc_2015-02-27.csv', stringsAsFactors = FALSE)

sc <- read.csv('ss3sim_sc_2015-03-03.csv')

#read in ts files
# ts <- read.csv('ss3sim_ts.csv', stringsAsFactors = FALSE)
# ts_mac <- read.csv('ss3sim_ts_mymac.csv', stringsAsFactors = FALSE)
# ts_227 <- read.csv('ss3sim_ts_2015-02-27.csv', stringsAsFactors = FALSE)

ts <- read.csv('ss3sim_ts_2015-03-03.csv')

#rbind files
# results.sc <- rbind(sc, sc_mac, sc_227)

# results.sc <- sc
# results.ts <- ts

#Subset only converged runs
# results.sc <- results.sc[results.sc$max_grad < .01, ]

# results.ts <- rbind(ts_mac, ts, ts_227)


#------------------------------------------------------------------------
#plot time-varying growth scenarios
# scenarios <- c(scenarios, scenariosLength)
# source('plotting_functions.r')
# apply(as.data.frame(scenarios), MAR = 1, FUN =  function(x) make_wtatage_png(x))

# #------------------------------------------------------------------------
# #Haven't done this yet
# ##Read in Results
# get_results_all(dir=getwd(), user_scenarios=scenarios, parallel=TRUE, over=TRUE)

# #Move Results to empirical_results folder


# results.ts <- read.csv('ss3sim_ts.csv')
# results.sc <- read.csv('ss3sim_scalar.csv')

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

#Remove Deterministic Case
#Remove non convergent runts
# results.sc.long$grad <- exp(results.sc.long$log_max_grad)
# results.sc.long <- results.sc.long[results.sc.long$grad < .05, ]
# (results.sc.long$log_max_grad < 0)

results.sc.long.growth <- droplevels(subset(results.sc.long, variable %in% growth.names))
results.sc.long.selex <- droplevels(subset(results.sc.long, variable %in% selex.names))
results.sc.long.management <- droplevels(subset(results.sc.long, variable %in% management.names))

#Remove Values that aren't estimated
# results.sc.long.selex <- subset(results.sc.long.selex, value != 0)
# results.sc.long.management <- subset(results.sc.long.management, value != 0)
# results.sc.long.growth <- subset(results.sc.long.selex, value != 0)
# ---------------------------------------
#Check to see if estimation is turned on for scenarios
# check.est <- results.sc.long %>% group_by(scenario, variable) %>% 
#     summarise(length.val = length(unique(value)))
# write.csv(subset(check.est, length.val == 4), file = '/Users/peterkuriyama/Desktop/check_est.csv', 
#     row.names = FALSE)

#---------------------------------------
#Plot HAKE ONLY
results.sc.long.selex <- results.sc.long.selex[grep('hake', results.sc.long.selex$species), ]
results.sc.long.management <- results.sc.long.management[grep('hake', 
  results.sc.long.management$species), ]
results.sc.long.growth <- results.sc.long.growth[grep('hake', 
  results.sc.long.growth$species), ]

#---------------------------------------
#Subset growth, selex, management by hake and hake age
hake.age.selex <- subset(results.sc.long.selex, species == 'hake-age')
hake.age.growth <- subset(results.sc.long.growth, species == 'hake-age')
hake.age.management <- subset(results.sc.long.management, species == 'hake-age')

#Find number of scenarios in each oneh
# head(hake.age.management)
# nmans <- hake.age.management %>% group_by(scenario, variable) %>% summarise(nn = length(value))
# write.csv(nmans, file = 'Convergence_amounts.csv', row.names = FALSE)

#Age management
g <- plot_scalar_boxplot(hake.age.management, x="variable", y='value',
                   horiz='X', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()
ggsave('figs/hake_age_management_box_v2.png', g, width = 9, height = 7)

g <- plot_scalar_points(hake.age.management, x="variable", y='value',
                   horiz='X', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()
ggsave('figs/hake_age_management_points_v2.png', g, width = 9, height = 7)


#Age Selectivity
g <- plot_scalar_points(hake.age.selex, x="variable", y='value',
                   horiz='X', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()

g <- plot_scalar_boxplot(hake.age.selex, x="variable", y='value',
                   horiz='X', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()
ggsave('figs/hake_age_selex_box.png', g, width = 9, height = 7)

#---------------------------------------
#Plot length based
hake.selex <- subset(results.sc.long.selex, species == 'hake')
hake.growth <- subset(results.sc.long.growth, species == 'hake')
hake.management <- subset(results.sc.long.management, species == 'hake')

#Management, length based
g <- plot_scalar_boxplot(hake.management, x="variable", y='value',
                   horiz='D', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()
ggsave('figs/hake_management_box_v2.png', g, width = 9, height = 7)

g <- plot_scalar_points(hake.management, x="variable", y='value',
                   horiz='D', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()
ggsave('figs/hake_management_points_v2.png', g, width = 9, height = 7)


#Selectivity
g <- plot_scalar_boxplot(hake.selex, x="variable", y='value',
                   horiz='D', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()
ggsave('figs/hake_selex_box.png', g, width = 9, height = 7)

g <- plot_scalar_boxplot(hake.growth, x="variable", y='value',
                   horiz='D', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()
ggsave('figs/hake_growth_box.png', g, width = 9, height = 7)


##---------------------------------------
#Plot Time Series
# results.ts <- c(ts_mac, ts)
ts.re.names <- names(results.ts)[grep('_re', names(results.ts))]

temp1 <- merge(results.ts, results.sc[, c('log_max_grad', 'ID')],
  by = 'ID')

results.ts <- temp1

#Biomass results only
ssb.ts.long <- melt(results.ts, measure.vars = 'SpawnBio_re',
  id.vars= c("ID","species", "replicate",
        "year", 'D', 'X', 'G'))
# ssb.ts.long$grad <- exp(ssb.ts.long$log_max_grad)

#Remove non-converged values
# ssb.ts.long <- ssb.ts.long[ssb.ts.long$grad < .01, ]

#Check Gradients for hake age
# check.grads <- ssb.ts.long %>% group_by(ID) %>% summarise(grad = mean(grad))
# check.grads <- as.data.frame(check.grads)
# check.grads[grep('hake-age', check.grads$ID), ]

#Subset data
hake.age.ts <- subset(ssb.ts.long, species == 'hake-age')
hake.age.ts %>% group_by(species, X, G) %>% summarise(nn = length(unique(replicate)))

#By Data amount/Type
#Check G1
# checkss <- hake.age.ts[grep('G1-X1', hake.age.ts$ID), ]

g <- plot_ts_lines(hake.age.ts, y = 'value', vert = 'G',
  horiz = 'D', rel = TRUE) 
  # ggtitle('SSB')
ggsave('figs/hake_age_ts_ssb.png', g, width = 9, height = 7)

hake.ts <- subset(ssb.ts.long, species == 'hake')
g <- plot_ts_lines(hake.ts, y = 'value', vert = 'G',
  horiz = 'D', rel = TRUE) 
ggsave('figs/hake_ts_ssb.png', g, width = 9, height = 7)

#Growth Pattern
# g <- plot_ts_lines(ssb.ts.long, y = 'value', vert = 'G',
#   horiz = 'species', rel = TRUE, color = 'log_max_grad') 
# ggsave('figs/G_ssb.png', g, width = 19, height = 9)

#-----Look at last year relative error in ssb estimates
hake.age.endSSB <- subset(hake.age.ts, year == 100)

g <- plot_scalar_boxplot(hake.age.endSSB, x="variable", y='value',
                   horiz='D', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()
ggsave('figs/end_ssb_hake_age.png', g, width = 19, height = 9)

g <- plot_scalar_points(hake.age.endSSB, x="variable", y='value',
                   horiz='X', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()

#--------
#Get Hake only
hake.ts <- subset(ssb.ts.long, species == 'hake')

g <- plot_ts_lines(hake.ts, y = 'value', vert = 'G',
  horiz = 'D', rel = TRUE) 
  # ggtitle('SSB')
ggsave('figs/hake_ts_ssb.png', g, width = 9, height = 7)

# hake.ts <- subset(ssb.ts.long, species == 'hake')
# g <- plot_ts_lines(hake.ts, y = 'value', vert = 'G',
#   horiz = 'D', rel = TRUE) 
# ggsave('figs/hake_ts_ssb.png', g, width = 9, height = 7)

#End SSB
hake.endSSB <- subset(hake.ts, year == 100)

g <- plot_scalar_boxplot(hake.endSSB, x="variable", y='value',
                   horiz='D', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()
ggsave('figs/hake_end_ssb_box.png', g, width = 9, height = 7)

g <- plot_scalar_points(hake.endSSB, x="variable", y='value',
                   horiz='D', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()
ggsave('figs/hake_end_ssb_points.png', g, width = 9, height = 7)


#Find how many convergences we got. 
# as.data.frame(hake.age.endSSB %>% group_by(species, D, X, G) %>% summarise(nn = length(value)))


# #By Data amount and type
# g <- plot_scalar_points(end.ssb, x = 'variable', y = 'value',
#   horiz = 'species', rel = TRUE, color = 'log_max_grad', 
#   vert = 'D')
# ggsave('figs/D_end_ssb_pt.png', g, width = 7, height = 9)






#---------------------------------------
#Check relative errors
#Evaluate Growth Estimation - Not interested in this

#Only plot values with non-zero relative error

#Plot SELECTIVITY
#By Data amount
g <- plot_scalar_points(results.sc.long.selex, x="variable", y='value',
                   horiz='species', vert = "D", rel=TRUE, color='log_max_grad') + 
    theme(axis.text.x = element_text(angle = 90)) + theme_bw()
ggsave('figs/D_sc_selex_pt.png', g, width = 9, height = 7)

g <- plot_scalar_boxplot(results.sc.long.selex, x="variable", y='value',
                   horiz='species', vert = "D", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/D_sc_selex_box.png', g, width = 9, height = 7)

#By Growth Pattern
g <- plot_scalar_points(results.sc.long.selex, x="variable", y='value',
                   horiz='species', vert = "G", rel=TRUE, color='log_max_grad') + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/G_sc_selex_pt.png', g, width = 9, height = 7)

g <- plot_scalar_boxplot(results.sc.long.selex, x="variable", y='value',
                   horiz='species', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/G_sc_selex_box.png', g, width = 9, height = 7)

#By Wtatage
g <- plot_scalar_points(results.sc.long.selex, x="variable", y='value',
                   horiz='species', vert = "X", rel=TRUE, color='log_max_grad') + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/X_sc_selex_pt.png', g, width = 9, height = 7)

g <- plot_scalar_boxplot(results.sc.long.selex, x="variable", y='value',
                   horiz='species', vert = "X", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/X_sc_selex_box.png', g, width = 9, height = 7)

#Plot MANAGEMENT REFERENCE POINTS
#By data Amount
g <- plot_scalar_points(results.sc.long.management, x="variable", y='value',
                   horiz='species', vert = "D", rel=TRUE, color='log_max_grad') + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/D_sc_management_pt.png', g, width = 9, height = 7)

g <- plot_scalar_boxplot(results.sc.long.management, x="variable", y='value',
                   horiz='species', vert = "D", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/D_sc_management_box.png', g, width = 9, height = 7)

#By Growth Pattern
g <- plot_scalar_points(results.sc.long.management, x="variable", y='value',
                   horiz='species', vert = "G", rel=TRUE, color='log_max_grad') + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/G_sc_management_pt.png', g, width = 9, height = 7)

g <- plot_scalar_boxplot(results.sc.long.management, x="variable", y='value',
                   horiz='species', vert = "G", rel=TRUE) + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/G_sc_management_box.png', g, width = 9, height = 7)


#By wtatage
g <- plot_scalar_points(results.sc.long.management, x="variable", y='value',
                   horiz='species', vert = "X", rel=TRUE, color='log_max_grad') + 
    theme(axis.text.x = element_text(angle = 90))
ggsave('figs/X_sc_management_pt.png', g, width = 9, height = 7)



##---------------------------------------
#Plot Time Series
ts.re.names <- names(results.ts)[grep('_re', names(results.ts))]

results.ts <- merge(results.ts, results.sc[, c('log_max_grad', 'ID')],
  by = 'ID', all = TRUE)

#Biomass results only
ssb.ts.long <- melt(results.ts, measure.vars = 'SpawnBio_re',
  id.vars= c("ID","species", "replicate",
         "log_max_grad", "year", 'D', 'X', 'G'))

#By Data amount/Type
g <- plot_ts_lines(ssb.ts.long, y = 'value', vert = 'D',
  horiz = 'species', rel = TRUE, color = 'log_max_grad') 
  # ggtitle('SSB')
ggsave('figs/D_ssb.png', g, width = 9, height = 7)

#Growth Pattern
g <- plot_ts_lines(ssb.ts.long, y = 'value', vert = 'G',
  horiz = 'species', rel = TRUE, color = 'log_max_grad') 
ggsave('figs/G_ssb.png', g, width = 19, height = 9)

#-----Look at last year relative error in ssb estimates
end.ssb <- subset(ssb.ts.long, year == 100)

#By Data amount and type
g <- plot_scalar_points(end.ssb, x = 'variable', y = 'value',
  horiz = 'species', rel = TRUE, color = 'log_max_grad', 
  vert = 'D')
ggsave('figs/D_end_ssb_pt.png', g, width = 7, height = 9)

g <- plot_scalar_boxplot(end.ssb, x = 'variable', y = 'value',
  horiz = 'species', rel = TRUE, vert = 'D')
ggsave('figs/D_end_ssb_box.png', g, width = 7, height = 9)

#By growth pattern
g <- plot_scalar_points(end.ssb, x = 'variable', y = 'value',
  horiz = 'species', rel = TRUE, color = 'log_max_grad', 
  vert = 'G')
ggsave('figs/G_end_ssb_pt.png', g, width = 7, height = 9)

g <- plot_scalar_boxplot(end.ssb, x = 'variable', y = 'value',
  horiz = 'species', rel = TRUE, vert = 'G')
ggsave('figs/G_end_ssb_box.png', g, width = 7, height = 9)


# #Exploratory Boxplots, will be more informative with more information
# z <- plot_scalar_boxplot(results.sc.long.management, x = 'variable', y = 'value', 
#     horiz = 'species', vert = 'D', rel = TRUE) + theme(axis.text.x = element_text(angle = 90))

# z <- plot_scalar_boxplot(results.sc.long.management, x = 'variable', y = 'value', 
#     horiz = 'species', vert = 'G', rel = TRUE) + theme(axis.text.x = element_text(angle = 90))


# subset(results.sc.long.growth, )

# g <- plot_scalar_points(results.sc.long.growth, x="variable", y='value',
#                    horiz='species', vert = "D", rel=TRUE, color='log_max_grad')
#     # theme(axis.text.x=element_text(angle=90))
# g <- plot_scalar_points(results.sc.long.growth, x="variable", y='value',
#                    horiz='species', vert = "G", rel=TRUE, color='log_max_grad')




