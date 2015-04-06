#This file is the central plae to develop and run analyses for the paper
#Sources files that do particular tasks

#------------------------------------------------------------------------
#Set Working Directory
setwd('/Volumes/home/Empirical/')
# setwd("Y:\\Empirical\\")

#Set working directory for results
results.dir <- "/Users/peterkuriyama/Desktop/test_runs"
# results.dir <- "c:\\Users\\ptrkrym\\Desktop\\test_runs"

#Paths are structured as they are in the ss3sim repo
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)

#------------------------------------------------------------------------
#Set Up Parallels and Register cores
library('doParallel')
registerDoParallel(cores = 4)
library('foreach')
message(paste(getDoParWorkers(), "cores have been registered for",
    "parallel processing."))

#------------------------------------------------------------------------
library(devtools)
devtools::install('../r4ss')
devtools::install('../ss3sim')
devtools::install('../ss3models')

#clone ss3sim and ss3models locally and load_all
#**Make sure both repos are up to date**#
# load_all('../ss3sim')
# load_all('../ss3models')

library(r4ss)
library(ss3sim)
library(ss3models)

#------------------------------------------------------------------------
#Copy r scripts to results directory

#That write case files
#write_casefiles writes the agecomp case files, 
#write_tv_cases writes time varying growth case files, "G"

#If false, check is .r is lowercase or uppercase

#This will overwrite any file in your results directory.
#Make changes to the file in the "Empirical" directory.
file.copy(paste0(getwd(), '/write_casefiles.r'),
            paste0(results.dir, '/write_casefiles.r'),
            overwrite = TRUE)

file.copy(paste0(getwd(), '/write_tv_cases.r'),
            paste0(results.dir, '/write_tv_cases.r'),
            overwrite = TRUE)

#scripts to format results and plot
file.copy(paste0(getwd(), '/load_results.r'),
            paste0(results.dir, '/load_results.r'),
            overwrite = TRUE)


#------------------------------------------------------------------------
#Set results directory as working directory
setwd(results.dir)

#------------------------------------------------------------------------
#Createn Case Files Dynamically for reproducibility

###Define species models that you want to run.
species.vec <- c('yellow-age', 'hake-age')

#Remove Existing cases folder
unlink('cases', recursive = TRUE)

#Make cases folder
dir.create(file.path(results.dir, 'cases'), recursive = FALSE)

for(ii in 1:length(species.vec))
{
    species <- species.vec[ii]
    
    #Create case directory and specify folder
    dir.create(file.path(results.dir, paste0('cases', '/', species)))
    case_folder <- paste0('cases', '/', species)
    source('write_casefiles.r')
    source('write_tv_cases.r')
}

#------------------------------------------------------------------------
#Run ss3sim 

#Globally set iterations to run
iters <- 57:112

####################################
#Age-Based Scenarios, "X = 1"
#D and X numbers have to be identical
scens1 <- expand_scenarios(cases = list(F = 0, D = 1, X = 1,
    G = 0:1), species = species.vec)
scens2 <- expand_scenarios(cases = list(F = 0, D = 2, X = 2,
    G = 0:1), species = species.vec)
scens3 <- expand_scenarios(cases = list(F = 0, D = 3, X = 3,
    G = 0:1), species = species.vec)
scens4 <- expand_scenarios(cases = list(F = 0, D = 4, X = 4,
    G = 0:1), species = species.vec)

# scenariosW <- c(scens1, scens2, scens3, scens4)
scenariosW <- c(scens2, scens3, scens4)

#Specify Case Files
case_files <- list(F = 'F', D = c('index', 'agecomp'), X = 'wtatage', 
    G = 'G')

#Run scenarios in for loop
for(ii in 1:length(scenariosW))
{
    #Parse out "-age" species
    find.spp <- scenariosW[ii]
    parsed <- strsplit(find.spp, '-')[[1]]
    spp <- parsed[length(parsed)]

    if(length(grep('age', parsed)) == 1){
        spp <- paste0(parsed[length(parsed) - 1], '-', parsed[length(parsed)])
    }
    
    #Define case_folder location
    case_folder1 <- paste0('cases', '/', spp)

    run_ss3sim(iterations = iters, scenarios = scenariosW[ii], case_folder = case_folder1,
        om_dir = ss3model(spp, 'om'), em_dir = ss3model(spp, 'em'),
        case_files = case_files, parallel = TRUE, parallel_iterations = TRUE)
}

####################################
#Length-Based Scenarios
scenariosL <- expand_scenarios(cases = list(F = 0, D = 2:4,
    G = 0:1, E = 2), species = species.vec)

#Define length_based case files, No X cases
case_files <- list(F = 'F', D = c('index', 'agecomp', 'lcomp'),
    G = 'G', E = 'E')

#Run scenarios in Loop
for(ii in 1:length(scenariosL))
{
    find.spp <- scenariosL[ii]
    parsed <- strsplit(find.spp, '-')[[1]]
    spp <- parsed[length(parsed)]

    if(length(grep('age',  parsed)) == 1){
        spp <- paste0(parsed[length(parsed) - 1], '-', parsed[length(parsed)])
    }
    
    case_folder1 <- paste0('cases', '/', spp)

    run_ss3sim(iterations = iters, scenarios = scenariosL[ii], case_folder = case_folder1,
        om_dir = ss3model(spp, 'om'), em_dir = ss3model(spp, 'em'),
        case_files = case_files, parallel = TRUE, parallel_iterations = TRUE)
}

scenarios <- c(scenariosW, scenariosL)

#------------------------------------------------------------------------
#Read in  Results
get_results_all(dir = getwd(), user_scenarios = scenarios, 
                parallel = TRUE, over = TRUE)

results.sc <- read.csv('ss3sim_scalar_lab.csv')
results.ts <- read.csv('ss3sim_ts_lab.csv')

save(results.sc, file =  'ss3sim_scalar_1_56.Rdata')
save(results.ts, file =  'ss3sim_ts_1_56.Rdata')

#------------------------------------------------------------------------
source('load_results.r')
source('plotting_functions.r')

width <- 12
height <- 8
source('make_plots.r')

##---------------------------------------
#Plot Time Series


# results.ts <- merge(results.ts, results.sc[, c('log_max_grad', 'ID')],
#   by = 'ID', all = TRUE)

# #Biomass results only
# ssb.ts.long <- melt(results.ts, measure.vars = 'spawnbio_re',
#   id.vars= c("ID","species", "replicate",
#          "log_max_grad", "year", 'D', 'X', 'G', 'E'))


# ssb.ts.e <- subset(ssb.ts.long, E == "E2")


# ssb.ts.w <- subset(ssb.ts.long, X == '')
# #By Data amount/Type
# g <- plot_ts_lines(ssb.ts.long, y = 'value', vert = 'D',
#   horiz = 'G', rel = TRUE, color = 'log_max_grad') 
#   # ggtitle('SSB')
# ggsave('figs/D_ssb.png', g, width = 9, height = 7)

# #Growth Pattern
# g <- plot_ts_lines(ssb.ts.long, y = 'value', vert = 'G',
#   horiz = 'species', rel = TRUE, color = 'log_max_grad') 
# ggsave('figs/G_ssb.png', g, width = 19, height = 9)

# #-----Look at last year relative error in ssb estimates
# end.ssb <- subset(ssb.ts.long, year == 100)

# #By Data amount and type
# g <- plot_scalar_points(end.ssb, x = 'variable', y = 'value',
#   horiz = 'species', rel = TRUE, color = 'log_max_grad', 
#   vert = 'D')
# ggsave('figs/D_end_ssb_pt.png', g, width = 7, height = 9)

# g <- plot_scalar_boxplot(end.ssb, x = 'variable', y = 'value',
#   horiz = 'species', rel = TRUE, vert = 'D')
# ggsave('figs/D_end_ssb_box.png', g, width = 7, height = 9)

# #By growth pattern
# g <- plot_scalar_points(end.ssb, x = 'variable', y = 'value',
#   horiz = 'species', rel = TRUE, color = 'log_max_grad', 
#   vert = 'G')
# ggsave('figs/G_end_ssb_pt.png', g, width = 7, height = 9)

# g <- plot_scalar_boxplot(end.ssb, x = 'variable', y = 'value',
#   horiz = 'species', rel = TRUE, vert = 'G')
# ggsave('figs/G_end_ssb_box.png', g, width = 7, height = 9)







# g <- plot_scalar_boxplot(results.sc.long.growth, x="variable", y='value',
#                          vert2='species', vert="D", rel=TRUE,
#                          horiz2="dat.bin", horiz="pop.bin", print=FALSE) +
#     theme(axis.text.x=element_text(angle=90))




# ## Source this data to load in the results and do any processing before
# ## making figures and tables.

# ## make a table with better names for merging into the main results; used
# ## really only for plotting and needs to be specific for each species


# ## Drop fixed params (columns of zeroes)
# results.sc$RecrDist_GP_1_re <- NULL
# # results.sc <- results.sc[,-which(apply(results.sc, 2, function(x) all(x==0)))]
# # re.names <- names(results.sc)[grep("_re", names(results.sc))]
# # results.sc.long <-
# #     melt(results.sc, measure.vars=re.names, id.vars=
# #          c("species","replicate", "D", "B", "dat.bin",
# #            "pop.bin", "log_max_grad", "params_on_bound_em",
# #            "runtime"))
# growth.names <- re.names[grep("GP_", re.names)]
# results.sc.long.growth <- droplevels(subset(results.sc.long, variable %in% growth.names))
# results.sc.long.growth$variable <- gsub("_Fem_GP_1_re|_re", "", results.sc.long.growth$variable)
# selex.names <- re.names[grep("Sel_", re.names)]
# results.sc.long.selex <- droplevels(subset(results.sc.long, variable %in% selex.names))
# results.sc.long.selex$variable <- gsub("ery|ey|Size|_re", "", results.sc.long.selex$variable)
# results.sc.long.selex$variable <- gsub("_", ".", results.sc.long.selex$variable)
# management.names <- c("SSB_MSY_re", "depletion_re", "SSB_Unfished_re", "Catch_endyear_re")
# results.sc.long.management <- droplevels(subset(results.sc.long, variable %in% management.names))
# results.sc.long.management$variable <- gsub("_re", "", results.sc.long.management$variable)











# ts.name <- paste0("ss3sim_ts", "_",Sys.Date(), ".csv")
# # copy.name <- paste0("/Users/peterkuriyama/Google Drive/Empirical_results/", ts.name)

# # copy.name <- paste0("C://Users//Peter//Google Drive//Empirical_results//", ts.name)

# file.copy("ss3sim_ts.csv", copy.name)

# sc.name <- paste0("ss3sim_sc", "_",Sys.Date(), ".csv")
# copy.name <- paste0("/Users/peterkuriyama/Google Drive/Empirical_results/", sc.name)
# file.copy("ss3sim_scalar.csv", copy.name)

# save.image()
# rm(list=ls())



#------------------------------------------------------------------------
# #plot time-varying growth scenarios
#scenarios <- c(scenarios, scenariosLength)
# source('plotting_functions.r')
# apply(as.data.frame(scenarios), MAR = 1, FUN =  function(x) make_wtatage_png(x))



# #------------------------------------------------------------------------
# #Haven't done this yet
# ##Read in Results
# get_results_all(dir=getwd(), user_scenarios=scenarios, parallel=TRUE, over=TRUE)

# #Move Results to empirical_results folder
#scenarios <- list.files()[grep('D', list.files())]

#make plots of sampled wtatage
# for(ii in 1:length(scenarios))
# {
  
#     plot_sampled_wtatage(scenarios[ii])
# }
# scenarios



# # results.ts <- read.csv('ss3sim_ts.csv')
# # results.sc <- read.csv('ss3sim_scalar.csv')

# # #---------------------------------------
# # #Change so results.ts and results.sc handle hyphens 
# # #Time series results
# age.ind <- grep('age', results.ts$scenario)
# results.ts$scenario <- as.character(results.ts$scenario)
# results.ts$species <- as.character(results.ts$species)

# spsp <- ldply(strsplit(results.ts[age.ind, 'scenario'], '-'))
# results.ts[age.ind, 'species'] <- paste(spsp$V5, spsp$V6, sep = '-')

# #scalar results
# age.ind <- grep('age', results.sc$scenario)
# results.sc$scenario <- as.character(results.sc$scenario)
# results.sc$species <- as.character(results.sc$species)

# spsp <- ldply(strsplit(results.sc[age.ind, 'scenario'], '-'))
# results.sc[age.ind, 'species'] <- paste(spsp$V5, spsp$V6, sep = '-')


# #---------------------------------------
# results.sc <- calculate_re(results.sc, add = TRUE)
# results.ts <- calculate_re(results.ts, add=TRUE)

# results.sc$log_max_grad <- log(results.sc$max_grad)
# re.names <- names(results.sc)[grep("_re", names(results.sc))]

# growth.names <- re.names[grep("GP_", re.names)]
# growth.names <- c(growth.names, 'D', 'G', 'X', 'scenario')
# selex.names <- re.names[grep("Sel_", re.names)]
# selex.names <- c(selex.names, 'D', 'G', 'X', 'scenario')
# management.names <- c("SSB_MSY_re", "depletion_re", "SSB_Unfished_re", "Catch_endyear_re")
# management.names <- c(management.names, 'D', 'G', 'X', 'scenario')

# results.sc.long <-
#     melt(results.sc, measure.vars=re.names,
#          id.vars= c("species", "replicate",
#          "log_max_grad", "params_on_bound_em", 'D', 'G', 'X', 'scenario'))


# results.sc.long.growth <- droplevels(subset(results.sc.long, variable %in% growth.names))
# results.sc.long.selex <- droplevels(subset(results.sc.long, variable %in% selex.names))

# #Remove Zeroes
# results.sc.long.selex <- subset(results.sc.long.selex, value != 0)

# results.sc.long.management <- droplevels(subset(results.sc.long, variable %in% management.names))
# results.sc.long.management <- subset(results.sc.long.management, value != 0)
# #---------------------------------------
# #Check to see if estimation is turned on for scenarios
# # check.est <- results.sc.long %>% group_by(scenario, variable) %>% 
# #     summarise(length.val = length(unique(value)))
# # write.csv(subset(check.est, length.val == 4), file = '/Users/peterkuriyama/Desktop/check_est.csv', 
# #     row.names = FALSE)

# #---------------------------------------
# #Check relative errors
# #Evaluate Growth Estimation - Not interested in this

# #Only plot values with non-zero relative error

# #Plot SELECTIVITY
# g <- plot_scalar_points(results.sc.long.selex, x="variable", y='value',
#                    horiz='species', vert = "D", rel=TRUE, color='log_max_grad') + 
#     theme(axis.text.x = element_text(angle = 90))
# ggsave('figs/D_sc_selex.png', g, width = 9, height = 7)

# g <- plot_scalar_points(results.sc.long.selex, x="variable", y='value',
#                    horiz='species', vert = "G", rel=TRUE, color='log_max_grad') + 
#     theme(axis.text.x = element_text(angle = 90))
# ggsave('figs/G_sc_selex.png', g, width = 9, height = 7)

# #Plot MANAGEMENT REFERENCE POINTS
# g <- plot_scalar_points(results.sc.long.management, x="variable", y='value',
#                    horiz='species', vert = "D", rel=TRUE, color='log_max_grad') + 
#     theme(axis.text.x = element_text(angle = 90))
# ggsave('figs/D_sc_management.png', g, width = 9, height = 7)

# g <- plot_scalar_points(results.sc.long.management, x="variable", y='value',
#                    horiz='species', vert = "G", rel=TRUE, color='log_max_grad') + 
#     theme(axis.text.x = element_text(angle = 90))
# ggsave('figs/G_sc_management.png', g, width = 9, height = 7)


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







# # #calc relative error in spawnbio 
# results.ts$spawnbio_re <- (results.ts$SpawnBio_em - results.ts$SpawnBio_om) / results.ts$SpawnBio_om
# ggplot(results.ts, aes(x = year, y = spawnbio_re)) +  + geom_line()


# # temp <- subset(results.ts, scenario == 'D1-F0-G0-X1-hake-age')
# # plot(temp$year, temp$spawnbio_re, pch = 19)


# ggplot(results.ts, aes(x = year, y = spawnbio_re)) + geom_boxplot() + 
#     facet_wrap( ~ scenario)
# ggplot



# results <- read.csv("results/bin_fla_scalar.csv")
# results <- within(results,{
#     CV_old_re <- (CV_old_Fem_GP_1_em-CV_old_Fem_GP_1_om)/CV_old_Fem_GP_1_om
#     CV_young_re <- (CV_young_Fem_GP_1_em-CV_young_Fem_GP_1_om)/CV_young_Fem_GP_1_om
#     L_at_Amin_re <- (L_at_Amin_Fem_GP_1_em-L_at_Amin_Fem_GP_1_om)/L_at_Amin_Fem_GP_1_om
#     L_at_Amax_re <- (L_at_Amax_Fem_GP_1_em-L_at_Amax_Fem_GP_1_om)/L_at_Amax_Fem_GP_1_om
#     VonBert_K_re <- (VonBert_K_Fem_GP_1_em-VonBert_K_Fem_GP_1_om)/VonBert_K_Fem_GP_1_om
#     depletion_re <- (depletion_em-depletion_om)/depletion_om
# })
# results_re <- calculate_re(results, FALSE)
# results_re <- results[, grep("_re", names(results))]
# results_re$B <- results$B;results_re$D <- results$D;results_re$E <- results$E
# results_re$replicate <- results$replicate
# results_long <- reshape2::melt(results_re, c("E","B", "D","replicate"))
# results_long <- merge(scen.df, results_long)
# results_long$B.value <- factor(results_long$B.value, levels=bin.seq)
# ## Make exploratory plots
# library(ggplot2)
# plot_scalar_boxplot(subset(results_long, E=="E0"), x="B.value", y="value", vert="variable", horiz2="D",
#                    horiz="E", rel=F, axes.free=TRUE) + xlab("# of length bins") +
#                                          ylab("relative error")+ ylim(-.7, .-3)
# ggsave("plots/bin_fla.png", width=9, height=5)








# #Check Relative error in biomass trajectories
# #Errors in selectivity
# #Check Cole's email

# get_results_all(dir = getwd(), user_scenarios = scenarios)


# results <- read.csv('ss3sim_scalar.csv')
# results <- within(results,{
#     CV_old_re <- (CV_old_Fem_GP_1_em-CV_old_Fem_GP_1_om)/CV_old_Fem_GP_1_om
#     CV_young_re <- (CV_young_Fem_GP_1_em-CV_young_Fem_GP_1_om)/CV_young_Fem_GP_1_om
#     L_at_Amin_re <- (L_at_Amin_Fem_GP_1_em-L_at_Amin_Fem_GP_1_om)/L_at_Amin_Fem_GP_1_om
#     L_at_Amax_re <- (L_at_Amax_Fem_GP_1_em-L_at_Amax_Fem_GP_1_om)/L_at_Amax_Fem_GP_1_om
#     VonBert_K_re <- (VonBert_K_Fem_GP_1_em-VonBert_K_Fem_GP_1_om)/VonBert_K_Fem_GP_1_om
#     depletion_re <- (depletion_em-depletion_om)/depletion_om
#     SSB_MSY_re <- (SSB_MSY_em - SSB_MSY_om) / SSB_MSY_om
#     SSB_Unfished_re <- (SSB_Unfished_em - SSB_Unfished_om) / SSB_Unfished_om
#     max_grad_re <- max_grad
# })
# results_re <- calculate_re(results, FALSE)

# calculate_re(read.csv('ss3sim_scalar.csv'), add = T)


# #------------------------------------------------------------------------







#------------------------------------------------------------------------
#Debug sample_wtatage.r 
# setwd('/Users/peterkuriyama/School/Research/capam_growth')
# library(devtools)

# load_all('ss3sim')
# load_all('ss3models')
# # devtools::install_github('r4ss/r4ss')

# library(ss3sim)
# library(ss3models)

#------------------------------------------------------------------------
# results.dir <- "/Users/peterkuriyama/School/Research/empirical_runs"
# setwd(results.dir)

# case_folder <- 'cases/hake-age'

# iters <- 1
# scen <- 'D4-F0-G1-X4-hake-age'

# case_files <- list(F = 'F', D = c('index', 'agecomp'), X = 'wtatage', 
#     G = 'G')

# find.spp <- scen
# parsed <- strsplit(find.spp, '-')[[1]]
# spp <- parsed[length(parsed)]

# if(length(grep('age', parsed)) == 1){
#     spp <- paste0(parsed[5], '-', parsed[6])
# }
# # case_folder1 <- paste0('cases', '/', spp)

# run_ss3sim(iterations = iters, scenarios = scen, case_folder = case_folder,
#     om_dir = ss3model(spp, 'om'), em_dir = ss3model(spp, 'em'),
#     case_files = case_files)



# # unlink(scen, recursive = TRUE)

# #Plot these Tests

# scenario <- scen
# # scenario <- 'D4-F0-G0-X4-hake-age'

# #OM
# wtatage.om <- read.table(paste0(scenario, '/1/om/wtatage.ss_new'), header = F,
#   skip = 2)
# names(wtatage.om)[1:6] <- c('yr', 'seas', 'gender', 'growpattern', 'birthseas', 'fleet')
# ages <- 7:ncol(wtatage.om)
# names(wtatage.om)[7:ncol(wtatage.om)] <- paste0('a', ages- 7)

# #EM
# wtatage.em <- read.table(paste0(scenario, '/1/em/wtatage.ss'), header = F,
#   skip = 2)
# names(wtatage.em)[1:6] <- c('yr', 'seas', 'gender', 'growpattern', 'birthseas', 'fleet')
# ages <- 7:ncol(wtatage.em)
# names(wtatage.em)[7:ncol(wtatage.em)] <- paste0('a', ages- 7)

# #Fleet 1 only
# wtom <- wtatage.om[wtatage.om$fleet == 1, 7:ncol(wtatage.om)]
# wtem <- wtatage.em[wtatage.em$fleet == 1, 7:ncol(wtatage.em)]

# #Calculate relative error
# # wtre <- (wtem - wtom) / wtom
# # write.csv(wtre, file = '/Users/peterkuriyama/Desktop/wtatage.csv')

# #Mean should converge
# meanvec <- t(apply(wtem, 2, mean))  


# #Check overall
# (meanvec - wtom[99, ]) / wtom[99, ]
# matplot(t(wtem), pch = 19)
# matplot(t(wtre), pch = 19)


# #-----------------------------------
# #Check to see which things have run
# # already <- list.files()
# # scenarios <- scenarios[scenarios %in% already == FALSE]

# #-----------------------------------
# #Run iterations in for loop
# for(ii in 1:length(scenarios))
# {
#     find.spp <- scenarios[ii]
#     parsed <- strsplit(find.spp, '-')[[1]]
#     spp <- parsed[length(parsed)]

#     if(length(grep('age', parsed)) == 1){
#         spp <- paste0(parsed[5], '-', parsed[6])
#     }
#     case_folder1 <- paste0('cases', '/', spp)

#     run_ss3sim(iterations = iters, scenarios = scenarios[ii], case_folder = case_folder1,
#         om_dir = ss3model(spp, 'om'), em_dir = ss3model(spp, 'em'),
#         case_files = case_files, parallel = TRUE,
#         parallel_iterations = TRUE)
# }




# #-----------------------------------
# #RUn Length Based Cases
# speciesLength <- c('yellow')
# scenariosLength <- expand_scenarios(cases = list(F = 0, D = 1,
#     G = 0:1), species = speciesLength)
# ## scenariosLength <- scenariosLength[c(27:28)]

# #-----------------------------------
# #Re-run ones that didn't run earlier
# # scenariosLength <- unique(run.again$scenario)[-grep("age", unique(run.again$scenario))]

# #-----------------------------------
# case_filesLength <- list(F = 'F', D = c('index', 'lcomp'),
#     G = 'G')

# for(ii in 1:length(scenariosLength))
# {
#     find.spp <- scenariosLength[ii]
#     parsed <- strsplit(find.spp, '-')[[1]]
#     spp <- parsed[length(parsed)]

#     case_folder1 <- paste0('cases', '/', spp)

#     run_ss3sim(iterations = iters, scenarios = scenariosLength[ii], case_folder = case_folder1,
#         om_dir = ss3model(spp, 'om'), em_dir = ss3model(spp, 'em'),
#         case_files = case_filesLength, parallel = FALSE,
#         parallel_iterations = FALSE)
# }
