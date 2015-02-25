#This file is the central plae to develop and run analyses for the paper
#Sources files that do particular tasks

#------------------------------------------------------------------------
#Set Working Directory, load files
# setwd('/Users/peterkuriyama/School/Research/capam_growth/Empirical')

#Save Shit to google drive
setwd("C://Users//Peter//Google Drive//Empirical")


#Paths are structured as they are in the ss3sim repo
library(ggplot2)
library(dplyr)
library(reshape2)
library(plyr)
#------------------------------------------------------------------------
#Set Up Parallels
library('doParallel')
registerDoParallel(cores = 4)
library('foreach')
message(paste(getDoParWorkers(), "cores have been registered for",
    "parallel processing."))
#------------------------------------------------------------------------
library(devtools)
devtools::install_github('r4ss/r4ss')
devtools::install_github('ss3sim/ss3sim')
devtools::install_github('ss3sim/ss3models')

# load_all('../ss3sim')
# load_all('../ss3models')
# devtools::install_github('ss3sim/ss3models')

library(r4ss)
library(ss3sim)
library(ss3models)

# case_folder <- 'cases'
#------------------------------------------------------------------------
#Create Files Dynamically for reproducibility
# - Must have species sub-directories in case_folder - #
# species.vec <- c('yellowAge', 'hakeAge', 'mackerelAge')

#All Species
species.vec <- c('hake-age', 'yellow-age', 'hake', 'yellow')

for(ii in 1:length(species.vec))
{
    species <- species.vec[ii]
    #Create case directory and specify folder
    dir.create(file.path(getwd(), paste0('cases', '/', species)))
    case_folder <- paste0('cases', '/', species)
    source('write_casefiles.R')
    source('test_tv.r')
}

#------------------------------------------------------------------------
##Run Models
#Globally set iterations
iters <- 5:10

#-----------------------------------
#Run age-based models
species.vec <- c('hake-age', 'yellow-age')
#Define Scenarios
scenarios1 <- expand_scenarios(cases = list(F = 0, D = 1, X = 1,
    G = 0:7), species = species.vec)
scenarios2 <- expand_scenarios(cases = list(F = 0, D = 2, X = 2,
    G = 0:7), species = species.vec)
scenarios3 <- expand_scenarios(cases = list(F = 0, D = 3, X = 3,
                                            G = 0:7), species = species.vec)
scenarios <- c(scenarios1, scenarios2, scenarios3)

# scenarios <- scenarios[grep('G0', scenarios)]
case_files <- list(F = 'F', D = c('index', 'agecomp'), X = 'wtatage', 
    G = 'G')
#-----------------------------------
#Check to see which things have run
# already <- list.files()
# scenarios <- scenarios[scenarios %in% already == FALSE]

#-----------------------------------
#Run iterations in for loop
for(ii in 1:length(scenarios))
{
    find.spp <- scenarios[ii]
    parsed <- strsplit(find.spp, '-')[[1]]
    spp <- parsed[length(parsed)]

    if(length(grep('age', parsed)) == 1){
        spp <- paste0(parsed[5], '-', parsed[6])
    }
    case_folder1 <- paste0('cases', '/', spp)

    run_ss3sim(iterations = iters, scenarios = scenarios[ii], case_folder = case_folder1,
        om_dir = ss3model(spp, 'om'), em_dir = ss3model(spp, 'em'),
        case_files = case_files, parallel = TRUE,
        parallel_iterations = TRUE)
}

#-----------------------------------
#RUn Length Based Cases
speciesLength <- c('hake', 'yellow')
scenariosLength <- expand_scenarios(cases = list(F = 0, D = 1:3,
    G = 0:7), species = speciesLength)
# scenariosLength <- scenariosLength[c(27:28)]

case_filesLength <- list(F = 'F', D = c('index', 'lcomp'),
    G = 'G')

for(ii in 1:length(scenariosLength))
{
    find.spp <- scenariosLength[ii]
    parsed <- strsplit(find.spp, '-')[[1]]
    spp <- parsed[length(parsed)]

    case_folder1 <- paste0('cases', '/', spp)

    run_ss3sim(iterations = iters, scenarios = scenariosLength[ii], case_folder = case_folder1,
        om_dir = ss3model(spp, 'om'), em_dir = ss3model(spp, 'em'),
        case_files = case_filesLength, parallel = TRUE,
        parallel_iterations = TRUE)
}


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






