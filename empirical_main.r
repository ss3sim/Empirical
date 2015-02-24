#This file is the central plae to develop and run analyses for the paper
#Sources files that do particular tasks

#------------------------------------------------------------------------
#Set Working Directory, load files
setwd('/Users/peterkuriyama/School/Research/capam_growth/Empirical')
#Paths are structured as they are in the ss3sim repo

#------------------------------------------------------------------------
#Set Up Parallels
library('doParallel')
registerDoParallel(cores = 2)
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
species.vec <- c('yellowAge')

for(ii in 1:length(species.vec))
{
    species <- species.vec[ii]
    case_folder <- paste0('cases', '/', species)
    source('write_casefiles.R')
    source('test_tv.r')
}


#------------------------------------------------------------------------
##Run Models
#Define Scenarios
scenarios1 <- expand_scenarios(cases = list(F = 0, D = 1, X = 1,
    G = 1:6), species = species.vec)
scenarios2 <- expand_scenarios(cases = list(F = 0, D = 2, X = 2,
    G = 1:6), species = species.vec)

scenarios <- c(scenarios1, scenarios2)
# scenarios <- scenarios[-1]

#Check to see which things have run
# already <- list.files()
# scenarios <- scenarios[scenarios %in% already == FALSE]

case_files <- list(F = 'F', D = c('index', 'agecomp'), X = 'wtatage', 
    G = 'G')

for(ii in 1:length(scenarios))
{
    find.spp <- scenarios[ii]
    parsed <- strsplit(find.spp, '-')[[1]]
    spp <- parsed[length(parsed)]

    case_folder1 <- paste0('cases', '/', spp)

    run_ss3sim(iterations = 1:4, scenarios = scenarios[ii], case_folder = case_folder1,
        om_dir = ss3model(spp, 'om'), em_dir = ss3model(spp, 'em'),
        case_files = case_files, parallel = TRUE,
        parallel_iterations = TRUE)
}

#------------------------------------------------------------------------
#plot time-varying growth scenarios
source('plotting_functions.r')
apply(as.data.frame(scenarios), MAR = 1, FUN =  function(x) make_wtatage_png(x))

#------------------------------------------------------------------------
#Haven't done this yet
##Read in Results
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






