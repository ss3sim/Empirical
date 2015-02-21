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

load_all('../ss3sim')
load_all('../ss3models')

library(r4ss)
library(ss3sim)
library(ss3models)

case_folder <- 'cases'
#------------------------------------------------------------------------
#Create Cases Dynamically for reproducibility
species <- 'hake'
source('write_casefiles.R')

#------------------------------------------------------------------------
##Run Models

#Define Scenarios
scenarios.E <- expand_scenarios(cases = list(F = 1, D = 1:2, X = 1), species = species)
scenarios <- scenarios.E

case_files <- list(F = 'F', D = c('index', 'agecomp'), X = 'wtatage')
scenarios <- 'F0-D1-X1-hake'

run_ss3sim(iterations = 1:5, scenarios = scenarios, case_folder = case_folder,
  om_dir = ss3model(species, 'om'), em_dir = ss3model(species, 'em'), case_files = case_files, parallel = TRUE)

#------------------------------------------------------------------------

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



