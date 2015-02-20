#This file is the central plae to develop and run analyses for the paper
#Sources files that do particular tasks

#------------------------------------------------------------------------
#Set Working Directory, load files
setwd('/Users/peterkuriyama/School/Research/capam_growth/Empirical')
#Paths are structured as they are in the ss3sim repo

#------------------------------------------------------------------------
library(devtools)

load_all('../ss3sim')
load_all('../ss3models')

library(r4ss)
library(ss3sim)
library(ss3models)

case_folder <- 'cases'
user.recdevs <- matrix(data=rnorm(100^2, mean=0, sd=.05),
                       nrow=100, ncol=100)
#------------------------------------------------------------------------
species <- 'hake'

#Create Cases
om <- ss3model('hake', 'om')
em <- ss3model('hake', 'em')

scenarios <- 'F0-D1-X1-hake'

case_files <- list(F = 'F', D = c('index', 'agecomp'), X = 'wtatage')


run_ss3sim(iterations = 6, scenarios = scenarios, case_folder = case_folder,
  om_dir = om, em_dir = em, case_files = case_files)
