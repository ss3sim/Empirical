options(device = 'quartz')

#Set Working Directories
setwd("/Users/peterkuriyama/School/Research/capam_growth/Empirical/runs")

### ------------------------------------------------------------
## Startup the working environment
## ## Update the development tools, if you haven't recently
## update.packages(c('r4ss','knitr', 'devtools', 'roxygen2'))
## Load the neccessary libraries
library(devtools)
library(plyr)

require(doParallel)
registerDoParallel(cores = 4)
require(foreach)
getDoParWorkers()
## Install the package from our local git repository, which is usually a
## development branch. You need to pull down any changes into the branch
## before running this command.
## load_all("../ss3sim")

install('../../ss3sim') # may need this for parallel runs??
library(ss3sim)
library(r4ss)

#Define Models
case_folder <- '../cases'
cod.om <- '../cowOM'
cod.em <- '../cowEM'

#Base Case Runs With Sampling Functions
case_files <-  list(F = "F", D =
    c("index","agecomp"), E = "E", R = "R", 
    X = 'wtatage')

### ------------------------------------------------------------
#Think About Cases now that the model is working

#CV = .1

# Case    1 Hake Years, 50 samples
# length(c(c(50, 60, 70, 75), seq(76, 88, 3), seq(89, 97, 2), c(98, 99, 100)))
# c(50, 60, 70, 75), seq(76, 88, 3), seq(89, 97, 2), c(98, 99, 100)
 #2 Hake Years, 200 samples
         #3 Hake Years, 500 samples

#Case     4 Every 10 years, every 5 years 50 samples
#         5 Every 10 years, every 5 years 200 samples
#         6 Every 10 years, every 5 years 500 samples

# c(seq(50, 80, 5), seq(85, 100, 5))

#Plots of Wtatage Data

### ------------------------------------------------------------
#Run Scenarios
scens <- c("F0-D0-E0-R0-X0-cow",
           "F0-D1-E0-R0-X1-cow",
           "F0-D2-E0-R0-X2-cow",
           "F0-D3-E0-R0-X3-cow",
           "F0-D4-E0-R0-X4-cow",
           "F0-D5-E0-R0-X5-cow",
           "F0-D6-E0-R0-X6-cow")

for(ii in 1:length(scens)){
  get_caseargs('/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases', 
    scenario = scens[ii], case_files = case_files)

  run_ss3sim(iterations = 1:3, scenarios = scens[ii],
    case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
    em_dir = cod.em, bias_adjust = TRUE, parallel = TRUE)


}


### ------------------------------------------------------------
#Run with CV of .01 in sampling_wtatage
### ------------------------------------------------------------
scens <- "F0-D0-E0-R0-X0-cow"
  get_caseargs('/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases', 
    scenario = scens, case_files = case_files)

  run_ss3sim(iterations = 1:3, scenarios = scens,
    case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
    em_dir = cod.em, bias_adjust = FALSE)


#RUn Base Case with 1000 samples every year
# get_caseargs('/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases', 
#     scenario = "F0-D0-E0-R0-X0-cow", case_files = case_files)

#   run_ss3sim(iterations = 1:25, scenarios = "F0-D0-E0-R0-X0-cow",
#     case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
#     em_dir = cod.em, bias_adjust = FALSE, parallel = FALSE)
scens <- scens[1]

#Read in Results
suppressWarnings(get_results_all(user_scenarios = scens, over=TRUE))

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
results_re <- results[, grep("_re", names(results))]

#Remove Columns unused
results_re <- results_re[, 1:4]

results_re$replicate <- results$replicate
results_re$D <- results$D
results_re$X <- results$X

results_long <- reshape2::melt(results_re, 
  c('D', 'X', 'replicate'))



samp.size <- data.frame(X = unique(results_long$X), Nsamp = c(1000, c(50, 200, 500), c(50, 200, 500)))
yearss <- data.frame(D = unique(results_long$D), years = c(99, rep('17 years', 3), rep('11 years', 3)))

##Add Cases with 500 samples every year and 
#Compare crappiness of actual hake data with 




###Plot outside of plot_scalar_boxplot 
#Use Ggplot boxplot only

results_long <- merge(samp.size, results_long, by = 'X')
results_long <- merge(yearss, results_long, by = 'D')
results_long <- results_long[-which(results_long$variable == 'max_grad_re'), ]
results_long$Nsamp <- as.factor(results_long$Nsamp)

library(ggplot2)

plot_scalar_boxplot(results_long, x ='Nsamp', y = 'value', vert = 'variable', horiz = 'years') + 
  theme_bw() + ylab('Relative Error') + geom_hline(yintercept = 0, col = 'red') + 
  ylim(-.2, .6) + xlab('Number of Samples per Year') + labs(title = 'Cod - Empirical Weight at Age')
 

plot_scalar_boxplot(results_long, x = 'Nsamp', y = 'value', vert = 'variable') + theme_bw()




plot_scalar_boxplot(results_long,
  x = 'X', y = 'value', vert = 'variable', horiz2 = 'D'))

  ))


















#Old Code
### ------------------------------------------------------------
### ------------------------------------------------------------
#Case 0 is estimate
get_caseargs('/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases', 
  scenario = "F0-D1-E0-R0-X1-cow", case_files = case_files)

run_ss3sim(iterations = 1:3, scenarios = "F0-D1-E0-R0-X1-cow",
  case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
  em_dir = cod.em, bias_adjust = FALSE, parallel = FALSE)



### ------------------------------------------------------------
#Cases with large sample sizes. 
get_caseargs('/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases', 
  scenario = "F0-D1-E0-R0-X1-cow", case_files = case_files)

run_ss3sim(iterations = 1:3, scenarios = "F0-D1-E0-R0-X1-cow",
  case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
  em_dir = cod.em, bias_adjust = FALSE, parallel = FALSE)

### ------------------------------------------------------------
#1000 Sample Size Data Scenario, not every year data
get_caseargs('/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases', 
  scenario = "F0-D2-E0-R0-X2-cow", case_files = case_files)

run_ss3sim(iterations = 1:3, scenarios = "F0-D2-E0-R0-X2-cow",
  case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
  em_dir = cod.em, bias_adjust = FALSE, parallel = FALSE)


results_re
### ------------------------------------------------------------
#500 Sample Size Data Scenario
get_caseargs('/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases', 
  scenario = "F0-D3-E0-R0-X3-cow", case_files = case_files)

run_ss3sim(iterations = 1:5, scenarios = "F0-D3-E0-R0-X3-cow",
  case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
  em_dir = cod.em, bias_adjust = FALSE, parallel = FALSE)

### ------------------------------------------------------------
#Perfect Data Scenario 10,0000 samples every year
get_caseargs('/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases', 
  scenario = "F0-D4-E0-R0-X4-cow", case_files = case_files)

run_ss3sim(iterations = 1:5, scenarios = "F0-D4-E0-R0-X4-cow",
  case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
  em_dir = cod.em, bias_adjust = FALSE, parallel = FALSE)




run_ss3sim(iterations = 27:31, scenarios = "F0-D4-E0-R0-X4-cow",
  case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
  em_dir = cod.em, bias_adjust = FALSE, parallel = FALSE)

get_results_all(user_scenarios = "F0-D4-E0-R0-X4-cow", over=TRUE)

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
results_re <- results[, grep("_re", names(results))]


### ------------------------------------------------------------
scenarios <- c("F0-D0-E0-R0-X1-cow", "F0-D1-E0-R0-X1-cow", "F0-D2-E0-R0-X2-cow",
  "F0-D3-E0-R0-X3-cow", "F0-D4-E0-R0-X4-cow")





for(ii)
scen.df <- rep()

# sampwtatage <- read.table(file = 'clipboard')
# dev.new()
# # image(as.matrix(sampwtatage))

# ss.new <- read.table(file = 'clipboard')
# image(as.matrix(ss.new)) 
get_results_all(user_scenarios = scenarios, over=TRUE)

# file.copy("ss3sim_scalar.csv", "results/bin_cow_scalar.csv", over=TRUE)
# file.copy("ss3sim_ts.csv", "results/bin_fla_ts.csv", over=TRUE)
# results <- read.csv("results/bin_fla_scalar.csv")

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
results_re <- results[, grep("_re", names(results))]

scenarios.df <- vector(length = 25 * 5)

scenarios.df[1:25] <- rep(scenarios[1], 25)
scenarios.df[26:50] <- rep(scenarios[2], 25)
scenarios.df[51:75] <- rep(scenarios[3], 25)
scenarios.df[76:100] <- rep(scenarios[4], 25)
scenarios.df[101:125] <- rep(scenarios[5], 25)

results_re$scen <- scenarios.df
results_re[c(1:4, 10)]


for(ii in 1:length(scenarios)){
  scenarios.df[1:25] <- rep(scenarios[ii], 25)
}

results_re[, c(1:4)]

results_re

results_re[results_re$max_grad_re < 10, ]
scen.df <- c("F0-D0-E0-R0-X1-cow", "F0-D1-E0-R0-X1-cow")



# results_re$B <- results$B;results_re$D <- results$D;results_re$E <- results$E
results_re$replicate <- results$replicate
results_long <- reshape2::melt(results_re, c("E", "D","replicate"))

results_long$scen.df <- "F0-D0-E0-R0-X1-cow"


# results_long <- merge(scen.df, results_long)
# results_long$B.value <- factor(results_long$B.value, levels=bin.seq)
## Make exploratory plots
library(ggplot2)
plot_scalar_boxplot(subset(results_long, E=="E0"), y="value", vert="variable", horiz2="D",
                   horiz="E", rel=F, axes.free=TRUE) + xlab("# of length bins") +
                                         ylab("relative error")+ ylim(-.7, .-3)




scenarios <- list("F0-D0-E0-R0-X1-cow", "F0-D1-E0-R0-X1-cow")

res.list <- NULL

output1 <- SS_output(paste0("F0-D0-E0-R0-X1-cow", '/1/em'), covar = FALSE, ncols = 500)




SS_plots(output1, covar = FALSE, uncertainty = FALSE, png = TRUE)


dev.off()







## Look at a couple of models closer using r4ss
res.list <- NULL
for(i in 1:length(temp.scen)){
    res.list[[i]] <- SS_output(paste0(temp.scen[i], "/1/em"), covar=FALSE)
}
for(i in 1:length(res.list)){
    SSplotComps(res.list[[i]], print=TRUE)
}
for(i in 1:length(res.list)){
    SS_plots(res.list[[i]], png=TRUE, uncer=F, html=F)
}

for(i in 1:length(res.list)){
    file.copy(paste0(scen[i],
                     "/1/emcomp_lenfit_sex1mkt0aggregated across time.png"), paste0("plots/lencompfit_", scen[i], ".png"))
    file.copy(paste0(scen[i],
                     "/1/em/plots/data_plot.png"), paste0("plots/data_", scen[i], ".png"))
}

## Read in the results and convert to relative error in long format
temp.scen <- scen[scen %in% dir(getwd())]
temp.scen <- temp.scen[-grep("D103", temp.scen)]
get_results_all(user_scenarios=scen, over=TRUE)

file.copy("ss3sim_scalar.csv", "results/bin_fla_scalar.csv", over=TRUE)
file.copy("ss3sim_ts.csv", "results/bin_fla_ts.csv", over=TRUE)
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












#No Mean wt at age
case_files <-  list(F = "F", D =
    c("index", "lcomp", "agecomp"), E = "E", R = "R")

get_caseargs('/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases', 
  scenario = "F0-D0-E0-R0-cow", case_files = case_files)

run_ss3sim(iterations = 1, scenarios = "F0-D0-E0-R0-cow",
  case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
  em_dir = cod.em, bias_adjust = FALSE, parallel = FALSE)


load('wtatage.complete.Rdata')
wtatage.complete

### ------------------------------------------------------------
#Run Single Case with Wtatage
case_folder <- '../cases'
cod.om <- '../cowOM'
cod.em <- '../cowEM'
# cod.em <- '../../growth_models/co-em'

# my.casefiles <- list(list(A = "agecomp", E = "E", F = "F", X = "mlacomp",
#     I = "index", L = "lcomp", R = "R", S = c(toupper(letters[10:15]))

# case_files <-  list(F = "F", D =
#     c("index", "lcomp", "agecomp"), E = "E", R = "R")

# run_ss3sim(iterations = 1, scenarios = "F0-D0-E0-R0-cow",
#   case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
#   em_dir = cod.em, bias_adjust = FALSE, parallel = FALSE)

case_files <-  list(F = "F", D =
    c("index", "lcomp", "agecomp"), E = "E", R = "R", 
    X = 'wtatage')

get_caseargs('/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases', 
  scenario = "F0-D0-E0-R0-X1-cow", case_files = case_files)


run_ss3sim(iterations = 1, scenarios = "F0-D0-E0-R0-X1-cow",
  case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
  em_dir = cod.em, bias_adjust = FALSE, parallel = FALSE)


unlink("/Users/peterkuriyama/School/Research/capam_growth/Empirical/runs/F0-D0-E0-R0-X1-cow")


### ------------------------------------------------------------
##Write Cases for wtatage Sampling
 
writeX <- function(fleets, years, Nsamp, case) {
  a <- c(paste("fleets;", fleets),
         paste("years;", years),
         paste("Nsamp;", Nsamp),
         "write_file; TRUE")
  writeLines(a, paste0("mlacomp", case, "-", spp.case[spp], ".txt"))
}

# Years Will be the same for both fleets 1 and 2
#NSamples also the same
oldwd <- getwd()
casewd <- "/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases"
setwd(casewd)

#Good Years
goods <- c(25, 35, 45, 55, 65, 75, seq(76, 91, 3), seq(92, 98, 2), 99, 100)

#Bad Years
bads <- seq(30, 100, 10)

#Nsamps
goods.samp <- c(rep(50, 5), rep(100, 13))
bads.samp <- rep(50, 8)

spp <- 'cow'
fleets <- c(1, 2)

writeW <- function(fleets, years, Nsamp, case, spp) {
  a <- c(paste("fleets;", fleets),
         paste("years;", years),
         paste("Nsamp;", Nsamp),
         "write_file; TRUE")
  writeLines(a, paste0("wtatage", case, "-", spp, ".txt"))
}

#Write Good Data Case
writeW(fleets = "1", years = "list(c(25, 35, 45, 55, 65, 75, seq(76, 91, 3), seq(92, 98, 2), 99, 100),
  c(25, 35, 45, 55, 65, 75, seq(76, 91, 3), seq(92, 98, 2), 99, 100))", 
  Nsamp = "list(c(rep(50, 5), rep(100, 13)), c(rep(50, 5), rep(100, 13)))",
  case = 1, spp = spp)

#Write Bad Data Case
writeW(fleets = "list(1, 2)", years = "list(seq(30, 100, 10), seq(30, 100, 10))", 
  Nsamp = "list(rep(50, 8), rep(50, 8))",
  case = 2, spp = spp)




### ------------------------------------------------------------
#Run Single Case with Wtatage
case_folder <- '../cases'
cod.om <- '../cowOM'
cod.em <- '../cowEM'
# cod.em <- '../../growth_models/co-em'

my.casefiles <- list(list(A = "agecomp", E = "E", F = "F", X = "mlacomp",
    I = "index", L = "lcomp", R = "R", S = c(toupper(letters[10:15]))

case_files <-  list(F = "F", D =
    c("index", "lcomp", "agecomp"), E = "E", R = "R")

run_ss3sim(iterations = 1, scenarios = "F0-D0-E0-R0-cow",
  case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
  em_dir = cod.em, bias_adjust = FALSE, parallel = FALSE)

case_files <-  list(F = "F", D =
    c("index", "lcomp", "agecomp"), E = "E", R = "R", 
    X = 'wtatage')

get_caseargs('/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases', 
  scenario = "F0-D0-E0-R0-X1-cow", case_files = case_files)


run_ss3sim(iterations = 1, scenarios = "F0-D0-E0-R0-X1-cow",
  case_folder = '../cases', case_files = case_files, om_dir = cod.om, 
  em_dir = cod.em, bias_adjust = FALSE, parallel = FALSE)






# run_ss3sim(iterations = my.totnum, scenarios = use.scen,
#                case_folder = dir.cases, case_files = my.casefiles, 
#                om_dir = use.om, em_dir = use.em, bias_adjust = my.bias,
#                ignore.stdout = TRUE, parallel = TRUE)


list()

scen <- expand_scenarios(cases=list(D=100:103, E=0:1, F=0, B=1:bin.n),
                         species="fla")
case_files <-  list(F = "F", D =
    c("index", "lcomp", "agecomp", "calcomp"), E = "E", B="bin")
get_caseargs(folder = 'cases', scenario = scen[1],
                  case_files = case_files)

run_ss3sim(iterations = 1:15, scenarios = c("B1-D100-E1-F0-fla", "B2-D100-E1-F0-fla"), parallel=TRUE,
           parallel_iterations=TRUE,
           case_folder = case_folder, om_dir = fla.om,
           em_dir = fla.em, case_files=case_files)

# Cases:
# OM-Time varying Linf
#   EM-Estimate parameters
#   EM-Use wtatage



## Bin analysis across multiple data types
## Write the cases to file
bin.n <- 5
bin.seq <- floor(seq(8, 40, len=bin.n))
for(i in 1:bin.n){
    x <- c(paste("bin_vector; list(len=seq(2, 86, length.out=", bin.seq[i],"))"),
            "type;c('len', 'cal')", "pop_bin;NULL")
    writeLines(x, con=paste0(case_folder, "/bin",i, "-fla.txt"))
}
scen.df <- data.frame(B.value=bin.seq, B=paste0("B", 1:bin.n))
scen <- expand_scenarios(cases=list(D=100:103, E=0:1, F=0, B=1:bin.n),
                         species="fla")
case_files <-  list(F = "F", D =
    c("index", "lcomp", "agecomp", "calcomp"), E = "E", B="bin")
get_caseargs(folder = 'cases', scenario = scen[1],
                  case_files = case_files)

run_ss3sim(iterations = 1:15, scenarios = c("B1-D100-E1-F0-fla", "B2-D100-E1-F0-fla"), parallel=TRUE,
           parallel_iterations=TRUE,
           case_folder = case_folder, om_dir = fla.om,
           em_dir = fla.em, case_files=case_files)






case_folder <- 'cases'
d <- system.file("extdata", package = "ss3sim")
fla.om <- paste0(d, "../../growth_models/fla-om")
fla.em <- paste0(d, "../../growth_models/fla-em")

scen.df <- data.frame(B.value=bin.seq, B=paste0("B", 1:bin.n))
scen <- expand_scenarios(cases=list(D=100:103, E=0:1, F=0, B=1:bin.n),
                         species="fla")
case_files <-  list(F = "F", D =
    c("index", "lcomp", "agecomp", "calcomp"), E = "E", B="bin")
get_caseargs(folder = 'cases', scenario = scen[1],
                  case_files = case_files)



run_ss3sim(iterations = 1:15, scenarios = c("B1-D100-E1-F0-fla", "B2-D100-E1-F0-fla"), parallel=TRUE,
           parallel_iterations=TRUE,
           case_folder = case_folder, om_dir = fla.om,
           em_dir = fla.em, case_files=case_files)
