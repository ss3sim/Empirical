#Set Working Directories
setwd("/Users/peterkuriyama/School/Research/capam_growth/Empirical/runs")

### ------------------------------------------------------------
## Startup the working environment
## ## Update the development tools, if you haven't recently
## update.packages(c('r4ss','knitr', 'devtools', 'roxygen2'))
## Load the neccessary libraries
library(devtools)
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

run_ss3sim(iterations = 1, scenarios = "F0-D0-E0-R0-X1-cow",
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
