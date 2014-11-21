#update.packages(c('knitr', 'devtools', 'roxygen2'))
## Build the ss3sim developement package
#remove.packages("ss3sim")
#devtools::install_github("ss3sim/ss3sim")
library(ss3sim)
library(r4ss)


writeW <- function(fleets, years, Nsamp, case, spp) {
  a <- c(paste("fleets;", fleets),
         paste("years;", years),
         paste("Nsamp;", Nsamp),
         "write_file; TRUE")
  writeLines(a, paste0("wtatage", case, "-", spp, ".txt"))
}
writeW <- function(years, fillFnc, case, spp) {
  a <- c("infile; wtatage.ss_new",
         "datfile; data.ss_new",
         "ctlfile; control.ss_new",
         "outfile; wtatage.ss",
         paste("years;", years),
         paste("fill_fnc;", fillFnc),
         "write_file; TRUE")
  writeLines(a, paste0("wtatage", case, "-", spp, ".txt"))
}

# Years Will be the same for both fleets 1 and 2
#NSamples also the same
oldwd <- getwd()
casewd <- "C:/NOAA2015/ss3Sim_Empirical/SimpleSims/cases"
#casewd <- "/Users/peterkuriyama/School/Research/capam_growth/Empirical/cases"
setwd(casewd)

om <- "../cowOM"
em <- "../cowEM"

spp <- 'cow'

#WtAtAge in every year
if(F) {
  writeW(years = "list(seq(1, 100, 1), seq(1, 100, 1), 1)",
    fillFnc="fill_across",
    case = 0, spp = spp)
}

case_files <-  list(F = "F", D =
    c("index", "lcomp", "agecomp"), E = "E", R = "R", 
    X = 'wtatage')

source("C:\\Users\\Allan.Hicks\\Documents\\GitHub\\Empirical\\Rcode\\get_caseargs.R")
source("C:\\Users\\Allan.Hicks\\Documents\\GitHub\\Empirical\\Rcode\\get_caseval.R")
get_caseargs(casewd, 
  scenario = "F0-D0-E0-R0-X0.cow",
  case_files = case_files)
source("C:\\NOAA2015\\ss3Sim_Empirical\\Rcode\\pastef.r")
source("C:\\NOAA2015\\ss3Sim_Empirical\\Rcode\\add_nulls.r")
source("C:\\NOAA2015\\ss3Sim_Empirical\\Rcode\\run_ss3sim_ach.r")
source("C:\\NOAA2015\\ss3Sim_Empirical\\Rcode\\ss3sim_base_ach.r")
source("C:\\NOAA2015\\ss3Sim_Empirical\\Rcode\\extract_expected_data.r")
source("C:\\Users\\Allan.Hicks\\Documents\\GitHub\\Empirical\\Rcode\\sample_wtatage.r")
source("C:\\NOAA2015\\ss3Sim_Empirical\\Rcode\\fromPeter\\fill_across.r")

setwd(casewd)
run_ss3sim(iterations = 26, scenarios ="D0-E0-F0-R0-cow",
  case_files = list(D = c("index", "lcomp", "agecomp"), E = "E", F = "F", R = "R"),
  case_folder = casewd, om_dir = om, em_dir = em)

#out <- SS_output(dir="D0-E0-F0-R0-cow/1/em", covar=FALSE,ncols=205)
#SS_plots(out,uncertainty=F)


setwd(casewd)
run_ss3sim(iterations = 1:20, scenarios ="D0-E0-F0-R0-X0-cow",
  case_files = list(D = c("index", "lcomp", "agecomp"), E = "E", F = "F", R = "R", X = "wtatage"),
  case_folder = casewd, om_dir = om, em_dir = em)

setwd(casewd)
run_ss3sim(iterations = 1:20, scenarios ="D1-E0-F0-R0-X1-cow",
  case_files = list(D = c("index", "lcomp", "agecomp"), E = "E", F = "F", R = "R", X = "wtatage"),
  case_folder = casewd, om_dir = om, em_dir = em)

setwd(casewd)
run_ss3sim(iterations = 1:20, scenarios ="D2-E0-F0-R0-X2-cow",
  case_files = list(D = c("index", "lcomp", "agecomp"), E = "E", F = "F", R = "R", X = "wtatage"),
  case_folder = casewd, om_dir = om, em_dir = em)


setwd("C:/NOAA2015/ss3Sim_Empirical/SimpleSims/cases/D0-E0-F0-R0-cow")
nsims <- 20
depl.1 <- list()
for(sim in 1:nsims) {
  out <- SS_output(dir=file.path(sim,"em"), covar=FALSE,ncols=205)
  depl.1[[sim]] <- out$current_depletion
}
depl.1 <- unlist(depl.1)
setwd("C:/NOAA2015/ss3Sim_Empirical/SimpleSims/cases/D0-E0-F0-R0-X0-cow")
nsims <- 20
depl.2 <- list()
for(sim in 1:nsims) {
  out <- SS_output(dir=file.path(sim,"em"), covar=FALSE,ncols=205)
  depl.2[[sim]] <- out$current_depletion
}
depl.2 <- unlist(depl.2)
setwd("C:/NOAA2015/ss3Sim_Empirical/SimpleSims/cases/D1-E0-F0-R0-X1-cow")
nsims <- 20
depl.3 <- list()
for(sim in 1:nsims) {
  out <- SS_output(dir=file.path(sim,"em"), covar=FALSE,ncols=205)
  depl.3[[sim]] <- out$current_depletion
}
depl.3 <- unlist(depl.3)
setwd("C:/NOAA2015/ss3Sim_Empirical/SimpleSims/cases/D2-E0-F0-R0-X2-cow")
nsims <- 20
depl.4 <- list()
for(sim in 1:nsims) {
  out <- SS_output(dir=file.path(sim,"em"), covar=FALSE,ncols=205)
  depl.4[[sim]] <- out$current_depletion
}
depl.4 <- unlist(depl.4)

boxplot(depl.1,depl.2,depl.3,depl.4)


setwd("C:/NOAA2015/ss3Sim_Empirical/SimpleSims/cases/test")
out <- sample_wtatage("wtatage.ss_new","wtatage.ss","data.ss_new","control.ss_new",
        years=list(26:100,26:100,1),Nsamp=list(rep(10000,75),rep(10000,75),rep(10000,75)),fill_across,write_file=F)








#Good Years
goods <- c(25, 35, 45, 55, 65, 75, seq(76, 91, 3), seq(92, 98, 2), 99, 100)

#Bad Years
bads <- seq(30, 100, 10)

#Nsamps
goods.samp <- c(rep(50, 5), rep(100, 13))
bads.samp <- rep(50, 8)

spp <- 'cow'
fleets <- c(1, 2)

#Write Good Data Case
writeW(fleets = "1", years = "list(c(25, 35, 45, 55, 65, 75, seq(76, 91, 3), seq(92, 98, 2), 99, 100),
  c(25, 35, 45, 55, 65, 75, seq(76, 91, 3), seq(92, 98, 2), 99, 100))", 
  Nsamp = "list(c(rep(50, 5), rep(100, 13)), c(rep(50, 5), rep(100, 13)))",
  case = 1, spp = spp)

#Write Bad Data Case
writeW(fleets = "list(1, 2)", years = "list(seq(30, 100, 10), seq(30, 100, 10))", 
  Nsamp = "list(rep(50, 8), rep(50, 8))",
  case = 2, spp = spp)








#compare fecundity and wtataage curves
setwd("C:/NOAA2015/ss3Sim_Empirical/SimpleSims/cases")
wtatage1 <- read.table("D0-E0-F0-R0-cow/1/om/wtatage.ss_new",skip=7,header=F)
names(wtatage1) <- c("yr","seas","gender","growpattern","birthseas","fleet","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")
wtatage2 <- read.table("D0-E0-F0-R0-X0-cow/1/em/wtatage.ss",skip=3,header=F)
names(wtatage2) <- c("yr","seas","gender","growpattern","birthseas","fleet","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")

plot(0:25,wtatage1[wtatage1$fleet==-2,-(1:6)],type="l",lwd=3,ylab="Fecundity",xlab="Age")
points(0:25,wtatage2[wtatage2$fleet==-2 & wtatage2$yr==1,-(1:6)],type="b",pch=16)
title(main="Fecundity: Maturity-at-age X weight")
legend("topleft",c("True Fecundity","Empirical Model"),lty=c(1,1),lwd=c(3,1),pch=c(NA,16))