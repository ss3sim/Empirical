
calculate_runtime <- function(start_time, end_time) {
  ## The start_time and end_time strings are complex and need to be cleaned up
  ## before processing into date objects.
  start <- data.frame(do.call(rbind, strsplit(x = as.character(start_time),
                                              split = " ", fixed = T))[, -(1:2)])
  end <- data.frame(do.call(rbind, strsplit(x = as.character(end_time),
                                            split = " ", fixed = T))[, -(1:2)])
  start <- as.data.frame(t(start))
  end <- as.data.frame(t(end))
  if(ncol(start) == 4) {
    names(start) <- names(end) <- c("month", "day", "time", "year")
  }
  if(ncol(start) == 5) {
    names(start) <- names(end) <- c("month", "", "day", "time", "year")
  }
  if(ncol(start) %in% c(4, 5)) {
    start.date <- lubridate::ymd_hms(with(start, paste(year,
                                                       month, day, time, sep = "-")))
    end.date <- lubridate::ymd_hms(with(end, paste(year,
                                                   month, day, time, sep = "-")))
    ## run.mins <- as.vector(end.date - start.date)
    run.mins <- as.vector(difftime(end.date, start.date, units='secs'))/60
  } else {
    run.mins <- NA
  }
  return(run.mins)
}

##############################################################################
#Setworking directory here
# setwd("../empirical/D3-E2-F1-G1-hake-age")
setwd("D4-E2-F1-G1-hake-age")

overwrite_files <- TRUE
# setwd("D2-E2-F1-G1-hake-age")
# scenario <- "D2-E2-F1-G1-hake-age"
scenario <- list.files()[grep("-", list.files())]

scenario <- "D2-E2-F1-G1-hake-age"
scenario <- scenariosl[1]

#Worked for one scenario that had issues
scalar.file <- paste0('results_scalar_', scenario, ".csv")
ts.file <- paste0("results_ts_",scenario,".csv")
resids.file <- paste0("results_resids_",scenario,".csv")
if(file.exists(scalar.file) | file.exists(ts.file)){
  if(overwrite_files) {
    ## Delete them and continue
    message(paste0("Files deleted for ", scenario))
    file.remove(scalar.file, ts.file)
  } else {
    ## Stop the progress
    stop(paste0("Files already exist for ", scenario,"
                and overwrite_files=FALSE"))
  }  }

bias <- c("bias.converged" = NA, "bias.tried" = NA)
if(length(grep("bias", dir()))==1){
  bias.file <- read.table(file="bias/AdjustBias.DAT", header=FALSE)
  ## The ones with NAs mean it didn't converge
  bias[1] <- NROW(na.omit(bias.file))
  bias[2] <- NROW(bias.file)
}

## Loop through each replicate, not including the bias folders, and get
## results from both models
## Remove the .csv files and bias folder, they are not reps
reps.dirs <- list.files(pattern = "[0-9]+$")
reps.dirs <- sort(as.numeric(reps.dirs))
reps.dirs <- reps.dirs[-c(1, 2)]
if(length(reps.dirs)==0)
  stop(paste("Error:No replicates for scenario", scenario))
## Loop through replicates and extract results using r4ss::SS_output
resids.list <- list()
message(paste0("Starting ", scenario, " with ", length(reps.dirs), " iterations"))
## Get the number of columns for this scenario
numcol <- read.table(file.path(reps.dirs[1], "om", "Report.sso"),
                     col.names = 1:300, fill = TRUE, quote = "",
                     colClasses = "character", nrows = -1, comment.char = "")
numcol <- max(which(apply(numcol, 2, function(x) all(x == "")) == FALSE)) + 1

for(rep in reps.dirs){
  print(rep)
  ## Check that the model finished running and if not skip it but
  ## report that ID
  ID <- paste0(scenario, "-", rep)
  if(!file.exists(paste0(rep,"/em/Report.sso"))){
    message(paste("Missing Report.sso file for:", ID, "; skipping..."))
  } else {
    ## Otherwise read in and write to file
    report.em <-
      SS_output(paste0(rep,"/em/"), covar=FALSE, verbose=FALSE,
                compfile="none", forecast=TRUE, warn=TRUE,
                readwt=FALSE, printstats=FALSE, NoCompOK=TRUE,
                ncols=numcol)
    report.om <-
      SS_output(paste0(rep,"/om/"), covar=FALSE, verbose=FALSE,
                compfile="none", forecast=FALSE, warn=TRUE,
                readwt=FALSE, printstats=FALSE, NoCompOK=TRUE,
                ncols=numcol)
    ## ## Grab the residuals for the indices
    ## resids <- log(report.em$cpue$Obs) - log(report.em$cpue$Exp)
    ## resids.long <- data.frame(report.em$cpue[,c("FleetName", "Yr")], resids)
    ## resids.list[[rep]] <-
    ##     cbind(scenario, rep, reshape2::dcast(resids.long, FleetName~Yr,
    ##                                          value.var="resids"))
    ## Get scalars from the two models
    scalar.om <- get_results_scalar(report.om)
    names(scalar.om) <- paste0(names(scalar.om),"_om")
    scalar.em <- get_results_scalar(report.em)
    names(scalar.em) <- paste0(names(scalar.em),"_em")
    ## Get timeseires from the two
    timeseries.om <- get_results_timeseries(report.om)
    names(timeseries.om) <- paste0(names(timeseries.om),"_om")
    timeseries.em <- get_results_timeseries(report.em)
    names(timeseries.em) <- paste0(names(timeseries.em),"_em")
    
    ## Combine them together and massage a bit
    scalar <- cbind(scalar.om, scalar.em, t(bias))
    ts <- cbind(timeseries.om, timeseries.em)
    scalar$scenario <- ts$scenario <- scenario
    scalar$replicate <- ts$replicate <- rep
    ## parse the scenarios into columns for plotting later
    scenario.scalar <-
      data.frame(do.call(rbind, strsplit(gsub("([0-9]+-)", "\\1 ",
                                              as.character(scalar$scenario)), "- ")), stringsAsFactors = FALSE)
    names(scenario.scalar) <-
      c(substr(as.vector(as.character(
        scenario.scalar[1,-ncol(scenario.scalar)])), 1,1) ,"species")
    scenario.ts <-
      data.frame(do.call(rbind, strsplit(gsub("([0-9]+-)", "\\1 ",
                                              as.character(ts$scenario)), "- ")),
                 row.names = row.names(ts), stringsAsFactors = FALSE)
    names(scenario.ts) <-
      c(substr(as.vector(as.character(
        scenario.ts[1,-ncol(scenario.ts)])), 1,1) ,"species")
    
    scalar <- cbind(scalar, scenario.scalar)
    ts <- cbind(ts, scenario.ts)
    ## Other calcs
    ts$year <- ts$Yr_om
    ts$Yr_om <- NULL
    ts$Yr_em <- NULL
    scalar$max_grad <- scalar$max_grad_em
    ignore.cols <- which(names(scalar) %in%
                           c("max_grad_om", "params_on_bound_om",
                             "max_grad_em","params_stuck_low_om",
                             "params_stuck_high_om" ))
    scalar <- scalar[ , -ignore.cols]
    
    ## Also get some meta data and other convergence info like the
    ## version, runtime, etc. as checks
    temp <- readLines(con=paste0(rep,"/em/Report.sso"), n=10)
    scalar$version <- temp[1]
    scalar$RunTime <- calculate_runtime(temp[4],temp[5])
    scalar$hessian <- file.exists(paste0(rep,"/em/admodel.cov"))
    ## The number of iterations for the run is only in this file for
    ## some reason.
    if(!file.exists(paste0(rep,"/em/CumReport.sso"))) {
      Niterations <- NA
    } else {
      cumrep <- readLines(paste0(rep,"/em/CumReport.sso"), n=5)
      tmp <- grep("N_iter", cumrep)
      if(length(tmp)==0){
        scalar$Niterations <- NA
      } else {
        scalar$Niterations <-
          as.numeric(strsplit(cumrep[tmp[1]],split=" ")[[1]][3])
      }
    }
    
    ## Write them to file in the scenario folder
    scalar.exists <- file.exists(scalar.file)
    write.table(x=scalar, file=scalar.file, append=scalar.exists,
                col.names=!scalar.exists, row.names=FALSE, sep=",")
    ts.exists <- file.exists(ts.file)
    write.table(x=ts, file=ts.file, append=ts.exists,
                col.names=!ts.exists, row.names=FALSE, sep=",")
  }
}

##############################################################################
#combine csvs across all scenarios
fil <- scenarios
old.wd <- getwd()

scalar.list <- as.list(1:24)
ts.list <- as.list(1:24)

# ii.loc <- rep(NA, 32)

for(ii in 1:length(fil))
{
  print(ii)
  setwd(fil[ii])
  
  #which scenarios have results files?
  temp <- list.files()
#   if(sum(grep('.csv', temp)) < 1) ii.loc[ii] <- ii
  
#   temp <- list.files()
  scalar.list[[ii]] <- read.csv(temp[grep('scalar', temp)])
  ts.list[[ii]] <- read.csv(temp[grep("_ts", temp)])
  setwd('..')
}

#Manually get ones that don't work
# get_results_all(dir = getwd() , user_scenarios = fil[30], 
#                 parallel = F, over = TRUE)
# 
# get_results_all(dir = getwd(), scenario = fil[30])
#############################################

scalar.list.out <- scalar.list
ts.list.out <- ts.list

scalar.all <- do.call(plyr::rbind.fill, scalar.list.out)
scalar.all$ID <- paste(scalar.all$scenario, scalar.all$replicate, sep = "-")

ts.all <- do.call(plyr::rbind.fill, ts.list.out)
ts.all$ID <- paste(ts.all$scenario, ts.all$replicate, sep="-")
write.csv(scalar.all, file="ss3sim_scalar_7_22.csv")
write.csv(ts.all, file="ss3sim_ts_7_22.csv")

#Save .Rdata files to home volumes
save(scalar.all, file = "Y://Empirical//results//ss3sim_scalar_7_22.Rdata")
save(ts.all, file = "Y://Empirical//results//ss3sim_ts_7_22.Rdata")



