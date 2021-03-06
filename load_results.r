#######################################
### Load required packages
#devtools::install_github("ss3sim/ss3sim")
# library(ss3sim)
# library(plyr)
# library(dplyr)
library(reshape2)
# library(ggplot2)

#######################################
### Set wd
setwd("/Volumes/home/Empirical/results")
# setwd("Y://Empirical//results")
# #load results into R
load('ss3sim_scalar_7_23.Rdata')
load('ss3sim_ts_7_23.Rdata')

results.sc <- scalar.all
results.ts <- ts.all
rm(scalar.all)
rm(ts.all)

load("ss3sim_scalar_7_22_block.Rdata")
results.sc.block <- scalar.all 
load("ss3sim_ts_7_22_block.Rdata")
results.ts.block <- ts.all

#Get dimensions right on each data frame
results.sc.block$X <- 'none'
# names(results.sc)[names(results.sc) %in% names(results.sc.block) == FALSE]
# names(results.sc.block)[names(results.sc.block) %in% names(results.sc) == FALSE]
results.sc$L_at_Amax_Fem_GP_1_BLK1repl_40_em <- 'none'
results.sc$L_at_Amax_Fem_GP_1_BLK1repl_60_em <- 'none'

results.sc <- rbind(results.sc, results.sc.block)

results.ts.block$X <- 'none'
results.ts <- rbind(results.ts, results.ts.block)

# names(results.ts)[names(results.ts) %in% names(results.ts.block) == FALSE]
# names(results.ts.block)[names(results.ts.block) %in% names(results.ts) == FALSE]

# #---------------------------------------
# #Change so results.ts and results.sc handle hyphens 
# #Time series results
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

#Do calcluations on data frames
results.sc$log_max_grad <- log(results.sc$max_grad)
results.sc$converged <- ifelse(results.sc$max_grad<.1, "yes", "no")
results.sc <- calculate_re(results.sc, add=TRUE)
results.sc$runtime <- results.sc$RunTime

#Drop fixed parameters(columns of 0)
results.sc <- results.sc[,-which(apply(results.sc, 2, function(x) all(x==0)))]
results.sc <- subset(results.sc, max_grad <= 0.1)

re.names <- names(results.sc)[grep('_re', names(results.sc))]

results.sc.long <-
    melt(results.sc, measure.vars=re.names,
         id.vars= c("species", "replicate",
         "log_max_grad", "params_on_bound_em", 'D', 'G', 'X', 'scenario'))

growth.names <- re.names[grep("GP_", re.names)]
results.sc.long.growth <- droplevels(subset(results.sc.long, variable %in% growth.names))
results.sc.long.growth$variable <- gsub("_Fem_GP_1_re|_re", "", results.sc.long.growth$variable)
selex.names <- re.names[grep("Sel_", re.names)]
results.sc.long.selex <- droplevels(subset(results.sc.long, variable %in% selex.names))
results.sc.long.selex$variable <- gsub("ery|ey|Size|_re", "", results.sc.long.selex$variable)
results.sc.long.selex$variable <- gsub("_", ".", results.sc.long.selex$variable)
management.names <- c("SSB_MSY_re", "depletion_re", "SSB_Unfished_re", "Catch_endyear_re")
results.sc.long.management <- droplevels(subset(results.sc.long, variable %in% management.names))
results.sc.long.management$variable <- gsub("_re", "", results.sc.long.management$variable)

#Time series calculations
results.ts <- calculate_re(results.ts)
results.ts <- merge(results.ts, results.sc[, c('log_max_grad', 'ID')],
  by = 'ID', all = TRUE)

#Add column indicating convergence
results.ts$converged <- ifelse(results.ts$log_max_grad <= log(0.1), 'yes', 'no')

#----------------------------------------------------------------------------


#----------------------------------------------------------------------------
#Check numbers that converged
temp1 <- as.data.frame(subset(results.ts, converged == 'yes') %>% group_by(scenario) %>% 
        summarise(Nconverged = length(unique(replicate, na.rm = TRUE))))

temp2 <- as.data.frame(subset(results.ts, is.na(converged)) %>% group_by(scenario) %>%
  summarise(Nunconverged = length(unique(replicate, na.rm = TRUE))))

temp <- merge(temp1, temp2, by = 'scenario', all = TRUE)

xx <- temp %>% group_by(scenario) %>% mutate(some = sum(Nconverged, Nunconverged, na.rm = TRUE)) %>%
  as.data.frame

yy <- subset(xx, some >= 120)
yy$ratio <- yy$Nconverged / yy$some

$ratio <- 

te
temp$ssumm <- sum(temp$Nconverged, temp$Nunconverged, na.rm = T)
# temp[temp$Nconverged < 120, 'scenario']

#Which ones didn't converge?
# check <- subset(results.ts, scenario == 'D2-E2-F1-G1-hake-age' & is.na(converged))
# ggplot(check, aes(x = year, y = SpawnBio_re, group = replicate)) + geom_line()
# nope <- results.ts[is.na(results.ts$converged), ]
# nope <- as.data.frame(nope %>% group_by(scenario) %>% summarise(N = length(unique(replicate))))
# nope[which(nope$N > 20), 'scenarior']
# unique(paste(nope$scenario, nope$replicate))
# unique(nope$scenario)
# nope <- subset(results.ts, converged != 'yes')

#----------------------------------------------------------------------------
#Subset only converged Runs
results.ts <- subset(results.ts, converged == 'yes')
# results.ts <- results.ts[-which(is.infinite(results.ts$log_max_grad) == TRUE), ]
# results.ts %>% group_by(scenario) %>% summarise(nroww = length(unique(replicate))) %>% as.data.frame

# xxx <- subset(results.ts, year > 25)


# xxx <- xxx %>% group_by(scenario, replicate) %>% mutate(maxx = max(SpawnBio_re)) %>% as.data.frame

# xxx2 <- xxx[-which(xxx$maxx > 1), ]


#----------------------------------------------------------------------------
# unique(results.ts[is.infinite(results.ts$log_max_grad), "ID"])
#Remove Duplicated

#Check some plots out
# ggplot(xxx, aes(x = year, y = SpawnBio_re, group = replicate)) + facet_wrap(~scenario) + geom_line()

# results.ts$unq <- paste(result.ts$ID, results.ts)

#Add end year b, last 10 year b, last 25 year b
# results.ts <-  results.ts %>% group_by(ID) %>% 
results.ts <- results.ts %>% distinct(ID, year) %>% group_by(ID) %>%
  mutate(end.b_re = SpawnBio_re[year == 100],
    last.10.b_re = mean(SpawnBio_re[year >= 91]),
    last.25.b_re = mean(SpawnBio_re[year >= 76]),
    last.60.b_re = mean(SpawnBio_re[year >= 40]),
    last.50.b_re = mean(SpawnBio_re[year >= 50]),
    median_ = median(SpawnBio_re)) %>% group_by(scenario) %>% mutate(first.100 = 
    unique(replicate, na.rm = TRUE)[order(unique(replicate, 
      na.rm = TRUE))][100]) %>% as.data.frame
    # mare = median(abs(SpawnBio_re)),
    # mre = median(SpawnBio_re))
# results.ts <- as.data.frame(results.ts)

#

# results.ts <- subset(results.ts, replicate <= first.100)

# xx <- results.ts %>% group_by(scenario) %>% summarise(Nreps = length(unique(replicate)))

#----------------------------------------------------------------------------
#Biomass results only
ssb.ts.long <- melt(results.ts, measure.vars = 'SpawnBio_re',
  id.vars= c("ID","species", "replicate",
         "log_max_grad", "year", 'D', 'X', 'G', 'E',
         "end.b_re", 'last.10.b_re', 'last.25.b_re', 'converged',
         'last.60.b_re', 'median_', 'scenario',
         "last.50.b_re"))

#Add informative label for growth
gg <- data.frame(G = as.factor(c("G0", 'G1')), 
  g.desc = c('invariant', 'varying'))
ssb.ts.long <- merge(ssb.ts.long, gg, by = 'G', all = TRUE)

ssb.ts.long$data.desc <- 1
ssb.ts.long[grep("E" , ssb.ts.long$ID), 'data.desc'] <- 'A + L'
ssb.ts.long[grep("X" , ssb.ts.long$ID), 'data.desc'] <- 'WtAtAge'

ssb.ts.long$data.amount <- 1
ssb.ts.long$D <- as.character(ssb.ts.long$D)
ssb.ts.long[which(ssb.ts.long$D == 'D2' | ssb.ts.long$X == 'X2'), 
  'data.amount'] <- 'unrealistic'
ssb.ts.long[which(ssb.ts.long$D == 'D3' | ssb.ts.long$X == 'X3'), 
  'data.amount'] <- 'rich'
ssb.ts.long[which(ssb.ts.long$D == 'D4' | ssb.ts.long$X == 'X4'), 
  'data.amount'] <- 'rich - late survey'
ssb.ts.long[which(ssb.ts.long$D == 'D5' | ssb.ts.long$X == 'X5'), 
  'data.amount'] <- 'moderate'

#Add informative labels for management data
gg <- data.frame(G = as.factor(c("G0", 'G1')), 
  g.desc = c('invariant', 'varying'))
results.sc.long.management <- merge(results.sc.long.management, gg, by = 'G', all = TRUE)

results.sc.long.management$data.desc <- 1
results.sc.long.management[grep("E" , results.sc.long.management$scenario), 'data.desc'] <- 'A + L'
results.sc.long.management[grep("X" , results.sc.long.management$scenario), 'data.desc'] <- 'WtAtAge'

results.sc.long.management$data.amount <- 1
results.sc.long.management$D <- as.character(results.sc.long.management$D)
results.sc.long.management[which(results.sc.long.management$D == 'D2' | results.sc.long.management$X == 'X2'), 
  'data.amount'] <- 'unrealistic'
results.sc.long.management[which(results.sc.long.management$D == 'D3' | results.sc.long.management$X == 'X3'), 
  'data.amount'] <- 'rich'
results.sc.long.management[which(results.sc.long.management$D == 'D4' | results.sc.long.management$X == 'X4'), 
  'data.amount'] <- 'rich - late survey'
results.sc.long.management[which(results.sc.long.management$D == 'D5' | results.sc.long.management$X == 'X5'), 
  'data.amount'] <- 'moderate'

#Add informative labels for selex data
gg <- data.frame(G = as.factor(c("G0", 'G1')), 
  g.desc = c('invariant', 'varying'))
results.sc.long.selex <- merge(results.sc.long.selex, gg, by = 'G', all = TRUE)

results.sc.long.selex$data.desc <- 1
results.sc.long.selex[grep("E" , results.sc.long.selex$scenario), 'data.desc'] <- 'A + L'
results.sc.long.selex[grep("X" , results.sc.long.selex$scenario), 'data.desc'] <- 'WtAtAge'

results.sc.long.selex$data.amount <- 1
results.sc.long.selex$D <- as.character(results.sc.long.selex$D)
results.sc.long.selex[which(results.sc.long.selex$D == 'D2' | results.sc.long.selex$X == 'X2'), 
  'data.amount'] <- 'unrealistic'
results.sc.long.selex[which(results.sc.long.selex$D == 'D3' | results.sc.long.selex$X == 'X3'), 
  'data.amount'] <- 'rich'
results.sc.long.selex[which(results.sc.long.selex$D == 'D4' | results.sc.long.selex$X == 'X4'), 
  'data.amount'] <- 'rich - late survey'
results.sc.long.selex[which(results.sc.long.selex$D == 'D5' | results.sc.long.selex$X == 'X5'), 
  'data.amount'] <- 'moderate'



# #only converged runs
# hake <- subset(ssb.ts.long, converged == 'yes' & species == 'hake-age')
# hake.w <- hake[is.na(hake$X) == FALSE, ]
# hake.l <- hake[is.na(hake$X), ]

# yellow <- subset(ssb.ts.long, converged == 'yes' & species == 'yellow-age')
# yellow.w <- yellow[is.na(yellow$X) == FALSE, ]
# yellow.l <- yellow[is.na(yellow$X), ]

# #Checks
# checks <- subset(ssb.ts.long, converged == 'yes')  
# checks.e <- subset(ssb.ts.long, converged == 'yes' & 
#   data.desc == 'A + L')
# checks.x <- subset(ssb.ts.long, converged == 'yes' & 
#   data.desc == 'WtAtAge')





# checks.e 


# #Length Results only
# ssb.ts.e <- subset(ssb.ts.long, E == "E2")

# #wtatage results only
# ssb.ts.x <- ssb.ts.long[is.na(ssb.ts.long$X) == FALSE, ]


# #add and merge more informative labels
# dd <- data.frame(D = as.factor(c("D2", 'D3', 'D4')), 
#   d.desc = c('unrealistic', 'rich', 'rich - late survey'))
# ssb.ts.e <- merge(ssb.ts.e, dd, by = 'D', all = TRUE)

# xx <- data.frame(X = as.factor(c("X2", 'X3', 'X4')), 
#   x.desc = c('unrealistic', 'rich', 'rich - late survey'))
# ssb.ts.x <- merge(ssb.ts.x, xx, by = 'X', all = TRUE)







