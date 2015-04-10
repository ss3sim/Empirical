#load results into R
results.sc <- read.csv('ss3sim_scalar_lab.csv')
results.ts <- read.csv('ss3sim_ts_lab.csv')

#Remove G2 results
results.sc <- subset(results.sc, G != "G2")
results.ts <- subset(results.ts, G != "G2")

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

#Add end year b, last 10 year b, last 25 year b
results.ts <- results.ts %>% group_by(ID) %>% 
  mutate(end.b_re = SpawnBio_re[year == 100],
    last.10.b_re = mean(SpawnBio_re[year >= 91]),
    last.25.b_re = mean(SpawnBio_re[year >= 76]),
    last.60.b_re = mean(SpawnBio_re[year >= 40]),
    mare = median(abs(SpawnBio_re)),
    mre = median(SpawnBio_re))
results.ts <- as.data.frame(results.ts)


results.ts$converged <- ifelse(results.ts$log_max_grad <= log(0.1), 'yes', 'no')

#Biomass results only
ssb.ts.long <- melt(results.ts, measure.vars = 'SpawnBio_re',
  id.vars= c("ID","species", "replicate",
         "log_max_grad", "year", 'D', 'X', 'G', 'E',
         "end.b_re", 'last.10.b_re', 'last.25.b_re', 'converged',
         'last.60.b_re', 'mare', 'mre'))

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
  'data.amount'] <- 'aa rich - late survey'

#only converged runs
hake <- subset(ssb.ts.long, converged == 'yes' & species == 'hake-age')
hake.w <- hake[is.na(hake$X) == FALSE, ]
hake.l <- hake[is.na(hake$X), ]

yellow <- subset(ssb.ts.long, converged == 'yes' & species == 'yellow-age')
yellow.w <- yellow[is.na(yellow$X) == FALSE, ]
yellow.l <- yellow[is.na(yellow$X), ]

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







