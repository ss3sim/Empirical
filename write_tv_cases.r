#Check and plot Growth varying scenarios

#------------------------------------------------------------------------
#Set Working Directory, load files
# setwd('/Users/peterkuriyama/School/Research/capam_growth/Empirical/')
#Paths are structured as they are in the ss3sim repo

#------------------------------------------------------------------------
#FUnction to write tv casefiles
write_tv <- function(species, value, perc.change,
  file.name, case_folder = 'cases')
{
  currd <- getwd()
  
  #Modify Linf by percentage
  setwd(ss3model(species, 'om'))
  ctl <- readLines('ss3.ctl')
  line <- ctl[grep(value, ctl)]
  val <- as.numeric(strsplit(line, split = ' ')[[1]][4])

  #Visually check when modifying
  # cat('line is ', line, '\n',
  #   'value is ', val, '\n')

  #Create vector of deviates
  dev <- val + val * perc.change  - val
  
  #Prep output for file
  dev <- paste0(dev, collapse = ', ')
  header <- c('dev; ', 'c(')
  out <- c(header, dev, ')')
  out <- paste0(out, collapse = '')
   
  GG <- c('function_type; change_tv',
    paste0('param; ', value),
    out)
 
  setwd(currd) 
 
  writeLines(GG, con = paste0(case_folder, '/', file.name, '-', species, '.txt'))
}  

# case_folder_orig <- 'cases'

#------------------------------------------------------------------------
# species <- 'hakeAge'
# case_folder <- paste0(case_folder_orig, '/', species)

#------------------------------------------------------------------------
#Modify Linfinity
value <- 'L_at_Amax_Fem_GP_1'

#Constant Growth
G0 <- rep(0, 100)
write_tv(species = species, value = value, file.name = 'G0',
  perc.change = G0, case_folder = case_folder)

#Period of positive and negative growth
G1 <- c(rep(0, 40), seq(0, .3, length.out = 10), rev(seq(0, .3, length.out = 10)),
  seq(0, -.3, length.out = 20), rev(seq(0, -.3, length.out = 20)))
write_tv(species = species, value = value, file.name = 'G1',
  perc.change = G1, case_folder = case_folder)

#Quick Periods of high variation
G2 <- c(rep(0, 40), seq(.1, .2, length.out = 10), rev(seq(.1, .2, length.out = 10)),
  0, 0, 0, seq(.2, .3, length.out = 5), .3, .3, rev(seq(.1, .3, length.out = 30)))
write_tv(species = species, value = value, file.name = 'G2',
  perc.change = G2, case_folder = case_folder)

#Random variations
G3 <- c(rep(0, 40), rnorm(60) / 10)
write_tv(species = species, value = value, file.name = 'G3',
  perc.change = G3, case_folder = case_folder)

#------------------------------------------------------------------------
#modify k
value <- 'VonBert_K_Fem_GP_1'
#Period of positive and negative growth
G4 <- c(rep(0, 40), seq(0, .3, length.out = 10), rev(seq(0, .3, length.out = 10)),
  seq(0, -.3, length.out = 20), rev(seq(0, -.3, length.out = 20)))
write_tv(species = species, value = value, file.name = 'G4',
  perc.change = G4, case_folder = case_folder)

#Quick Periods of high variation
G5 <- c(rep(0, 40), seq(.1, .2, length.out = 10), rev(seq(.1, .2, length.out = 10)),
  0, 0, 0, seq(.2, .3, length.out = 5), .3, .3, rev(seq(.1, .3, length.out = 30)))
write_tv(species = species, value = value, file.name = 'G5',
  perc.change = G5, case_folder = case_folder)

#Random variations
G6 <- c(rep(0, 40), rnorm(60) / 10)
write_tv(species = species, value = value, file.name = 'G6',
  perc.change = G6, case_folder = case_folder)

#Constant 
# G7 <- rep(0, 100)
# write_tv(species = species, value = value, file.name = 'G7',
#   perc.change = G7, case_folder = case_folder)


#------------------------------------------------------------------------
#Run Them
# case_files <- list(F = 'F', D = c('index', 'agecomp'), X = 'wtatage',
#   G = 'G')
# scenarios <- 'F0-D1-G1-X1-hake'

# run_ss3sim(iterations = 10, scenarios = scenarios, case_folder = case_folder,
#   om_dir = ss3model(species, 'om'), em_dir = ss3model(species, 'em'), case_files = case_files, parallel = TRUE)




