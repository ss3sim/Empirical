# case_folder <- paste0(case_folder_orig, '/', species)
old.dir <- getwd()

### ------------------------------------------------------------
#Make sure that case files are copied appropriately from ss3models package

setwd(system.file('cases',package="ss3models"))
ffiles <- list.files()
to.move <- ffiles[grep(species, ffiles)]
file.copy(to.move, to = paste0(old.dir, '/', case_folder))

#Change Directory back
setwd(old.dir)

### ------------------------------------------------------------
#Source to write casefiles
#1---------"Deterministic" Scenario
index1 <- c('fleets; c(2, 3)',
  'years; list(seq(25, 100, 1), seq(40, 100, 1))',
  'sds_obs; list(.05, .05)')
writeLines(index1, con = paste0(case_folder, '/', 'index1-',
  species, '.txt'))

agecomp1 <- c('fleets;c(1,2)', 'years;list(seq(25,100, by=1),seq(40,100, by=1))',
              'Nsamp;list(1000, 1000)', 'cpar;NA')
writeLines(agecomp1, con = paste0(case_folder, '/', 'agecomp1-',
  species, '.txt'))
writeLines(agecomp1, con = paste0(case_folder, '/', 'lcomp1-',
  species, '.txt'))

wtatage1 <- c('fleets;c(1,2)', 'years;list(seq(25,100, by=1),seq(40,100, by=1))',
              'write_file; TRUE', 'cv_wtatage; .05')
writeLines(wtatage1, con = paste0(case_folder, '/', 'wtatage1-',
  species, '.txt'))

#2---------Data Unrealistic Scenario, mid range age comp sample size
index2 <- c('fleets; c(2, 3)',
  'years; list(seq(25, 100, 1), seq(40, 100, 1))',
  'sds_obs; list(.05, .05)')
writeLines(index2, con = paste0(case_folder, '/', 'index2-',
  species, '.txt'))

agecomp2 <- c('fleets;c(1,2)', 'years;list(seq(25,100, by=1),seq(40,100, by=1))',
              'Nsamp;list(500, 500)', 'cpar;NA')
writeLines(agecomp2, con = paste0(case_folder, '/', 'agecomp2-',
  species, '.txt'))
writeLines(agecomp2, con = paste0(case_folder, '/', 'lcomp2-',
  species, '.txt'))

wtatage2 <- c('fleets;c(1,2)', 'years;list(seq(25,100, by=1),seq(40,100, by=1))',
              'write_file; TRUE', 'cv_wtatage; .05')
writeLines(wtatage2, con = paste0(case_folder, '/', 'wtatage2-',
  species, '.txt'))

#3---------Realistic Data Scenario, Triennial Survey, SD of .2
index3 <- c('fleets; c(2, 3)',
  'years; list(seq(25, 100, 1), seq(40, 100, 3))',
  'sds_obs; list(.2, .2)')
writeLines(index3, con = paste0(case_folder, '/', 'index3-',
  species, '.txt'))

agecomp3 <- c('fleets;c(1,2)', 'years;list(seq(25,100, by=1),seq(40, 100, by = 3))',
              'Nsamp; list(c(rep(35, 25), rep(75, 25), rep(100, 26)), rep(100, 21))', 
              'cpar;NA')
writeLines(agecomp3, con = paste0(case_folder, '/', 'agecomp3-',
  species, '.txt'))
writeLines(agecomp3, con = paste0(case_folder, '/', 'lcomp3-',
  species, '.txt'))

wtatage3 <- c('fleets;c(1,2)', 'years;list(seq(25,100, by=1),seq(40,100, by=3))',
              'write_file; TRUE', 'cv_wtatage; .2')
writeLines(wtatage3, con = paste0(case_folder, '/', 'wtatage3-',
  species, '.txt'))

#4---------Realistic Data, Late Triennial Survey
index4 <- c('fleets; c(2, 3)',
  'years; list(seq(25, 100, 1), seq(67, 100, 3))',
  'sds_obs; list(.2, .2)')
writeLines(index4, con = paste0(case_folder, '/', 'index4-',
  species, '.txt'))

agecomp4 <- c('fleets;c(1,2)', 'years;list(seq(25,100, by=1), seq(67, 100, by = 3))',
              'Nsamp; list(c(rep(35, 25), rep(75, 25), rep(100, 26)), rep(100, 12))', 
              'cpar;NA')
writeLines(agecomp4, con = paste0(case_folder, '/', 'agecomp4-',
  species, '.txt'))
writeLines(agecomp4, con = paste0(case_folder, '/', 'lcomp4-',
  species, '.txt'))

wtatage4 <- c('fleets;c(1,2)', 'years;list(seq(25,100, by=1), seq(67,100, by=3))',
              'write_file; TRUE', 'cv_wtatage; .2')
writeLines(wtatage4, con = paste0(case_folder, '/', 'wtatage4-',
  species, '.txt'))



