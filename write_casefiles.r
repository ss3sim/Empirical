# case_folder <- paste0(case_folder_orig, '/', species)
old.dir <- getwd()

### ------------------------------------------------------------
#Make sure that case files are copied appropriately from ss3models package
orig.species <- species

setwd(system.file('cases',package="ss3models"))
ffiles <- list.files()
to.move <- ffiles[grep(species, ffiles)]
rw <- 0

if(length(to.move) == 0){
  species <- gsub("rw", "", species)
  to.move <- ffiles[grep(species, ffiles)]  
  rw <- 1
}

file.copy(to.move, to = paste0(old.dir, '/', case_folder))
setwd(paste0(old.dir, '/', case_folder))

#Rename if time-varying estimation modifier present
if(rw == 1){
  froms <- list.files()
  new.species <- gsub("-", "rw-", species)
  tos <- gsub(species, new.species, froms )
  file.rename(froms, tos)
}

#Change Directory back
setwd(old.dir)

species <- orig.species
### ------------------------------------------------------------
#Source to write casefiles
#1---------"Deterministic" Scenario
# index1 <- c(' fleets; c(2, 3)',
#             'years; list(seq(26, 100, 1), seq(41, 100, 1))',
#             'sds_obs; list(.05, .05)')
# writeLines(index1, con = paste0(case_folder, '/', 'index1-',
#                                 species, '.txt'))

# agecomp1 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1),seq(41,100, by=1))',
#               'Nsamp;list(1000, 1000)', 'cpar;NA')
# # 'Nsamp;list(1000, 1000)', 'cpar;NA')
# writeLines(agecomp1, con = paste0(case_folder, '/', 'agecomp1-',
#                                   species, '.txt'))
# writeLines(agecomp1, con = paste0(case_folder, '/', 'lcomp1-',
#                                   species, '.txt'))

# wtatage1 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1),seq(41,100, by=1))',
#               'write_file; TRUE', 'cv_wtatage; .05')
# writeLines(wtatage1, con = paste0(case_folder, '/', 'wtatage1-',
#                                   species, '.txt'))

# #2---------Data Unrealistic Scenario, mid range age comp sample size
# index2 <- c('fleets; c(2, 3)',
#             'years; list(seq(26, 100, 1), seq(41, 100, 1))',
#             'sds_obs; list(.05, .05)')
# writeLines(index2, con = paste0(case_folder, '/', 'index2-',
#                                 species, '.txt'))

# agecomp2 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1),seq(41,100, by=1))',
#               'Nsamp;list(500, 500)', 'cpar;NA')
# writeLines(agecomp2, con = paste0(case_folder, '/', 'agecomp2-',
#                                   species, '.txt'))
# writeLines(agecomp2, con = paste0(case_folder, '/', 'lcomp2-',
#                                   species, '.txt'))

# wtatage2 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1),seq(41,100, by=1))',
#               'write_file; TRUE', 'cv_wtatage; .05')
# writeLines(wtatage2, con = paste0(case_folder, '/', 'wtatage2-',
#                                   species, '.txt'))

# #3---------Realistic Data Scenario, Triennial Survey, SD of .2
# index3 <- c('fleets; c(2, 3)',
#             'years; list(seq(26, 100, 1), seq(41, 100, 3))',
#             'sds_obs; list(.2, .2)')
# writeLines(index3, con = paste0(case_folder, '/', 'index3-',
#                                 species, '.txt'))

# agecomp3 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1),seq(41, 100, by = 3))',
#               'Nsamp; list(c(rep(35, 25), rep(75, 25), rep(100, 25)), rep(100, 20))', 
#               'cpar;NA')
# writeLines(agecomp3, con = paste0(case_folder, '/', 'agecomp3-',
#                                   species, '.txt'))
# writeLines(agecomp3, con = paste0(case_folder, '/', 'lcomp3-',
#                                   species, '.txt'))

# wtatage3 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1),seq(41,100, by=3))',
#               'write_file; TRUE', 'cv_wtatage; .2')
# writeLines(wtatage3, con = paste0(case_folder, '/', 'wtatage3-',
#                                   species, '.txt'))

# #4---------Realistic Data, Late Triennial Survey
# index4 <- c('fleets; c(2, 3)',
#             'years; list(seq(26, 100, 1), seq(67, 100, 3))',
#             'sds_obs; list(.2, .2)')
# writeLines(index4, con = paste0(case_folder, '/', 'index4-',
#                                 species, '.txt'))

# agecomp4 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1), seq(67, 100, by = 3))',
#               'Nsamp; list(c(rep(35, 25), rep(75, 25), rep(100, 25)), rep(100, 1))', 
#               'cpar;NA')
# writeLines(agecomp4, con = paste0(case_folder, '/', 'agecomp4-',
#                                   species, '.txt'))
# writeLines(agecomp4, con = paste0(case_folder, '/', 'lcomp4-',
#                                   species, '.txt'))

# wtatage4 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1), seq(67,100, by=3))',
#               'write_file; TRUE', 'cv_wtatage; .2')
# writeLines(wtatage4, con = paste0(case_folder, '/', 'wtatage4-',
#                                   species, '.txt'))

# #5---------Moderate Data
# index5 <- c('fleets; c(2, 3)',
#             'years; list(seq(26, 100, 1), seq(76, 100, 2))',
#             'sds_obs; list(.2, .2)')
# writeLines(index5, con = paste0(case_folder, '/', 'index5-',
#                                 species, '.txt'))

# agecomp5 <- c('fleets;c(1,2)', 'years;list(c(36,46,seq(51,66,by=5),71:100), seq(76,100, by=2))',
#               'Nsamp; list(c(rep(35, 1), rep(75, 5), rep(100, 30)), rep(100, 13))', 
#               'cpar;NA')
# writeLines(agecomp5, con = paste0(case_folder, '/', 'agecomp5-',
#                                   species, '.txt'))
# writeLines(agecomp5, con = paste0(case_folder, '/', 'lcomp5-',
#                                   species, '.txt'))

# wtatage5 <- c('fleets;c(1,2)', 'years;list(c(36,46,seq(51,66,by=5),71:100), seq(76,100, by=2))',
#               'write_file; TRUE', 'cv_wtatage; .2')
# writeLines(wtatage5, con = paste0(case_folder, '/', 'wtatage5-',
#                                   species, '.txt'))

#---------------------------------------------------------------------------------------
#With Dirichlet (2 indicates cpar value)
#1---------"Deterministic" Scenario
index1 <- c(' fleets; c(2, 3)',
             'years; list(seq(26, 100, 1), seq(41, 100, 1))',
             'sds_obs; list(.05, .05)')
writeLines(index1, con = paste0(case_folder, '/', 'index1-',
                                 species, '.txt'))

agecomp1 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1),seq(41,100, by=1))',
               # 'Nsamp;list(1000, 1000)', 'cpar;NA')
               'Nsamp;list(1000, 1000)', 'cpar;c(2, 2)')
writeLines(agecomp1, con = paste0(case_folder, '/', 'agecomp1-',
                                   species, '.txt'))
writeLines(agecomp1, con = paste0(case_folder, '/', 'lcomp1-',
                                   species, '.txt'))

wtatage1 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1),seq(41,100, by=1))',
               'write_file; TRUE', 'cv_wtatage; .05')
writeLines(wtatage1, con = paste0(case_folder, '/', 'wtatage1-',
                                   species, '.txt'))

#2---------Data Unrealistic Scenario, mid range age comp sample size
index2 <- c('fleets; c(2, 3)',
             'years; list(seq(26, 100, 1), seq(41, 100, 1))',
             'sds_obs; list(.05, .05)')
writeLines(index2, con = paste0(case_folder, '/', 'index2-',
                                 species, '.txt'))

agecomp2 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1),seq(41,100, by=1))',
               'Nsamp;list(500, 500)', 'cpar;c(2, 2)')
writeLines(agecomp2, con = paste0(case_folder, '/', 'agecomp2-',
                                   species, '.txt'))
writeLines(agecomp2, con = paste0(case_folder, '/', 'lcomp2-',
                                   species, '.txt'))

wtatage2 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1),seq(41,100, by=1))',
               'write_file; TRUE', 'cv_wtatage; .05')
writeLines(wtatage2, con = paste0(case_folder, '/', 'wtatage2-',
                                   species, '.txt'))

#3---------Realistic Data Scenario, Triennial Survey, SD of .2
index3 <- c('fleets; c(2, 3)',
             'years; list(seq(26, 100, 1), seq(41, 100, 3))',
             'sds_obs; list(.2, .2)')
writeLines(index3, con = paste0(case_folder, '/', 'index3-',
                                 species, '.txt'))

agecomp3 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1),seq(41, 100, by = 3))',
               'Nsamp; list(c(rep(35, 25), rep(75, 25), rep(100, 25)), rep(100, 20))', 
               'cpar;c(2, 2)')
writeLines(agecomp3, con = paste0(case_folder, '/', 'agecomp3-',
                                   species, '.txt'))
writeLines(agecomp3, con = paste0(case_folder, '/', 'lcomp3-',
                                   species, '.txt'))

wtatage3 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1),seq(41,100, by=3))',
               'write_file; TRUE', 'cv_wtatage; .2')
writeLines(wtatage3, con = paste0(case_folder, '/', 'wtatage3-',
                                   species, '.txt'))

#4---------Realistic Data, Late Triennial Survey
index4 <- c('fleets; c(2, 3)',
             'years; list(seq(26, 100, 1), seq(67, 100, 3))',
             'sds_obs; list(.2, .2)')
writeLines(index4, con = paste0(case_folder, '/', 'index4-',
                                 species, '.txt'))

agecomp4 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1), seq(67, 100, by = 3))',
               'Nsamp; list(c(rep(35, 25), rep(75, 25), rep(100, 25)), rep(100, 1))', 
               'cpar;c(2, 2)')
writeLines(agecomp4, con = paste0(case_folder, '/', 'agecomp4-',
                                   species, '.txt'))
writeLines(agecomp4, con = paste0(case_folder, '/', 'lcomp4-',
                                   species, '.txt'))

wtatage4 <- c('fleets;c(1,2)', 'years;list(seq(26,100, by=1), seq(67,100, by=3))',
               'write_file; TRUE', 'cv_wtatage; .2')
writeLines(wtatage4, con = paste0(case_folder, '/', 'wtatage4-',
                                   species, '.txt'))

#5---------Moderate Data
index5 <- c('fleets; c(2, 3)',
             'years; list(seq(26, 100, 1), seq(76, 100, 2))',
             'sds_obs; list(.2, .2)')
writeLines(index5, con = paste0(case_folder, '/', 'index5-',
                                 species, '.txt'))

agecomp5 <- c('fleets;c(1,2)', 'years;list(c(36,46,seq(51,66,by=5),71:100), seq(76,100, by=2))',
               'Nsamp; list(c(rep(35, 1), rep(75, 5), rep(100, 30)), rep(100, 13))', 
               'cpar;c(2, 2)')
writeLines(agecomp5, con = paste0(case_folder, '/', 'agecomp5-',
                                   species, '.txt'))
writeLines(agecomp5, con = paste0(case_folder, '/', 'lcomp5-',
                                   species, '.txt'))

wtatage5 <- c('fleets;c(1,2)', 'years;list(c(36,46,seq(51,66,by=5),71:100), seq(76,100, by=2))',
               'write_file; TRUE', 'cv_wtatage; .2')
writeLines(wtatage5, con = paste0(case_folder, '/', 'wtatage5-',
                                   species, '.txt'))

if(length(grep('yellow', species)) != 0){
  E2 <- c("natM_type; 1Parm", 
          "natM_n_breakpoints; NULL",
          "natM_lorenzen; NULL", 
          "natM_val; c(NA, NA)",
          "par_name; c('L_at_Amin', 'L_at_Amax', 'VonBert_K', 'CV_young', 'CV_old')",
          "par_int; c(18, 62, .047, .13, .13)", 
          "par_phase; c(6, 6, 6, 7, 7)",
          "forecast_num; 0", 
          "run_change_e_full; TRUE")
  writeLines(E2, con = paste0(case_folder, '/', 'E2-', 
                              species, '.txt'))
}

if(length(grep('hake', species)) != 0){
  E2 <- c("natM_type; 1Parm", 
          "natM_n_breakpoints; NULL",
          "natM_lorenzen; NULL", 
          "natM_val; c(NA, NA)",
          "par_name; c('L_at_Amin_Fem_GP_1', 'L_at_Amax_Fem_GP_1', 'VonBert_K_Fem_GP_1','CV_young_Fem_GP_1', 'CV_old_Fem_GP_1')",
          "par_int; c(5, 55, .4, .1, .1)", 
          "par_phase; c(4, 4, 4, 7, 7)",
          "forecast_num; 0", 
          "run_change_e_full; TRUE")
  writeLines(E2, con = paste0(case_folder, '/', 'E2-', 
                              species, '.txt'))
}

if(length(grep('mackerel', species)) != 0){
  E2 <- c("natM_type; 1Parm", 
          "natM_n_breakpoints; NULL",
          "natM_lorenzen; NULL", 
          "natM_val; c(NA, NA)",
          "par_name; c('L_at_Amin_Fem_GP_1', 'L_at_Amax_Fem_GP_1', 'VonBert_K_Fem_GP_1','CV_young_Fem_GP_1', 'CV_old_Fem_GP_1')",
          "par_int; c(15, 45, .35, .1, .1)", 
          "par_phase; c(4, 4, 4, 7, 7)",
          "forecast_num; 0", 
          "run_change_e_full; TRUE")
  writeLines(E2, con = paste0(case_folder, '/', 'E2-', 
                              species, '.txt'))
}
#Write E casefile to turn on growth estimation