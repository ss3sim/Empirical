#test codOM with mean-size-at-age data included

#cod OM has three fleets (1=fishery, 2=survey, 3=CPUE)
#mirror fleet 3 wt-at-age from fleet 1
setwd("C:/NOAA2015/ss3Sim_Empirical/test")

years <- list(
			c(seq(26,100, by=1),100),  #fishery
			c(seq(75,100, by=4),100),  #survey
			1  #CPUE mirrored from fleet 1
			)

infile <- "wtatage.ss_new"
datfile <- "data.ss_new"
ctlfile <- "control.ss_new"
outfile <- "wtatage.ss"

sample_wtatage(infile=infile, outfile=outfile,
			   datfile=datfile, ctlfile=ctlfile,
               years=years, fill_fnc=fill_across,
               write_file=TRUE)


