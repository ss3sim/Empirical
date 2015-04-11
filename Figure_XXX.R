#####################################
### Figure XXX for Peter's paper

#####################################
### Some controls

### Directory with the results
results.dir <- "C:/Users/Felipe/Desktop/Growth trials/growth_figs"
setwd(results.dir)

### Source the plotting functoions
source("C:/Users/Felipe/Documents/GitHub/Empirical/Plots_for_paper.R")

### Scenarios
species.vec <- c('yellow-age', 'hake-age')
scenarios <- c(paste("D4-F0-G",c(3,1,2,4),"-X4-",species.vec[1], sep=""),
               paste("D4-F0-G",c(3,1,2,4),"-X4-",species.vec[2], sep=""))
spp.names <- c("Yelloweye", "Hake")

### File where the figure is gonna be stored
png(file = "Figure_XXX.png",
    height = 12, width = 8, units = 'in', res = 500)

### Set the plotting matrix
matlay <- matrix(c(1,2,9,
                   3,4,10,
                   5,6,11,
                   7,8,12),
                 ncol=3, byrow=T)
layout(matlay, widths=c(0.5,1,1))

### Set some margins
par(mar=c(2,1,1,1), oma=c(2,4,2,0))

### Range of the plots
yrvec <- 41:100

### Custom colors?
BandW <- FALSE
require(RColorBrewer)
ColPal <- colorRampPalette(brewer.pal(11, "Spectral")[-6])
if(BandW) ColPal <- colorRampPalette(brewer.pal(9, "Greys")[-1])

######################################
### Read weight-at-age data
TheData <- list()
for(it in 1:length(scenarios)){
  scenario <- scenarios[it]
  TheData[[it]] <- read.table(paste0(scenario, '/1/om/wtatage.ss_new'), header = F, skip = 2)
}

### Find the max by spp.
maxWaA <- numeric(length(scenarios))
for(it in 1:length(scenarios)){
  wtatage <- TheData[[it]]
  names(wtatage)[1:6] <- c('yr', 'seas', 'gender', 'growpattern', 'birthseas', 'fleet')
  ages <- 7:ncol(wtatage)
  
  names(wtatage)[7:ncol(wtatage)] <- paste0('a', ages- 7)
  wtatage <- wtatage[wtatage$fleet == 1, -(1:6)]
  maxWaA[it] <- max(unlist(wtatage))
}

######################################
### Now the plots

for(it in 1:length(scenarios)){
  
  ### Read in the data
  scenario <- scenarios[it]
  
  wtatage <- TheData[[it]]
  names(wtatage)[1:6] <- c('yr', 'seas', 'gender', 'growpattern', 'birthseas', 'fleet')
  ages <- 7:ncol(wtatage)
  
  names(wtatage)[7:ncol(wtatage)] <- paste0('a', ages- 7)
  wtatage <- wtatage[wtatage$fleet == 1, -(1:6)]
  meanvec <- t(apply(wtatage, 2, mean))  
  
  maxspWtaA <- max(maxWaA[1:(length(scenarios)/2)])
  if(it>length(scenarios)/2) maxspWtaA <- max(maxWaA[(length(scenarios)/2+1):length(scenarios)])
  
  ### Plot the time series
    if(it<5){
    x.lim <- c(-20,20)
    if(it==4) x.lim <- c(-0.02,0.02)
    plot_growth_case(scenario, y.lim = range(yrvec), x.lim=x.lim)
    box()
    axis(2)
    if(it==3) axis(1, at=c(-20,-10,0,10,20), labels=c(-30,-15,0,15,30))
    if(it==4) axis(1, at=c(-0.02,-0.01,0,0.01,0.02), labels=c(-75,-38,0,38,75))
    if(it==4) mtext(side=1, line=2.5, text="Deviation (%)")
    cases <- list(expression("Random noise in "*L[infinity]), 
                  expression("Time varying "*L[infinity]),
                  expression("Time varying "*L[infinity]),
                  expression("Time varying K"))
    xcase <- c(rep(-20,3), -0.02)
    text(x=xcase[it], y=103.5, labels=cases[[it]], pos=4, xpd=TRUE)
  }
  
  ### Plot the weights-at-age
  makeimage(agevec = ages - 7, yrvec = yrvec, mat = wtatage[-(1:yrvec[1] - 1), ], maxWt = maxspWtaA, 
            meanvec = meanvec, main = NULL, axisvals = FALSE, Colors = ColPal(50),
            addText = F, byYr = 2, byAge = 2, textRnd = 0)
    # Spp. name
  if(it==1|it==5) mtext(side=3, line=1, text=spp.names[it%%3])
    # Axes
  if(it==4|it==8){
    axis(1)
    mtext(side=1, line=2.5, text="Age")
  }
   
}
mtext(side=2, line=2.5, text="Year", outer=T)

dev.off()

