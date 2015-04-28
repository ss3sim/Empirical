#####################################
### Figure XXX for Peter's paper

#####################################
### Some controls
library(RColorBrewer)

color.bar <- function(lut, min, max=-min, nticks=8, ticks=seq(min, max, len=nticks), title='') {
    scale = (length(lut)-1)/(max-min)
 
    # dev.new(width=1.75, height=5)
    plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
    axis(2, ticks, las=1)
    for (i in 1:(length(lut)-1)) {
      y = (i-1)/scale + min
      rect(0,y,10,y+1/scale, col=lut[i], border=NA)
    } 
}

color.bar(lut = ColPal(50), min = 0, max = 8, nticks = 9)

### Directory with the results
# results.dir <- "C:/Users/Felipe/Desktop/Growth trials/growth_figs"
results.dir <- "/Users/peterkuriyama/Desktop/test_runs"
setwd(results.dir)

### Source the plotting functoions
# source("C:/Users/Felipe/Documents/GitHub/Empirical/Plots_for_paper.R")

source("/Volumes/home/Empirical/Plots_for_paper.R")
### Scenarios
species.vec <- c('yellow-age', 'hake-age')
scenarios <- c(paste("D4-F0-G",c(3,1,2,4),"-X4-",species.vec[1], sep=""),
               paste("D4-F0-G",c(3,1,2,4),"-X4-",species.vec[2], sep=""))
spp.names <- c("Yelloweye", "Hake")

### File where the figure is gonna be stored
# png(file = "Figure_XXX.png",
#     height = 12, width = 8, units = 'in', res = 500)


plot_growth_cases <- function(BandW = FALSE){
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
  # BandW <- TRUE
  
  ColPal <- colorRampPalette(brewer.pal(11, "Spectral")[-6])
  if(BandW) ColPal <- colorRampPalette(brewer.pal(9, "Greys")[-1])

  ######################################
  ### Read weight-at-age data
  TheData <- list()
  for(it in 1:length(scenarios)){
    scenario <- scenarios[it]
    # TheData[[it]] <- read.table(paste0(scenario, '/1/om/wtatage.ss_new'), header = F, skip = 2)
    TheData[[it]] <- read.table(paste0(scenario, '/100/om/wtatage.ss_new'), header = F, skip = 2)
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
    # print(maxspWtaA)
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
    makeimage(agevec = ages - 7, yrvec = yrvec, mat = wtatage[-(1:yrvec[1] - 1), ], 
      maxWt = maxspWtaA, meanvec = meanvec, main = NULL, axisvals = FALSE, Colors = ColPal(50),
              addText = F, byYr = 2, byAge = 2, textRnd = 0)
      # Spp. name
    if(it==1|it==5) mtext(side=3, line=1, text=spp.names[it%%3])
      # Axes
    if(it==4|it==8){
      axis(1)
      mtext(side=1, line=2.5, text="Age")
    }
  }
}

#Save plots
png(file = "/Volumes/home/Empirical/figs/growth_casesBW.png",
    height = 12, width = 8, units = 'in', res = 200)
plot_growth_cases(BandW = TRUE)
dev.off()


png(file = "/Volumes/home/Empirical/figs/growth_cases.png",
    height = 12, width = 8, units = 'in', res = 200)
plot_growth_cases(BandW = FALSE)
dev.off()















# ### Set the plotting matrix
# matlay <- matrix(c(1,2,9,
#                    3,4,10,
#                    5,6,11,
#                    7,8,12),
#                  ncol=3, byrow=T)
# layout(matlay, widths=c(0.5,1,1))

# ### Set some margins
# par(mar=c(2,1,1,1), oma=c(2,4,2,0))

# ### Range of the plots
# yrvec <- 41:100

# ### Custom colors?
# BandW <- TRUE
# require(RColorBrewer)
# ColPal <- colorRampPalette(brewer.pal(11, "Spectral")[-6])
# if(BandW) ColPal <- colorRampPalette(brewer.pal(9, "Greys")[-1])

# ######################################
# ### Read weight-at-age data
# TheData <- list()
# for(it in 1:length(scenarios)){
#   scenario <- scenarios[it]
#   # TheData[[it]] <- read.table(paste0(scenario, '/1/om/wtatage.ss_new'), header = F, skip = 2)
#   TheData[[it]] <- read.table(paste0(scenario, '/100/om/wtatage.ss_new'), header = F, skip = 2)
# }

# ### Find the max by spp.
# maxWaA <- numeric(length(scenarios))
# for(it in 1:length(scenarios)){
#   wtatage <- TheData[[it]]
#   names(wtatage)[1:6] <- c('yr', 'seas', 'gender', 'growpattern', 'birthseas', 'fleet')
#   ages <- 7:ncol(wtatage)
  
#   names(wtatage)[7:ncol(wtatage)] <- paste0('a', ages- 7)
#   wtatage <- wtatage[wtatage$fleet == 1, -(1:6)]
#   maxWaA[it] <- max(unlist(wtatage))
# }

# ######################################
# ### Now the plots
# for(it in 1:length(scenarios)){
  
#   ### Read in the data
#   scenario <- scenarios[it]
  
#   wtatage <- TheData[[it]]
#   names(wtatage)[1:6] <- c('yr', 'seas', 'gender', 'growpattern', 'birthseas', 'fleet')
#   ages <- 7:ncol(wtatage)
  
#   names(wtatage)[7:ncol(wtatage)] <- paste0('a', ages- 7)
#   wtatage <- wtatage[wtatage$fleet == 1, -(1:6)]
#   meanvec <- t(apply(wtatage, 2, mean))  
  
#   maxspWtaA <- max(maxWaA[1:(length(scenarios)/2)])
#   print(maxspWtaA)
#   if(it>length(scenarios)/2) maxspWtaA <- max(maxWaA[(length(scenarios)/2+1):length(scenarios)])
  
#   ### Plot the time series
#     if(it<5){
#     x.lim <- c(-20,20)
#     if(it==4) x.lim <- c(-0.02,0.02)
#     plot_growth_case(scenario, y.lim = range(yrvec), x.lim=x.lim)
#     box()
#     axis(2)
#     if(it==3) axis(1, at=c(-20,-10,0,10,20), labels=c(-30,-15,0,15,30))
#     if(it==4) axis(1, at=c(-0.02,-0.01,0,0.01,0.02), labels=c(-75,-38,0,38,75))
#     if(it==4) mtext(side=1, line=2.5, text="Deviation (%)")
#     cases <- list(expression("Random noise in "*L[infinity]), 
#                   expression("Time varying "*L[infinity]),
#                   expression("Time varying "*L[infinity]),
#                   expression("Time varying K"))
#     xcase <- c(rep(-20,3), -0.02)
#     text(x=xcase[it], y=103.5, labels=cases[[it]], pos=4, xpd=TRUE)
#   }
  
#   ### Plot the weights-at-age
#   makeimage(agevec = ages - 7, yrvec = yrvec, mat = wtatage[-(1:yrvec[1] - 1), ], 
#     maxWt = maxspWtaA, meanvec = meanvec, main = NULL, axisvals = FALSE, Colors = ColPal(50),
#             addText = F, byYr = 2, byAge = 2, textRnd = 0)
#     # Spp. name
#   if(it==1|it==5) mtext(side=3, line=1, text=spp.names[it%%3])
#     # Axes
#   if(it==4|it==8){
#     axis(1)
#     mtext(side=1, line=2.5, text="Age")
#   }
# }







#   if(it == 8){
#       # # xlim <- c(0, 20)
#       # xrange <- c(18, 19)
#       # # ylim <- c(0, 4)

#       lut <- ColPal(50)
#       min <- 50
#       seq(0, maxspWtaA, length.out = 50)
#       # max <- 8
#       # nticks <- 9
#       # ticks <- seq(min, max, len=nticks)
#       # title <- 'poop'

#       # color.bar <- function(lut, min, max=-min, nticks=8, ticks=seq(min, max, len=nticks), title='',...) {
#       # scale = (length(lut)-1)/(max-min)
#       scale <- (length(lut) - 1) / (40)

#       # par(new = TRUE)
#       rect(19, 50, 20, 90, col = 'white', border = NA)
#       for (i in 1:(length(lut)-1)) {
#         y = (i - 1)/scale + min
#         # print(y)
#         rect(19.5, y, 20, y+1/scale, col=lut[i], border=NA)
#       } 


#       segments(17, 50, 18, 50, col = 'black')
#       segments(17.5, 60, 18, 60, col = 'black')
#       segments(17.5, 70, 18, 70, col = 'black'); text(17.3, 70, "4")



#   }
# }


# mtext(side=2, line=2.5, text="Year", outer=T)


# dev.off()

