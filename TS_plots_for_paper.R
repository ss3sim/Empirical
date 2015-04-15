###################
### Get the data
setwd("C:/Users/Felipe/Documents/GitHub/Empirical")
source("load_results.r")

###################
### Extract the bits needed
mini.results <- data.frame(species=results.ts$species, D=results.ts$D, G=results.ts$G, replicate=results.ts$replicate, 
                           E=results.ts$E, X=results.ts$X, 
                           year=results.ts$year, converged=results.ts$converged, SpawnBio_re=results.ts$SpawnBio_re)

# head(mini.results)
# head(results.ts)
# dim(mini.results)

####################
### Set the figure folder
setwd("C:/Users/Felipe/Documents/GitHub/Empirical/figs")

####################
### Colors
require(RColorBrewer)
ColPal <- colorRampPalette(brewer.pal(9, "Greys")[-1])

####################
### Some controls

spp <- unique(mini.results$species)
DD <- unique(mini.results$D)
GG <- unique(mini.results$G)
reps <- unique(mini.results$replicate)
datas <- c("Unrealistic", "Rich", "Rich - Late survey")

####################
### The time series figures

for(sp in 1:2){
  if(sp==1) fname <- "Hake_TS.png"
  if(sp==2) fname <- "Yelloweye_TS.png"
  #windows(height=9, width=7)
  ### File where the figure is gonna be stored
  png(file = fname,
      height = 8, width = 7, units = 'in', res = 500)
  
  matlay <- matrix(c(1,2,3,
                     4,5,6,
                     0,0,0,
                     7,8,9,
                     10,11,12), ncol=3, byrow=T)
  layout(matlay, heights=c(1,1,0.2,1,1))
  par(mar=c(0,0,0,0), oma=c(4,4,2,2))
  counter <- 0
  for(assmt in 1:2){
    for(Git in 1:2){
      for(Dit in 1:length(DD)){
        counter <- counter+1
        plot(1, 1, xlim=c(0,100), ylim=c(-1,1), type='n', axes=F, xlab=NA, ylab=NA)
        box()
        abline(h=0)
        
        for(rit in 1:length(reps)){
          if(assmt==1){
            t.results <- mini.results[mini.results$species==spp[sp] &
                                        mini.results$G==GG[Git] &
                                        mini.results$D==DD[Dit] &
                                        mini.results$replicate==reps[rit] &
                                        mini.results$E=="E2" &  
                                        mini.results$converged=="yes", ]
          }
          if(assmt==2){
            t.results <- mini.results[mini.results$species==spp[sp] &
                                        mini.results$G==GG[Git] &
                                        mini.results$D==DD[Dit] &
                                        mini.results$replicate==reps[rit] &
                                        is.na(mini.results$E) &  
                                        mini.results$converged=="yes", ]
          }
          
          
          lines(t.results$year[1:100], t.results$SpawnBio_re[1:100], col=ColPal(56)[rit])
          
          text(x=0, y=1, labels=paste0(letters[counter],"."))
          if(counter==1|counter==2|counter==3)
            text(x=50, y=1, labels=datas[counter])
          
          if(counter==3|counter==9)
            mtext(side=4, line=0.5, text="Time-invariant")
          if(counter==6|counter==12)
            mtext(side=4, line=0.5, text="Time-varying")
          
          if(counter==1)
            mtext(side=3, text="von Bertalanffy growth", adj=0)
          
          if(counter==7)
            mtext(side=3, text="Empirical weight-at-age", adj=0)
          
          if(counter==1|counter==4|counter==7|counter==10)
            axis(2, at=c(-0.5,0,0.5))
          if(counter==4|counter==5|counter==6)
            axis(1, labels=F)
          if(counter==10|counter==11|counter==12)
            axis(1)        
        }
        cat("Sp: ", sp, "Counter: ", counter, "\n")
      }
    }
  }
  mtext(side=1, line=2.5, text="Year", outer=T)
  mtext(side=2, line=2.5, text="Relative error in SSB", outer=T)
  dev.off()
}
