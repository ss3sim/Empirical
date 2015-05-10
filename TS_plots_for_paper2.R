###################
### Get the data
# setwd("C:/Users/Felipe/Documents/GitHub/Empirical")
setwd("/Volumes/home/Empirical")
source("load_results.r")

library(RColorBrewer)
ColPal <- colorRampPalette(brewer.pal(9, "Greys")[-1])

###################
### Extract the bits needed
mini.results <- ssb.ts.long

#remove g7 and g8
mini.results <- subset(mini.results, G != "G7" & G != "G8")
mini.results$after.fishery <- ifelse(mini.results$year >= 25, 'yes', 'no')

# mini.results <- data.frame(species=results.ts$species, D=results.ts$D, G=results.ts$G, replicate=results.ts$replicate, 
#                            E=results.ts$E, X=results.ts$X, 
#                            year=results.ts$year, converged=results.ts$converged, SpawnBio_re=results.ts$SpawnBio_re)
mini.results <- mini.results %>% group_by(scenario, after.fishery) %>% 
  mutate(mare = 100 * round((median(abs(value), na.rm = TRUE)), digits = 2))
mini.results <- as.data.frame(mini.results)
mini.results$unq <- paste(mini.results$data.amount, mini.results$G,
 mini.results$data.desc)

plot.order <- c("unrealistic G0 A + L", "rich G0 A + L",  "rich - late survey G0 A + L",
                "unrealistic G1 A + L", "rich G1 A + L",  "rich - late survey G1 A + L",
                "unrealistic G0 WtAtAge", "rich G0 WtAtAge",  "rich - late survey G0 WtAtAge",
                "unrealistic G1 WtAtAge", "rich G1 WtAtAge",  "rich - late survey G1 WtAtAge")

#Order everything
mini.results <- mini.results[order(mini.results$ID, mini.results$year), ]
mini.results <- mini.results[is.na(mini.results$converged) == FALSE, ]

#
d.amount <- c("Unrealistic", "Rich", "Rich - Late Survey")

plot_ssb_with_mare <- function(data){
  matlay <- matrix(c(1,2,3,
                     4,5,6,
                     0,0,0,
                     7,8,9,
                     10,11,12), ncol=3, byrow=T)
  layout(matlay, heights=c(1,1,0.2,1,1))
  par(mar=c(0,0,0,0), oma=c(4,4,6,3), mgp = c(.25, .6, 0))

  for(ii in 1:length(plot.order)){
    plot(1, 1, xlim=c(-7,100), ylim=c(-1.5,1.5), type='n', axes=F, xlab=NA, ylab=NA)
    box()
    abline(h=0)
    temp <- subset(data, unq == plot.order[ii])
    temp.mare <-subset(temp, after.fishery == 'yes')
    unq.reps <- unique(temp$replicate)
    for(jj in 1:length(unq.reps)){
      
      tr <- subset(temp, replicate == unq.reps[jj])
      # lines(tr$year, tr$value, col = ColPal(length(unq.reps))[jj])
      lines(tr$year, tr$value, col = "#00000040")
    }
    
    #add mare value
    text(x = -5, y=1.4, labels=paste0(letters[ii],"."), cex = 1.2)
    text(paste0("MARE=", unique(temp.mare$mare)), x= 85, y = 1.4, cex = 1)
    
    if(ii == 1) mtext(side = 3, text = 'Age + Length', line = 2.5, cex = 1.2, adj = 0)
    if(ii == 7) mtext(side = 3, text = 'Weight-at-Age', line = .25, cex = 1.2, adj = 0)
    if(ii %in% c(1, 2, 3)) mtext(side = 3, line = .5, 
      text = paste("Data", d.amount[ii]), adj = 0)
    if(ii %in% c(3, 9)) mtext(side = 4, text = "Time-Invariant", cex = 1, line = .5)
    if(ii %in% c(6, 12)) mtext(side = 4, text = "Time-Varying Growth", cex = 1, line = .5)
    if(ii %in% c(1, 4, 7, 10)) axis(side = 2, at = c(-1, -.5, 0, .5, 1), las = 2)
    # if(ii %in% c(4, 5, 6)) axis(side = 1, labels = FALSE)
    if(ii %in% c(10, 11, 12)) axis(side = 1, cex.axis = 1, at = seq(0, 100, 20))
  }
  mtext(side = 1, text = "Year", outer = TRUE, cex = 1.2, line = 2.5)
  mtext(side = 2, text = "Relative Error SSB", outer = TRUE, cex = 1.2, line = 2.5)
}

# plot_ssb_with_mare(ssb.hake)

#save plots
ssb.hake <- subset(mini.results, species == 'hake-age')

tiff(height = 8, width = 7, units = 'in', res = 200, 
  file = "/Volumes/home/Empirical/figs/hake_ssb_re.tiff")
plot_ssb_with_mare(ssb.hake)
dev.off()


pdf(height = 8, width = 7, 
  file = "/Volumes/home/Empirical/figs/FIG2.pdf")
plot_ssb_with_mare(ssb.hake)
dev.off()

jpeg(height = 8, width = 7, units = 'in', res = 300, 
  file = "/Volumes/home/Empirical/figs/hake_ssb_re.jpg")
plot_ssb_with_mare(ssb.hake)
dev.off()

ssb.yellow <- subset(mini.results, species == 'yellow-age')
png(height = 8, width = 7, units = 'in', res = 200, 
  file = "/Volumes/home/Empirical/figs/yellow_ssb_re.png")
plot_ssb_with_mare(ssb.yellow)
dev.off()

pdf(height = 8, width = 7,
  file = "/Volumes/home/Empirical/figs/FIG3.pdf")
plot_ssb_with_mare(ssb.yellow)
dev.off()


jpeg(height = 8, width = 7, units = 'in', res = 300, 
  file = "/Volumes/home/Empirical/figs/yellow_ssb_re.jpg")
plot_ssb_with_mare(ssb.yellow)
dev.off()

# head(mini.results)
# head(results.ts)
# dim(mini.results)

####################
### Set the figure folder
# setwd("C:/Users/Felipe/Documents/GitHub/Empirical/figs")
# setwd("/Volumes/home/Empirical/figs")

# ####################
# ### Colors
# require(RColorBrewer)
# ColPal <- colorRampPalette(brewer.pal(9, "Greys")[-1])

# ####################
# ### Some controls
# spp <- unique(mini.results$species)
# DD <- unique(mini.results$D)
# GG <- unique(mini.results$G)
# reps <- unique(mini.results$replicate)
# datas <- c("Unrealistic", "Rich", "Rich - Late survey")

# ####################
# ### The time series figures

# for(sp in 1:2){
#   if(sp==1) fname <- "Hake_TS.png"
#   if(sp==2) fname <- "Yelloweye_TS.png"
  
#   #windows(height=9, width=7)
#   ### File where the figure is gonna be stored
#   png(file = fname,
#       height = 8, width = 7, units = 'in', res = 500)
  
#   matlay <- matrix(c(1,2,3,
#                      4,5,6,
#                      0,0,0,
#                      7,8,9,
#                      10,11,12), ncol=3, byrow=T)
#   layout(matlay, heights=c(1,1,0.2,1,1))
#   par(mar=c(0,0,0,0), oma=c(4,4,2,2))
#   counter <- 0
#   for(assmt in 1:2){
#     for(Git in 1:2){
#       for(Dit in 1:length(DD)){
#         counter <- counter+1
#         plot(1, 1, xlim=c(0,100), ylim=c(-1,1), type='n', axes=F, xlab=NA, ylab=NA)
#         box()
#         abline(h=0)
        
#         for(rit in 1:length(reps)){
#           if(assmt==1){
#             t.results <- mini.results[mini.results$species==spp[sp] &
#                                         mini.results$G==GG[Git] &
#                                         mini.results$D==DD[Dit] &
#                                         mini.results$replicate==reps[rit] &
#                                         mini.results$E=="E2" &  
#                                         mini.results$converged=="yes", ]
#           }
#           if(assmt==2){
#             t.results <- mini.results[mini.results$species==spp[sp] &
#                                         mini.results$G==GG[Git] &
#                                         mini.results$D==DD[Dit] &
#                                         mini.results$replicate==reps[rit] &
#                                         is.na(mini.results$E) &  
#                                         mini.results$converged=="yes", ]
#           }
          
          
#           lines(t.results$year[1:100], t.results$SpawnBio_re[1:100], col=ColPal(56)[rit])
          
#           text(x=0, y=1, labels=paste0(letters[counter],"."))
#           if(counter==1|counter==2|counter==3)
#             text(x=50, y=1, labels=datas[counter])
          
#           if(counter==3|counter==9)
#             mtext(side=4, line=0.5, text="Time-invariant")
#           if(counter==6|counter==12)
#             mtext(side=4, line=0.5, text="Time-varying")
          
#           if(counter==1)
#             # mtext(side=3, text="von Bertalanffy growth", adj=0)
#             mtext(side=3, text="Age and Length", adj=0)
          
#           if(counter==7)
#             mtext(side=3, text="Empirical weight-at-age", adj=0)
          
#           if(counter==1|counter==4|counter==7|counter==10)
#             axis(2, at=c(-0.5,0,0.5))
#           if(counter==4|counter==5|counter==6)
#             axis(1, labels=F)
#           if(counter==10|counter==11|counter==12)
#             axis(1)        
#         }
#         cat("Sp: ", sp, "Counter: ", counter, "\n")
#       }
#     }
#   }
#   mtext(side=1, line=2.5, text="Year", outer=T)
#   mtext(side=2, line=2.5, text="Relative error in SSB", outer=T)
#   dev.off()
# }
