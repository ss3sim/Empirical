###################
### Get the data
# setwd("C:/Users/Felipe/Documents/GitHub/Empirical")
# setwd("/Volumes/home/Empirical")
# source("load_results.r")

library(RColorBrewer)
ColPal <- colorRampPalette(brewer.pal(9, "Greys")[-1])

###################
### Extract the bits needed
mini.results <- ssb.ts.long

#Check replicates included in each scenario
# as.data.frame(subset(mini.results, converged == 'yes') %>% group_by(scenario) %>% 
#   summarise(Nconverged = length(unique(replicate))))

#remove g7 and g8
# mini.results <- subset(mini.results, G != "G7" & G != "G8")
mini.results$after.fishery <- ifelse(mini.results$year > 25, 'yes', 'no')

# mini.results <- data.frame(species=results.ts$species, D=results.ts$D, G=results.ts$G, replicate=results.ts$replicate, 
#                            E=results.ts$E, X=results.ts$X, 
#                            year=results.ts$year, converged=results.ts$converged, SpawnBio_re=results.ts$SpawnBio_re)
mini.results <- mini.results %>% group_by(scenario, after.fishery) %>% 
  mutate(mare = 100 * round((median(abs(value), na.rm = TRUE)), digits = 2))
mini.results <- as.data.frame(mini.results)
mini.results$unq <- paste(mini.results$data.amount, mini.results$G,
 mini.results$data.desc)

#Order Everything
mini.results <- mini.results[order(mini.results$ID, mini.results$year), ]
# mini.results <- mini.results[is.na(mini.results$converged) == FALSE, ]

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Plot only three data cases 
plot_ssb_with_mare <- function(data, cex = 1){
  d.amount <- c("Rich", "Rich - Late Survey", "Moderate")
  plot.order <- c( "rich G0 A + L",  "rich - late survey G0 A + L", "moderate G0 A + L",
                "rich G1 A + L",  "rich - late survey G1 A + L", "moderate G1 A + L",
                "rich G0 WtAtAge",  "rich - late survey G0 WtAtAge", "moderate G0 WtAtAge",
                "rich G1 WtAtAge",  "rich - late survey G1 WtAtAge", "moderate G1 WtAtAge")

  matlay <- matrix(c(1,2,3,
                     4,5,6,
                     0,0,0,
                     7,8,9,
                     10,11,12), ncol = 3, byrow = TRUE)

  layout(matlay, heights=c(1,1,0.2,1,1))
  par(mar=c(0,0,0,0), oma=c(4,4,6,3), mgp = c(.25, .6, 0))

  for(ii in 1:length(plot.order)){
    # plot(1, 1, xlim=c(-7,100), ylim=c(-1.5,1.5), type='n', axes=F, xlab=NA, ylab=NA)
    plot(1, 1, xlim=c(-.5,100), ylim=c(-1.5,1.5), type='n', axes=F, xlab=NA, ylab=NA)
    box()
    abline(h=0)
    temp <- subset(data, unq == plot.order[ii])
    temp.mare <-subset(temp, after.fishery == 'yes')
    unq.reps <- unique(temp$replicate)
    # print(length(unq.reps))
    for(jj in 1:length(unq.reps)){
      tr <- subset(temp, replicate == unq.reps[jj])
      tr <- tr[order(tr$year),]
      if(max(tr$value) > 1 & ii != 12) next
      # lines(tr$year, tr$value, col = ColPal(length(unq.reps))[jj])
      lines(tr$year, tr$value, col = "#00000040")
    }
    
    #add mare value
    # text(x = -5, y=1.4, labels=paste0(letters[ii],"."), cex = 1.2 * cex)
    text(x = .5, y=1.4, labels=paste0(letters[ii],"."), cex = 1.2 * cex)
    text(paste0("MARE=", unique(temp.mare$mare)), x= 85, y = 1.4, cex = 1 * cex)
    
    if(ii == 1) mtext(side = 3, text = 'Age + Length', line = 2.2, cex = 1.2 * cex, adj = 0)
    if(ii == 7) mtext(side = 3, text = 'Weight-at-Age', line = .25, cex = 1.2 * cex, adj = 0)
    if(ii %in% c(1, 2, 3)) mtext(side = 3, line = .5, 
      text = paste("Data", d.amount[ii]), adj = 0, cex = 1 * cex)
    if(ii %in% c(3, 9)) mtext(side = 4, text = "Time-Invariant", cex = 1 * cex, line = .5)
    if(ii %in% c(6, 12)) mtext(side = 4, text = "Time-Varying Growth", cex = 1 * cex, line = .5)
    if(ii %in% c(1, 4, 7, 10)) axis(side = 2, at = c(-1, -.5, 0, .5, 1), las = 2, cex.axis = 1 * cex)
    # if(ii %in% c(4, 5, 6)) axis(side = 1, labels = FALSE)
    if(ii %in% c(10, 11, 12)) axis(side = 1, cex.axis = 1 * cex, at = seq(0, 100, 20))
  }
  mtext(side = 1, text = "Year", outer = TRUE, cex = 1.2 * cex, line = 2.5)
  mtext(side = 2, text = "Relative Error SSB", outer = TRUE, cex = 1.2 * cex, line = 2.5)
}


#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Subset Species
ssb.hake <- subset(mini.results, species == 'hake-age')
ssb.yellow <- subset(mini.results, species == 'yellow-age')


#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Compare F patterns 
# fpatts <- ssb.hake[ssb.hake$scenario %in% c("D3-E2-F0-G1-hake-age", "D3-E2-F1-G1-hake-age" ), ]
# fpatts <- subset(fpatts, replicate != 1)
# # which(fpatts$value  == max(fpatts$value))

# # subset(fpatts, value < )
# ggplot(fpatts, aes(x = year, y = value, group = replicate)) + geom_line() + facet_wrap(~scenario)

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Compare time-varying block estimation to three methods, only data rich
#Hake
hake <- mini.results[grep('hake', mini.results$scenario), ]

rich.hake <- hake[grep("D3", hake$scenario), ]
rich.hake <- rich.hake[grep("G1", rich.hake$scenario), ]
rich.hake <- rich.hake[grep("F1", rich.hake$scenario), ]

descs <- (data.frame(scenario = as.character(unique(rich.hake$scenario)), 
  desc = as.character(c("A + L", "Block A + L", "Weight-at-Age"))))
descs[, 1] <- as.character(descs[, 1])
descs[, 2] <- as.character(descs[, 2])

rich.hake <- merge(rich.hake, descs, all = T)

#Yelloweye
yellow <- mini.results[grep('yellow', mini.results$scenario), ]

rich.yellow <- yellow[grep("D3", yellow$scenario), ]
rich.yellow <- rich.yellow[grep("G1", rich.yellow$scenario), ]
rich.yellow <- rich.yellow[grep("F1", rich.yellow$scenario), ]


descs <- (data.frame(scenario = as.character(unique(rich.yellow$scenario)), 
  desc = as.character(c("A + L", "Block A + L", "Weight-at-Age"))))
descs[, 1] <- as.character(descs[, 1])
descs[, 2] <- as.character(descs[, 2])

rich.yellow <- merge(rich.yellow, descs, all = T)

#------------------------------------------------
#Function
# dev.new(width = 5.5, height = 3, units = 'in')
# data <- rich.hake
# cex <- .8
plot_ssb_with_block <- function(data, cex = .8){
  par(mfcol = c(1, 3), mar=c(0,0,0,0), oma=c(3.5,3.5,1,1), mgp = c(.25, .6, 0))
  plot.order <- c("A + L", "Block A + L", "Weight-at-Age")

  for(ii in 1:length(plot.order)){
    # plot(1, 1, xlim=c(-7,100), ylim=c(-1.5,1.5), type='n', axes=F, xlab=NA, ylab=NA)
    plot(1, 1, xlim=c(-.5,100), ylim=c(-1,1.5), type='n', axes=F, xlab=NA, ylab=NA)
    box()
    abline(h=0)
    temp <- subset(data, desc == plot.order[ii])
    temp.mare <-subset(temp, after.fishery == 'yes')
    unq.reps <- unique(temp$replicate)
    # print(length(unq.reps))
    for(jj in 1:length(unq.reps)){
      tr <- subset(temp, replicate == unq.reps[jj])
      tr <- tr[order(tr$year),]
      if(max(tr$value) > 1 & ii != 12) next
      # lines(tr$year, tr$value, col = ColPal(length(unq.reps))[jj])
      lines(tr$year, tr$value, col = "#00000040")
    }
    
    #add mare value
    # text(x = -5, y=1.4, labels=paste0(letters[ii],"."), cex = 1.2 * cex)
    # text(x = .5, y=1.4, labels=paste0(letters[ii],"."), cex = 1.2 * cex)
    mtext(side = 3, plot.order[ii], adj = .1, outer = F, line = -2, cex = 1 * cex)
    # text(x = .5, y=1.4, labels=plot.order[ii], cex = 1.2 * cex)
    mtext(side = 3, paste0("MARE = ", unique(temp.mare$mare)), cex = .8 * cex, adj = .95,
      line = -2)
    # text(paste0("MARE=", unique(temp.mare$mare)), x= 85, y = 1.4, cex = 1 * cex)
    if(ii == 1) axis(side = 2, at = c(-1, -.5, 0, .5, 1), las = 2, cex.axis = 1 * cex)
    axis(side = 1, cex.axis = 1 * cex, at = seq(0, 100, 20))
  }    
  mtext(side = 1, text = "Year", outer = TRUE, cex = 1.2 * cex, line = 2)
  mtext(side = 2, text = "Relative Error SSB", outer = TRUE, cex = 1.2 * cex, line = 2)
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Final Plots

#------------------------------------------------
#Plots of Time-Varying stuff
plot_ssb_with_block(data = rich.hake, cex = .8)

png(height = 80, width = 140, units = 'mm', res = 150,
  file = "/Volumes/home/Empirical/figs/FIG6_tv_hake_ssb.png")
plot_ssb_with_block(data = rich.hake, cex = .8)
dev.off()

png(height = 80, width = 140, units = 'mm', res = 150,
  file = "/Volumes/home/Empirical/figs/FIG7_tv_yellow_ssb.png")
plot_ssb_with_block(data = rich.yellow, cex = .8)
dev.off()


tiff(height = 80, width = 140, units = 'mm', res = 300,
  file = "/Volumes/home/Empirical/figs/FIG6_tv_hake_ssb.tiff")
plot_ssb_with_block(data = rich.hake, cex = .8)
dev.off()

tiff(height = 80, width = 140, units = 'mm', res = 300,
  file = "/Volumes/home/Empirical/figs/FIG7_tv_yellow_ssb.tiff")
plot_ssb_with_block(data = rich.yellow, cex = .8)
dev.off()

#------------------------------------------------
#Plots of all scenarios
#two column - 190mm
#1.5 column - 140mm
#1 column - 90mm

#Hake
tiff(height = 160, width = 140, units = 'mm', res = 300,
  file = "/Volumes/home/Empirical/figs/FIG2_hake_ssb.tiff")
plot_ssb_with_mare(ssb.hake, cex = .8)
dev.off()

png(height = 160, width = 140, units = 'mm', res = 200,
  file = "/Volumes/home/Empirical/figs/FIG2_hake_ssb.png")
plot_ssb_with_mare(ssb.hake, cex = .8)
dev.off()

#Yelloweye
tiff(height = 160, width = 140, units = 'mm', res = 300,
  file = "/Volumes/home/Empirical/figs/FIG3_yellow_ssb.tiff")
plot_ssb_with_mare(ssb.yellow, cex = .8)
dev.off()

png(height = 160, width = 140, units = 'mm', res = 300,
  file = "/Volumes/home/Empirical/figs/FIG3_yellow_ssb.png")
plot_ssb_with_mare(ssb.yellow, cex = .8)
dev.off()















# #------------------------------------------------------------------------------------------------------------------------------------------------------------
# #Plot all four scenarios
# #Order everything

# # data <- subset(mini.results, species == 'hake-age')

# plot_ssb_with_mare_all <- function(data){
#   d.amount <- c("Unrealistic", "Rich", "Rich - Late Survey", "Moderate")
#   plot.order <- c("unrealistic G0 A + L", "rich G0 A + L",  "rich - late survey G0 A + L", "moderate G0 A + L",
#                 "unrealistic G1 A + L", "rich G1 A + L",  "rich - late survey G1 A + L", "moderate G1 A + L",
#                 "unrealistic G0 WtAtAge", "rich G0 WtAtAge",  "rich - late survey G0 WtAtAge", "moderate G0 WtAtAge",
#                 "unrealistic G1 WtAtAge", "rich G1 WtAtAge",  "rich - late survey G1 WtAtAge", "moderate G1 WtAtAge")

#   matlay <- matrix(c(1,2,3,4,
#                      5,6,7,8,
#                      0,0,0,0,
#                      9,10,11,12,
#                      13,14,15,16), ncol = 4, byrow = TRUE)

#   layout(matlay, heights=c(1,1,0.2,1,1))
#   par(mar=c(0,0,0,0), oma=c(4,4,6,3), mgp = c(.25, .6, 0))

#   for(ii in 1:length(plot.order)){
#     plot(1, 1, xlim=c(-7,100), ylim=c(-1.5,1.5), type='n', axes=F, xlab=NA, ylab=NA)
#     box()
#     abline(h=0)
#     temp <- subset(data, unq == plot.order[ii])
#     temp.mare <-subset(temp, after.fishery == 'yes')
#     unq.reps <- unique(temp$replicate)
#     for(jj in 1:length(unq.reps)){
      
#       tr <- subset(temp, replicate == unq.reps[jj])
#       # lines(tr$year, tr$value, col = ColPal(length(unq.reps))[jj])
#       lines(tr$year, tr$value, col = "#00000040")
#     }
    
#     #add mare value
#     text(x = -5, y=1.4, labels=paste0(letters[ii],"."), cex = 1.2)
#     text(paste0("MARE=", unique(temp.mare$mare)), x= 85, y = 1.4, cex = 1)
    
#     if(ii == 1) mtext(side = 3, text = 'Age + Length', line = 2.5, cex = 1.2, adj = 0)
#     if(ii == 9) mtext(side = 3, text = 'Weight-at-Age', line = .25, cex = 1.2, adj = 0)
#     if(ii %in% c(1, 2, 3, 4)) mtext(side = 3, line = .5, 
#       text = paste("Data", d.amount[ii]), adj = 0)
#     if(ii %in% c(4, 12)) mtext(side = 4, text = "Time-Invariant", cex = 1, line = .5)
#     if(ii %in% c(8, 16)) mtext(side = 4, text = "Time-Varying Growth", cex = 1, line = .5)
#     if(ii %in% c(1, 5, 9, 13)) axis(side = 2, at = c(-1, -.5, 0, .5, 1), las = 2)
#     # if(ii %in% c(4, 5, 6)) axis(side = 1, labels = FALSE)
#     if(ii %in% c(13, 14, 15, 16)) axis(side = 1, cex.axis = 1, at = seq(0, 100, 20))
#   }
#   mtext(side = 1, text = "Year", outer = TRUE, cex = 1.2, line = 2.5)
#   mtext(side = 2, text = "Relative Error SSB", outer = TRUE, cex = 1.2, line = 2.5)
# }
















#Hake
# ssb.hake <- subset(mini.results, species == 'hake-age')
# tiff(height = 160, width = 140, units = 'mm', res = 300,
#   file = "/Volumes/home/Empirical/figs/hake_ssb_re_hires.tiff")
# plot_ssb_with_mare(ssb.hake, cex = .8)
# dev.off()

# #Low Res Plots
# tiff(height = 160, width = 140, units = 'mm', res = 100,
#   file = "/Volumes/home/Empirical/figs/hake_ssb_re_lores.tiff")
# plot_ssb_with_mare(ssb.hake, cex = .8)
# dev.off()

# #png
# png(height = 160, width = 140, units = 'mm', res = 150,
#   file = "/Volumes/home/Empirical/figs/hake_ssb_re_lores.png")
# plot_ssb_with_mare(ssb.hake, cex = .8)
# dev.off()

# png(height = 217, width = 190, units = 'mm', res = 100,
#   file = "/Volumes/home/Empirical/figs/hake_ssb_re_loresBig.png")
# plot_ssb_with_mare(ssb.hake, cex = .8)
# dev.off()

# #Yelloweye
# ssb.yellow <- subset(mini.results, species == 'yellow-age')
# tiff(height = 160, width = 140, units = 'mm', res = 300,
#   file = "/Volumes/home/Empirical/figs/yellow_ssb_re_hires.tiff")
# plot_ssb_with_mare(ssb.yellow, cex = .8)
# dev.off()

# #Low Res Plots
# tiff(height = 160, width = 140, units = 'mm', res = 100,
#   file = "/Volumes/home/Empirical/figs/yellow_ssb_re_lores.tiff")
# plot_ssb_with_mare(ssb.yellow, cex = .8)
# dev.off()

# #png
# png(height = 160, width = 140, units = 'mm', res = 150,
#   file = "/Volumes/home/Empirical/figs/yellow_ssb_re_lores.png")
# plot_ssb_with_mare(ssb.yellow, cex = .8)
# dev.off()

# png(height = 217, width = 190, units = 'mm', res = 100,
#   file = "/Volumes/home/Empirical/figs/yellow_ssb_re_loresBig.png")
# plot_ssb_with_mare(ssb.yellow, cex = .8)
# dev.off()













#Crap



# ssb.yellow <- subset(mini.results, species == 'yellow-age')
# png(height = 8, width = 7, units = 'in', res = 200, 
#   file = "/Volumes/home/Empirical/figs/yellow_ssb_re.png")
# plot_ssb_with_mare(ssb.yellow)
# dev.off()

# pdf(height = 8, width = 7,
#   file = "/Volumes/home/Empirical/figs/FIG3.pdf")
# plot_ssb_with_mare(ssb.yellow)
# dev.off()


# jpeg(height = 8, width = 7, units = 'in', res = 300, 
#   file = "/Volumes/home/Empirical/figs/yellow_ssb_re.jpg")
# plot_ssb_with_mare(ssb.yellow)
# dev.off()

# #save plots
# ssb.hake <- subset(mini.results, species == 'hake-age')

# tiff(height = 8, width = 7, units = 'in', res = 300, 
#   file = "/Volumes/home/Empirical/figs/hake_ssb_re.tiff")
# plot_ssb_with_mare(ssb.hake)
# dev.off()

# pdf(height = 8, width = 7, 
#   file = "/Volumes/home/Empirical/figs/FIG2.pdf")
# plot_ssb_with_mare(ssb.hake)
# dev.off()

# jpeg(height = 8, width = 7, units = 'in', res = 300, 
#   file = "/Volumes/home/Empirical/figs/hake_ssb_re.jpg")
# plot_ssb_with_mare(ssb.hake)
# dev.off()

# ssb.yellow <- subset(mini.results, species == 'yellow-age')
# png(height = 8, width = 7, units = 'in', res = 200, 
#   file = "/Volumes/home/Empirical/figs/yellow_ssb_re.png")
# plot_ssb_with_mare(ssb.yellow)
# dev.off()

# pdf(height = 8, width = 7,
#   file = "/Volumes/home/Empirical/figs/FIG3.pdf")
# plot_ssb_with_mare(ssb.yellow)
# dev.off()


# jpeg(height = 8, width = 7, units = 'in', res = 300, 
#   file = "/Volumes/home/Empirical/figs/yellow_ssb_re.jpg")
# plot_ssb_with_mare(ssb.yellow)
# dev.off()

# head(mini.results)
# head(results.ts)
# dim(mini.results)
