#-------------------------------------------------------------------------------------
#Functions

print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {
    tmp <- par("usr")
    text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
    text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
    text(x=text.x, y=text.y, labels=label, ...)
}

#--------------------------------
#For Hake
plot_selex_hake <- function(d, yy = .1, cex = 1){
  # print(unique(d$species))
  par(mfrow=c(2, 3), mar=c(0,0,0,0), oma=c(6, 5,2,3), mgp = c(.25, .6, 0))
  for(ii in 1:6){
    temp <- subset(d, d$unq == plot.order[ii])
    temp <- temp[duplicated(temp$median_) == FALSE, ]
    temp1 <- subset(d, d$unq == plot.order[ii + 6])
    temp1$xvalue <- temp1$xvalue + 0.2
    temp1 <- temp1[duplicated(temp1$median_) == FALSE, ]

# cat(ii, temp$mare, temp1$mare, '\n')
    # cbind(temp, temp1)
    # plot(0, 0, type = 'n', xlim = c(.6, 4.5), ylim = c(-yy - .01, yy + .07),
    plot(0, 0, type = 'n', xlim = c(.4, 4.5), ylim = c(-yy - .01, yy + .07),
      axes = FALSE, ann = FALSE)
    abline(h = 0, col = 'gray50')
    #plot temp
    with(temp, {
      points(x = xvalue, y = median_, pch = 16, cex = 1.2 * cex, col = 'black');
      segments(x0 = xvalue, y0 = l2, y1 = u2, col = 'black', lwd = .7);
      text(x = xvalue, y = yy + .05, label = mare, col = 'black', cex = 1.2 * cex)
      # text(x = xvalue[which(mare != 0)], y = .12, 
      #   label = mare[which(mare != 0)], col = 'black', cex = 1.2)
    })
    #plot temp1
    with(temp1, {
      points(x = xvalue, y = median_, pch = 16, cex = 1.2 * cex, col = 'gray45')
      segments(x0 = xvalue, y0 = l2, y1 = u2, col = 'gray45', lwd = .7)
      text(x = xvalue + .1, y = yy + .065, label = mare, col = 'gray45', cex = 1.2 * cex)
      # text(x = xvalue[which(mare != 0)], y = .13, 
      #   label = mare[which(mare != 0)], col = 'gray45', cex = 1.2)
    })
    box(col = 'black')

    if(ii %in% c(1, 4)) axis(side = 2, at = seq(-yy, yy, by = yy), las = 2, cex = 1.2 * cex)
    if(ii == 4) legend('bottomleft', c('Age + Length', 'Weight-at-age'), 
      bty = 'n', col = c('black', 'gray45'), lwd = 2)
    if(ii >=4) axis(side = 1, at = c(1.1, 2.1, 3.1, 4.1), las = 1, cex = 1.2 * cex,
      labels = temp$par.name, las = 2, cex = 1.2 * cex)
    print.letter(paste0(letters[ii], "."), xy = c(.06, 0.95), cex = 1.4 * cex)
    if(ii %in% c(1, 2, 3)) mtext(side = 3, line = .5, 
      text = paste("Data", d.amount[ii]), adj = 0, cex = .75 * cex)
  }
  mtext('Time-Invariant', side = 4, line = .7, outer = TRUE, adj = .75, cex = cex)
  mtext('Time-Varying Growth', side = 4, line = .7, outer = TRUE, at = .25, cex = cex)
  mtext('Relative Error', side = 2, outer = TRUE, line = 3, cex = 1.2 * cex)
  mtext('Selectivity Parameter', side = 1, outer = TRUE, line = 4.5, cex = 1.2 * cex)
  # mtext('Data Rich', side = 3, outer = TRUE, adj = 0, cex = cex) #.17
  # mtext('Data Rich - Late Survey', side = 3, outer = TRUE, adj = .38, cex = cex)
  # mtext('Data Moderate', side = 3, outer = TRUE, adj = .92, cex = cex)
}

#Save Plots
# tiff(width = 140, height = 160, units = 'mm', res = 300, 
#   file = "/Volumes/home/Empirical/figs/hake_selectivity_re_hires.tiff")
# plot_selex_hake(d = d.hake, yy = .15, cex = 1)
# dev.off()

#--------------------------------
#For Yellow

plot_selex_yellow <- function(d, yy = .05, cex = 1, add.to.y = .005){
  # print(unique(d$species))
  par(mfrow=c(2, 3), mar=c(0,0,0,0), oma=c(6, 5,2,3), mgp = c(.25, .6, 0))
  for(ii in 1:6){
    temp <- subset(d, d$unq == plot.order[ii])
    temp <- temp[duplicated(temp$median_) == FALSE, ]
    temp1 <- subset(d, d$unq == plot.order[ii + 6])
    temp1$xvalue <- temp1$xvalue + 0.2
    temp1 <- temp1[duplicated(temp1$median_) == FALSE, ]

# cat(ii, temp$mare, temp1$mare, '\n')
    # cbind(temp, temp1)
    plot(0, 0, type = 'n', xlim = c(.3, 4.5), ylim = c(-.05, yy + add.to.y),
      axes = FALSE, ann = FALSE)
    abline(h = 0, col = 'gray50')
    #plot temp
    with(temp, {
      points(x = xvalue, y = median_, pch = 16, cex = 1.2 * cex, col = 'black');
      segments(x0 = xvalue, y0 = l2, y1 = u2, col = 'black', lwd = .7);
      text(x = xvalue, y = yy , label = mare, col = 'black', cex = 1.2 * cex)
      # text(x = xvalue[which(mare != 0)], y = .12, 
      #   label = mare[which(mare != 0)], col = 'black', cex = 1.2)
    })
    #plot temp1
    with(temp1, {
      points(x = xvalue, y = median_, pch = 16, cex = 1.2 * cex, col = 'gray45')
      segments(x0 = xvalue, y0 = l2, y1 = u2, col = 'gray45', lwd = .7)
      text(x = xvalue + .1, y = yy + add.to.y, label = mare, col = 'gray45', cex = 1.2 * cex)
      # text(x = xvalue[which(mare != 0)], y = .13, 
      #   label = mare[which(mare != 0)], col = 'gray45', cex = 1.2)
    })
    box(col = 'black')

    if(ii %in% c(1, 4)) axis(side = 2, at = seq(-.05, .05, by = .05), las = 2, cex = 1.2 * cex)
    if(ii == 4) legend('bottomleft', c('Age + Length', 'Weight-at-age'), 
      bty = 'n', col = c('black', 'gray45'), lwd = 2)
    if(ii >=4) axis(side = 1, at = c(1.1, 2.1, 3.1, 4.1), las = 1, cex = 1.2 * cex,
      labels = temp$par.name, las = 2)
    print.letter(paste0(letters[ii], "."), xy = c(.06, 0.95), cex = 1.4 * cex)
    if(ii %in% c(1, 2, 3)) mtext(side = 3, line = .5, 
      text = paste("Data", d.amount[ii]), adj = 0, cex = .75 * cex)
  }
  mtext('Time-Invariant', side = 4, line = .7, outer = TRUE, adj = .75, cex = cex)
  mtext('Time-Varying Growth', side = 4, line = .7, outer = TRUE, at = .25, cex = cex)
  mtext('Relative Error', side = 2, outer = TRUE, line = 3, cex = 1.2 * cex)
  mtext('Selectivity Parameter', side = 1, outer = TRUE, line = 4.5, cex = 1.2 * cex)
  # mtext('Data Unrealistic', side = 3, outer = TRUE, adj = 0) #.17
  # mtext('Data Rich', side = 3, outer = TRUE, adj = .38)
  # mtext('Data Rich - Late Survey', side = 3, outer = TRUE, adj = .92)
}
# plot_selex(d = d.hake, yy = .1)
# results.sc.long.selex$after.fishery

#-------------------------------------------------------------------------------------
#Format selectivity data 
d <- subset(results.sc.long.selex, G != 'G7' & G != 'G8')
d <- ddply(d, .(G, variable, data.desc, data.amount, species), mutate, 
  median_ = median(value, na.rm = TRUE),
  l2 = quantile(value, 0.25, na.rm = TRUE),
  u2 = quantile(value, 0.75, na.rm = TRUE),
  mare = 100 * round(median(abs(value), na.rm = TRUE), 2),
  count = length(value))
d$unq <- paste(d$data.amount, d$G, d$data.desc)

#better parameter names
pars <- data.frame(variable = unique(d$variable),
                   par.name = c('Peak \n Fishery', 'Width \n Fishery', 
                                'Peak \n Survey', 'Width \n Survey'))
d <- merge(d, pars, by = 'variable')
d$xvalue <- as.numeric(as.factor(d$variable))

d.amount <- c("Rich", "Rich - Late Survey", "Moderate")
plot.order <- c( "rich G0 A + L",  "rich - late survey G0 A + L", "moderate G0 A + L",
              "rich G1 A + L",  "rich - late survey G1 A + L", "moderate G1 A + L",
              "rich G0 WtAtAge",  "rich - late survey G0 WtAtAge", "moderate G0 WtAtAge",
              "rich G1 WtAtAge",  "rich - late survey G1 WtAtAge", "moderate G1 WtAtAge")

d.hake <- subset(d, species == 'hake-age')
d.yellow <- subset(d, species == 'yellow-age')


dev.new(width = 5.5, height = 6, units = 'in')
#-------------------------------------------------------------------------------------
#Save Plots
tiff(width = 140, height = 160, units = 'mm', res = 300, 
  file = "/Volumes/home/Empirical/figs/FIG4_hake_selex.tiff")
  # hake_selectivity_re_hires.tiff")
plot_selex_hake(d = d.hake, yy = .15, cex = 1)
dev.off()

png(width = 140, height = 160, units = 'mm', res = 200, 
  file = "/Volumes/home/Empirical/figs/FIG4_hake_selex.png")
plot_selex_hake(d = d.hake, yy = .15, cex = 1)
dev.off()

#yelloweye
tiff(width = 140, height = 160, units = 'mm', res = 300, 
  file = "/Volumes/home/Empirical/figs/FIG5_yellow_selex.tiff")
plot_selex_yellow(d = d.yellow, yy = .07, add.to.y = .007)
dev.off()

png(width = 140, height = 160, units = 'mm', res = 200, 
  file = "/Volumes/home/Empirical/figs/FIG5_yellow_selex.png")
plot_selex_yellow(d = d.yellow, yy = .07, add.to.y = .007)
dev.off()








# png(width = 140, height = 160, units = 'mm', res = 200, 
#   file = "/Volumes/home/Empirical/figs/yellow_selectivity_re.png")
# plot_selex_yellow(d = d.yellow, yy = .07, cex = 1, add.to.y = .007)
# dev.off()




# plot_selex_hake(d = d.hake, yy = .15, cex = 1)



# png(width = 7.5, height = 5.8, units = 'in', res = 200,
#   "/Volumes/home/Empirical/figs/hake_selectivity_re.png")
# plot_selex_hake(d = d.hake, yy = .1)
# dev.off()

# jpeg(width = 7.5, height = 5.8, units = 'in', res = 500,
#   "/Volumes/home/Empirical/figs/hake_selectivity_re.jpg")
# plot_selex_hake(d = d.hake, yy = .1)
# dev.off()

# pdf(width = 7.5, height = 5.8, 
#   "/Volumes/home/Empirical/figs/FIG4.pdf")
# plot_selex_hake(d = d.hake, yy = .1)
# dev.off()

# plot_selex_yellow(d = d.yellow, yy = .05)


# png(width = 7.5, height = 5.8, units = 'in', res = 200,
#   "/Volumes/home/Empirical/figs/yellow_selectivity_re.png")
# plot_selex_yellow(d = d.yellow, yy = .05)
# dev.off()

# jpeg(width = 7.5, height = 5.8, units = 'in', res = 500,
#   "/Volumes/home/Empirical/figs/yellow_selectivity_re.jpg")
# plot_selex_yellow(d = d.yellow, yy = .05)
# dev.off()

# pdf(width = 7.5, height = 5.8, 
#   "/Volumes/home/Empirical/figs/FIG5.pdf")
# plot_selex_yellow(d = d.yellow, yy = .05)
# dev.off()

