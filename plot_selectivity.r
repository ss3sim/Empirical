# head(results.sc.long.selex)
print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {
    tmp <- par("usr")
    text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
    text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
    text(x=text.x, y=text.y, labels=label, ...)
}

species <- c('hake-age', 'yellow-age')
spp <- c('hake-age', 'yellow-age')
sp <- 1
# spp <- species[1]

d <- subset(results.sc.long.selex, G != 'G7' & G != 'G8')
d <- ddply(d, .(G, variable, data.desc, data.amount), mutate, 
  median_ = median(value, na.rm = TRUE),
  l2 = quantile(value, 0.25, na.rm = TRUE),
  u2 = quantile(value, 0.75, na.rm = TRUE),
  mare = 100 * round(median(abs(value), na.rm = TRUE), 2),
  count = length(value))
d$unq <- paste(d$data.amount, d$G, d$data.desc)

#better parameter names
pars <- data.frame(variable = unique(d$variable),
                   par.name = c('Fish 1', 'Fish 3', 
                                'Surv 1', 'Surv 3'))
d <- merge(d, pars, by = 'variable')
d$xvalue <- as.numeric(as.factor(d$variable))

plot.order <- c("unrealistic G0 A + L", "rich G0 A + L",  "rich - late survey G0 A + L",
                "unrealistic G1 A + L", "rich G1 A + L",  "rich - late survey G1 A + L",
                "unrealistic G0 WtAtAge", "rich G0 WtAtAge",  "rich - late survey G0 WtAtAge",
                "unrealistic G1 WtAtAge", "rich G1 WtAtAge",  "rich - late survey G1 WtAtAge")


png(width = 7.5, height = 5.8, units = 'in', res = 200,
  "/Volumes/home/Empirical/figs/selectivity_re.png")
 
  par(mfrow=c(2, 3), mar=c(0,0,0,0), oma=c(4, 6,2,3))
  for(ii in 1:6){
    temp <- subset(d, d$unq == plot.order[ii])
    temp <- temp[duplicated(temp$median_) == FALSE, ]
    temp1 <- subset(d, d$unq == plot.order[ii + 6])
    temp1$xvalue <- temp1$xvalue + 0.2
    temp1 <- temp1[duplicated(temp1$median_) == FALSE, ]

    # cbind(temp, temp1)
    plot(0, 0, type = 'n', xlim = c(.9, 4.5), ylim = c(-.15, .15),
      axes = FALSE, ann = FALSE)
    abline(h = 0, col = 'gray50')
    #plot temp
    with(temp, {
      points(x = xvalue, y = median_, pch = 16, cex = .85, col = 'black');
      segments(x0 = xvalue, y0 = l2, y1 = u2, col = 'black', lwd = .7);
      text(x = xvalue, y = .12, label = mare, col = 'black', cex = 1.2)
      # text(x = xvalue[which(mare != 0)], y = .12, 
      #   label = mare[which(mare != 0)], col = 'black', cex = 1.2)
    })
    #plot temp1
    with(temp1, {
      points(x = xvalue, y = median_, pch = 16, cex = .85, col = 'gray45')
      segments(x0 = xvalue, y0 = l2, y1 = u2, col = 'gray45', lwd = .7)
      text(x = xvalue, y = .13, label = mare, col = 'gray45', cex = 1.2)
      # text(x = xvalue[which(mare != 0)], y = .13, 
      #   label = mare[which(mare != 0)], col = 'gray45', cex = 1.2)
    })
    box(col = 'black')

    if(ii %in% c(1, 4)) axis(side = 2, at = seq(-.1, .1, by = .1), las = 2, cex = 1.2)
    if(ii >=4) axis(side = 1, at = c(1.1, 2.1, 3.1, 4.1), labels = temp$par.name, las = 1, cex = 1.2)
    print.letter(paste0("(", letters[ii], ")"), xy = c(.05, 0.05), cex = 1.2)
  }
  mtext('Time-Invariant', side = 4, line = .7, outer = TRUE, at = .75)
  mtext('Time-Varying', side = 4, line = .7, outer = TRUE, at = .25)
  mtext('Relative Error', side = 2, outer = TRUE, line = 3, cex = 1.2)
  mtext('Selectivity Parameter', side = 1, outer = TRUE, line = 2.5, cex = 1.2)
  mtext('Unrealistic', side = 3, outer = TRUE, at = .17)
  mtext('Rich', side = 3, outer = TRUE, at = .5)
  mtext('Rich - Late Survey', side = 3, outer = TRUE, at = .85)

dev.off()