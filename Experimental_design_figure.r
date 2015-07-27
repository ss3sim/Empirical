print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {
    tmp <- par("usr")
    text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
    text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
    text(x=text.x, y=text.y, labels=label, ...)
}

###############################################################################
## This creates a figure with the experimental design for the empirical paper
# setwd("C:/Users/Felipe/Dropbox/Fish 600/Growth")

require(RColorBrewer)

ScenPal <- adjustcolor(brewer.pal(3, "Set1"), alpha=0.5)
ScenPal2 <- c("black", brewer.pal(5, "Dark2")[c(2,4,5)])
ScenPal3 <- c("black", brewer.pal(5, "Dark2")[c(1,3)])
# Do black and white?
if(FALSE){
  ScenPal <- adjustcolor(c("gray30","gray50","gray70"), alpha=0.5)
  ScenPal2 <- c("black","gray30","gray50","gray70")
  ScenPal3 <- c("black","gray30","gray50")
} 

#windows(width=7, height=9)
# tiff("Experimental_design.tiff", width=7,height=9, res=500, units='in', compression='lzw')
dev.new(width = 5, height = 5, units = 'in')
cex <- 1




plot_experimental_design <- function(cex = 1){
  par(mar=c(4,4.5,0,1), oma=c(0,0,1,0), mgp = c(.5, .5, 0))

  matlay <- c(1,1,
              2,2,
              3,3)
  matlay <- matrix(matlay,ncol=2,byrow=T)

  layout(mat=matlay, heights=c(0.25,0.25,0.25,0.25), widths=c(0.5,0.5))

  ################################################################################
  ### First, do the panels with fishing mortality

  # F0 <- c(rep(0,25), rep(0.95,75))
  F1 <- c(rep(0,26), ((1/37)*(1:37))*0.95,(-(1/38)*(1:38))*0.5+0.95)

  plot(F1, type='l', axes=F, ylim=c(0,1.5), ylab=NA, lwd=2, ann = F)
  # mtext(side = 1, text = "Year", outer = F, line = 2, cex = .8 * cex)
  mtext(side=2, text=expression(F/F[MSY]), line=2.5, cex = .8 * cex)
  # lines(F1+0.01, col="gray", lwd=2)
  # legend("topleft", legend=c("Constant","Two-way trip"), lwd=2, col=c("black","gray"), bty='n')
  # legend("bottomright", legend=c("Constant"), lwd=2, col=c("black"), bty='n')
  axis(2, las = 1)
  axis(1)
  print.letter(paste0(letters[1], "."), xy = c(.03, 0.95), cex = 1.4 * cex)
  # text(0, 1.5, labels="a.", cex = 1.4 * cex)

  ####################################################################################
  ### Second, do the one with data

  #Data rich
  fishery1 <- data.frame(years = seq(26, 100, by = 1), 
    Nsamps = c(rep(35, 25), rep(75, 25), rep(100, 25)))
  survey1 <- data.frame(years = seq(41,100, by=3), 
    Nsamps = rep(100, 1))

  #Data rich, late survey
  fishery2 <- data.frame(years = seq(26,100, by=1), 
    Nsamps = c(rep(35, 25), rep(75, 25), rep(100, 25)))
  survey2 <- data.frame(years = seq(67,100, by=3), 
    Nsamps = rep(100, 1))

  #Data Moderate Scenario
  survey3 <- data.frame(years = seq(76,100, by=2), Nsamps = rep(100, 13))
  fishery3 <- data.frame(years = c(36,46,seq(51,66,by=5),71:100),
    Nsamps = c(rep(35, 1), rep(75, 5), rep(100, 30)))

  base <- exp(1)
  scaler <- 1.5

  plot(fishery1$years, rep(4, nrow(fishery1)), cex = log(fishery1[, 2] / 7, base = base)/scaler,
    pch = 19, col = ScenPal[1], xlim = c(0, 100), ylim = c(0, 4.5),
    axes = F, ylab = NA, xlab = NA)
  points(fishery2$years, rep(3.5, nrow(fishery2)), cex = log(fishery2[, 2] / 7, base = base)/scaler,
    pch = 19, col = ScenPal[2])  
  points(fishery3$years, rep(3, nrow(fishery3)), cex = log(fishery3[, 2] / 7, base = base)/scaler,
    pch = 19, col = ScenPal[3])  

  points(survey1$years, rep(1.5, nrow(survey1)), cex = log(survey1[, 2] / 7, base = base)/scaler,
    pch = 19, col = ScenPal[1])  
  points(survey2$years, rep(1, nrow(survey2)), cex = log(survey2[, 2] / 7, base = base)/scaler,
    pch = 19, col = ScenPal[2])  
  points(survey3$years, rep(.5, nrow(survey3)), cex = log(survey3[, 2] / 7, base = base)/scaler,
    pch = 19, col = ScenPal[3])  
  print.letter(paste0(letters[2], "."), xy = c(.03, 0.95), cex = 1.4 * cex)

  mtext(side = 3, text = 'Fishery', line = -.5, cex = .7 * cex)
  mtext(side = 3, text = 'Survey', line = -5.5, cex = .7 * cex)
  # mtext(side = 3, text = 'Survey', line = -15)
  # text(40, 4.3, labels="Fishery")
  # text(40, 1.9, labels="Survey")

  axis(1)
  # legend("topleft", legend=c("Data-rich late-survey","Data-rich", "Data-unrealistic"), pch=19, col=ScenPal, bty='n', 
  #        pt.cex=1.5)
  legend(x = par('usr')[1], y = 4.1, legend = c(35, 75, 100), pch = 21, pt.cex = log(c(35, 75, 100)/7, base = base)/scaler,
    bty = 'n')
  legend('bottomleft', legend = c('Rich', "Rich - Late Survey", "Moderate"),
    pch = 19, col = ScenPal, bty = 'n', pt.cex = 1.5 * cex )

  # legend(0, 1.5, legend=c(35,100,500), pch=21, pt.cex=log(c(35,100,500)/7, base=base)/scaler, bty='n')
  # text(0, 4.5, labels="b.", cex = 1.4)

  #################################################################################
  ### Third, do the time varing stuff

  ### From Peter's cases - Just copy and pasted
  dev2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.5, 6.11111111111111, 6.72222222222222, 
           7.33333333333334, 7.94444444444444, 8.55555555555556, 9.16666666666667, 9.77777777777777, 
           10.3888888888889, 11, 11, 10.3888888888889, 9.77777777777777, 9.16666666666667, 8.55555555555556, 
           7.94444444444444, 7.33333333333334, 6.72222222222222, 6.11111111111111, 5.5, 0, 0, 0, 11, 12.375, 
           13.75, 15.125, 16.5, 16.5, 16.5, 16.5, 16.1206896551724, 15.7413793103448, 15.3620689655172, 
           14.9827586206897, 14.6034482758621, 14.2241379310345, 13.8448275862069, 13.4655172413793, 
           13.0862068965517, 12.7068965517241, 12.3275862068966, 11.948275862069, 11.5689655172414, 11.1896551724138, 
           10.8103448275862, 10.4310344827586, 10.051724137931, 9.67241379310344, 9.29310344827586, 8.91379310344828, 
           8.53448275862069, 8.1551724137931, 7.77586206896552, 7.39655172413793, 7.01724137931035, 6.63793103448276, 
           6.25862068965517, 5.87931034482759, 5.5)
  dev1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 11.1543469723957, -1.03799715870328, 6.27059091500808, 5.24846841315007, 
            3.22652602127025, -11.9754232243074, -0.0526090743126986, -3.12714384822269, 1.38037313068654, 
            -4.00157636120656, 3.62107311002566, 3.05977112637004, -9.27486033552133, 3.78601884209986, 
            -2.34454069724591, 10.6520816462052, -1.46523662447839, 1.88682414704984, 2.97859029110509, 
            7.3942568593842, 4.36496863923671, -7.47000245308514, -0.0321423310383082, -1.48306128205327, 
            -0.626755273801159, 2.44141960355427, -10.7953451448666, 4.59696179157652, -1.42476214492493, 
            4.03153247653532, 3.40989869879991, -12.9354073654904, -0.555392092413555, -3.28526301675421, 
            -10.2630150335296, 4.11610249886079, -0.381917674531685, -8.96350850022787, 9.93298211587771, 
            3.19234894036513, 1.18115525489218, 0.406258973007837, 1.92855346126796, 7.21742692439339, 
            -2.6273188872232, -3.03569696263516, 0.774943235140995, -0.96832060149864, 4.30206232448288, 
            -4.48587431086561, -2.5534780691864, 1.83328746202287, -3.95449410471966, 5.83695126454094,
            -8.05461284096399, 8.10283107964042, -5.11805342509933, -7.00780100424766, 2.30079532029411, 
            0.80455259331152)

  dev3 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 1.83333333333334, 3.66666666666666, 5.5, 7.33333333333334, 9.16666666666667, 11, 
            12.8333333333333, 14.6666666666667, 16.5, 16.5, 14.6666666666667, 12.8333333333333, 11, 9.16666666666667, 
            7.33333333333334, 5.5, 3.66666666666666, 1.83333333333334, 0, 0, -0.868421052631582, -1.73684210526316, 
            -2.60526315789474, -3.47368421052632, -4.3421052631579, -5.21052631578947, -6.07894736842105, 
            -6.94736842105263, -7.81578947368421, -8.68421052631579, -9.55263157894737, -10.4210526315789, 
            -11.2894736842105, -12.1578947368421, -13.0263157894737, -13.8947368421053, -14.7631578947368, 
            -15.6315789473684, -16.5, -16.5, -15.6315789473684, -14.7631578947368, -13.8947368421053, 
            -13.0263157894737, -12.1578947368421, -11.2894736842105, -10.4210526315789, -9.55263157894737, 
            -8.68421052631579, -7.81578947368421, -6.94736842105263, -6.07894736842105, -5.21052631578947, 
            -4.3421052631579, -3.47368421052632, -2.60526315789474, -1.73684210526316, -0.868421052631582, 0)

  dev1 <- dev1/max(abs(c(dev1,dev2,dev3)))
  dev2 <- dev2/max(abs(c(dev1,dev2,dev3)))
  dev3 <- dev3/max(abs(c(dev1,dev2,dev3)))


  plot(1:100, rep(0,100), ylim=c(-1.2,1.2), axes=F, ann = FALSE, pch=19)
  axis(1)
  axis(2, at=c(-1,0,1), labels = c(-30, 0, 30), las = 2)
  mtext(side=2, text="Deviation (%)", line=2.5, cex=0.8 * cex)
  mtext(side=1, text="Year", line=2.5, cex = cex)
  #points(dev1, col=ScenPal2[2], pch=19)
  #points(dev2+0.02, col=ScenPal2[3], pch=19)
  points(dev3-0.02, col=ScenPal2[4], pch=19)
  points(rep(0,100), pch=19, col=ScenPal2[2])

  #legend("topleft", legend=c("Time invariant", "Random noise", "Time variance 1", "Time variance 2"), pch=19,
  #       col=ScenPal2, bty='n')
  legend("bottomleft", legend=c("Time invariant", "Time-varying"), pch=19,
         col=ScenPal2[c(2,4)], bty='n')
  print.letter(paste0(letters[3], "."), xy = c(.03, 0.95), cex = 1.4 * cex)
  # text(0, 1.2, labels="c.", cex = 1.4)

}


tiff(width = 140, height = 140, units = 'mm', res = 300, 
  file = 'figs/FIG1_experimental_design.tiff')
plot_experimental_design()
dev.off()

png(width = 140, height = 140, units = 'mm', res = 150, 
  file = 'figs/FIG1_experimental_design.png')
plot_experimental_design()
dev.off()

