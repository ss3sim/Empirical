update.packages(c('knitr', 'devtools', 'roxygen2'))
## Build the ss3sim developement package
remove.packages("ss3sim")
devtools::install_github("ss3sim/ss3sim")
library(ss3sim)
library(r4ss)

d <- "C:/NOAA2015/ss3Sim_Empirical/"
setwd(d)
case_folder <- paste0(d, "Cases_col") #casefolder doesn't seem to matter. Puts sims in wd
om <- paste0(d, "colOM")
em <- paste0(d, "colEM")

if(F) {
  run_ss3sim(iterations = 1, scenarios ="E0-F0-R0-col",
  	case_files = list(E = "E", F = "F", R = "R"),
  	case_folder = case_folder, om_dir = om,	em_dir = em)
  #	ss3path = d)

  run_ss3sim(iterations = 1, scenarios ="E0-F0-R0-G0-col",
  	case_files = list(E = "E", F = "F", R = "R", G="G"),
  	case_folder = case_folder, om_dir = om,	em_dir = em)
  #	ss3path = d) #this doesn't work with timevary function

  run_ss3sim(iterations = 1, scenarios ="E0-F0-R0-G1-col",
  	case_files = list(E = "E", F = "F", R = "R", G="G"),
  	case_folder = case_folder, om_dir = om,	em_dir = em)
}

noG <- SS_output(dir="E0-F0-R0-col/1/om", covar=FALSE)
SS_plots(noG,plot=1,uncertainty=F)

G0 <- SS_output(dir="E0-F0-R0-G0-col/1/om", covar=FALSE)
SS_plots(G0,plot=1,uncertainty=F)

G1 <- SS_output(dir="E0-F0-R0-G1-col/1/om", covar=FALSE)
SS_plots(G1,plot=1,uncertainty=F)



makeimage <- function(agevec=0:15,yrvec=1975:2013,mat,interpmat=NULL,maxWt=max(mat,na.rm=T),
                      meanvec=NULL,yrvec2=NULL,main="",dofont=TRUE,dorect=FALSE, 
                      addText=FALSE,byYr=1,byAge=1, textRnd=1,
                      height=7,width=7,doPNG=NULL) {
  if(is.null(meanvec)){
    meanvec <- mat[,1]
    mat <- mat[,-1]
  }
  if(is.null(doPNG)) {
    windows(height=height,width=width)
  } else {
    png(file=doPNG,height=height,width=width,units="in",res=300)
  }
  par(mar=c(4.2,4.2,4,1)+.1)
  print(dim(mat))
  yrvec2 <- c(min(yrvec)-c(2,1),yrvec)  #c(1973,1974,yrvec)
  mat2 <- t(as.matrix(rbind(meanvec,rep(NA,length(meanvec)),mat)))
  print(dim(mat2))
  image(x=agevec,y=yrvec2,z=mat2,axes=FALSE,xlab='Age',ylab='Year',
        col=rainbow(60)[1:50], main=main,breaks=seq(0,ceiling(maxWt),length=51))
  # add text
  if(addText) {
	  zdataframe <- expand.grid(yr=yrvec2,age=agevec)
	  zdataframe$z <- c(t(mat2))
	  if(!is.null(interpmat)){
	    interpmat2 <- cbind(meanvec,NA,interpmat)
	    zdataframe$interp <- c(t(interpmat2))
	  } else{
	    zdataframe$interp <- 0
	  }
	  zdataframe$font <- 1
	  if(dofont) zdataframe$font <- ifelse(is.na(zdataframe$interp),2,1)

	  ztext <- format(round(zdataframe$z,textRnd))
	  ztext[ztext=="  NA"] <- ""
	  ztext[ztext=="   NA"] <- ""
	  text(x=zdataframe$age[seq(1,length(zdataframe$age),byAge)],
          y=zdataframe$yr[seq(1,length(zdataframe$yr),byYr)],
          label=ztext[seq(1,length(zdataframe$age),byAge)],
          font=zdataframe$font,cex=.7)
    interp <- zdataframe[is.na(zdataframe$interp) & zdataframe$yr!=1974,]
    if(dorect)
      rect(interp$age-.5,interp$yr-.5,
           interp$age+.5,interp$yr+.5,col=rgb(0,0,0,.3),density=20)
  }

  # finish plot
  axis(1,at=agevec,cex.axis=.7);
  axis(2,at=c(min(yrvec)-2,yrvec2[-(1:2)]),
       lab=c("mean",yrvec),las=1,cex.axis=.7)

  if(!is.null(doPNG)) {
    dev.off()
  }
}

maxWt<-40

wtatage.noG <- read.table("E0-F0-R0-col/1/om/wtatage.ss_new",header=F,skip=2)
names(wtatage.noG) <- c("yr", "seas", "gender", "growpattern", "birthseas", "fleet",
          paste0("a",c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
                     "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25")))
wtatage.noG <- wtatage.noG[wtatage.noG$fleet==1,-(1:6)]
head(wtatage.noG)
meanvec <- t(apply(wtatage.noG,2,mean))
makeimage(agevec=0:25,yrvec=41:100,mat=wtatage.noG[-(1:40),],maxWt= maxWt,
            meanvec=meanvec,main="Mean weight at age, time-invariant growth",
            addText=T,byYr=2,byAge=2,textRnd=0,
            height=9,width=7,doPNG="Figures/E0F0R0-col_wtataage.png")


wtatage.G0 <- read.table("E0-F0-R0-G0-col/1/om/wtatage.ss_new",header=F,skip=2)
names(wtatage.G0) <- c("yr", "seas", "gender", "growpattern", "birthseas", "fleet",
          paste0("a",c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
                     "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25")))
wtatage.G0 <- wtatage.G0[wtatage.G0$fleet==1,-(1:6)]
head(wtatage.G0)
meanvec <- t(apply(wtatage.G0,2,mean))
makeimage(agevec=0:25,yrvec=41:100,mat=wtatage.G0[-(1:40),],maxWt= maxWt,
            meanvec=meanvec,main="Mean weight at age, time-invariant growth",
            addText=T,byYr=2,byAge=2,textRnd=0,
            height=9,width=7,doPNG="Figures/E0F0R0G0-col_wtataage.png")

wtatage.G1 <- read.table("E0-F0-R0-G1-col/1/om/wtatage.ss_new",header=F,skip=2)
names(wtatage.G1) <- c("yr", "seas", "gender", "growpattern", "birthseas", "fleet",
          paste0("a",c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
                     "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25")))
wtatage.G1 <- wtatage.G1[wtatage.G1$fleet==1,-(1:6)]
head(wtatage.G1)
meanvec <- t(apply(wtatage.G1,2,mean))
makeimage(agevec=0:25,yrvec=41:100,mat=wtatage.G1[-(1:40),],maxWt= maxWt,
            meanvec=meanvec,main="Mean weight at age, time-invariant growth",
            addText=T,byYr=2,byAge=2,textRnd=0,
            height=9,width=7,doPNG="Figures/E0F0R0G1-col_wtataage.png")
