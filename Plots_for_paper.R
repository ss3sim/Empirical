#------------------------------------------------------------------------
#Make colored wtatage function
makeimage <- function(agevec=0:15, yrvec=1975:2013, mat, interpmat=NULL, maxWt=max(mat,na.rm=T),
                      meanvec=NULL, yrvec2=NULL, main="", dofont=TRUE, dorect=FALSE, 
                      addText=FALSE, byYr=1, byAge=1, textRnd=1, axisvals=TRUE,
                      height=7, width=7, doPNG=NULL, Colors=NULL) {
  if(is.null(meanvec)){
    meanvec <- mat[,1]
    mat <- mat[,-1]
  }
  
  ### Set colors
  if(is.null(Colors)){
    Colors <- rainbow(60)[1:50]
  }
  
  #   if(is.null(doPNG)) {
  #     windows(height=height,width=width)
  #   } else {
  #     png(file=doPNG,height=height,width=width,units="in",res=500)
  #   }
  #par(mar=c(4.2,4.2,4,1)+.1)
  # print(dim(mat))
  yrvec2 <- c(min(yrvec)-c(2,1),yrvec)  #c(1973,1974,yrvec)
  mat2 <- t(as.matrix(rbind(meanvec,rep(NA,length(meanvec)),mat)))
  # print(dim(mat2))
  image(x=agevec, y=yrvec2, z=mat2, axes=FALSE, xlab="", ylab="",
        col=Colors, main=main,breaks=seq(0,ceiling(maxWt),length=51))
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
  #   axis(1,at=agevec,cex.axis=.7);
  #   axis(2,at=c(min(yrvec)-2,yrvec2[-(1:2)]),
  #        lab=c("mean",yrvec),las=1,cex.axis=.7)

  if(axisvals) {
    axis(1)
    axis(2, at=c(min(yrvec)-2, pretty(yrvec)),
         lab=c("mean", pretty(yrvec)))
  }

  
  #   if(!is.null(doPNG)) {
  #     dev.off()
  #   }
}

#------------------------------------------------------------------------

### Parse the scenarios for the model to understand
parse_scenario <- function(scenario)
{
  val <- strsplit(scenario, '-')
  case.file <- val[[1]][grep('G', val[[1]])  ]
  case.spp <- val[[1]][length(val[[1]])]
  
  if(grep('age', scenario) == 1) {
    case.spp <- paste(val[[1]][length(val[[1]]) - 1], val[[1]][length(val[[1]])],
                      sep = '-')  
  }
  
  return(data.frame(case.file, case.spp)) 
}


### Plot the case time series
plot_growth_case <- function(scenario, y.lim = 5, x.lim=NULL)
{
  #Split Scenario into file and species
  val <- strsplit(scenario, '-')
  case.file <- val[[1]][grep('G', val[[1]])  ]
  case.spp <- val[[1]][length(val[[1]])]
  
  if(grep('age', scenario) == 1) {
    case.spp <-  paste(val[[1]][length(val[[1]]) - 1], val[[1]][length(val[[1]])],
                       sep = '-')  
  }
  #Find file in case folder
  folder <- paste0('cases/', case.spp)
  flz <- list.files(folder)
  file <- flz[grep(case.file, flz)]
  
  case.path <- paste0(folder, '/', file)
  
  #Read in case file
  cse <- readLines(case.path)
  par <- cse[2]
  devs <- cse[3]
  
  #Parse devs out
  parse.devs <- strsplit(devs, '; ')
  
  devs <- (eval(parse(text = parse.devs[[1]][2])))
  to.plot <- data.frame(years = 1:100, devs = devs)
  
  if(is.null(x.lim)) {
    x.lim <- max(abs(min(devs)),abs(max(devs)))
    x.lim <- c(-x.lim, x.lim)
  }
  
  plot(to.plot$devs, to.plot$years, pch = 19, xlab = NA,
       ylab = NA, ylim = y.lim, xlim = x.lim, axes=FALSE)
  # return(data.frame(case.file, case.spp))
}


