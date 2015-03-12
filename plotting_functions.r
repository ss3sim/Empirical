#------------------------------------------------------------------------
#Make colored wtatage function
makeimage <- function(agevec=0:15,yrvec=1975:2013,mat,interpmat=NULL,maxWt=max(mat,na.rm=T),
                      meanvec=NULL,yrvec2=NULL,main="",dofont=TRUE,dorect=FALSE, 
                      addText=FALSE,byYr=1,byAge=1, textRnd=1,
                      height=7,width=7,doPNG=NULL) {
  if(is.null(meanvec)){
    meanvec <- mat[,1]
    mat <- mat[,-1]
  }
#   if(is.null(doPNG)) {
#     windows(height=height,width=width)
#   } else {
#     png(file=doPNG,height=height,width=width,units="in",res=300)
#   }
  par(mar=c(4.2,4.2,4,1)+.1)
  # print(dim(mat))
  yrvec2 <- c(min(yrvec)-c(2,1),yrvec)  #c(1973,1974,yrvec)
  mat2 <- t(as.matrix(rbind(meanvec,rep(NA,length(meanvec)),mat)))
  # print(dim(mat2))
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
  
#   if(!is.null(doPNG)) {
#     dev.off()
#   }
}

#------------------------------------------------------------------------
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

plot_growth_case <- function(scenario, y.lim)
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
  
  plot(to.plot$devs, to.plot$years, pch = 19, xlab = 'Deviations',
       ylab = 'Year', main = paste(case.spp, '\n', par), ylim = y.lim)
  # return(data.frame(case.file, case.spp))
}


#------------------------------------------------------------------------
make_wtatage_png <- function(scenario, main.title = 'Weight at Age', yrvec = 41:100,
  height = 7.18, width = 12.25, res = 300)
{
  wtatage <- read.table(paste0(scenario, '/1/om/wtatage.ss_new'), header = F, skip = 2)
  names(wtatage)[1:6] <- c('yr', 'seas', 'gender', 'growpattern', 'birthseas', 'fleet')
  ages <- 7:ncol(wtatage)
  
  names(wtatage)[7:ncol(wtatage)] <- paste0('a', ages- 7)
  wtatage <- wtatage[wtatage$fleet == 1, -(1:6)]
  meanvec <- t(apply(wtatage, 2, mean))  
  
  file.name <- parse_scenario(scenario)
  
  png(file = paste0('figs/', file.name$case.file, '_', file.name$case.spp, '.png'),
    height = height, width = width, units = 'in', res = res)

  par(mfcol = c(1, 2))
  plot_growth_case(scenario, y.lim = range(yrvec))
  
  makeimage(agevec = ages - 7, yrvec = yrvec, mat = wtatage[-(1:yrvec[1] - 1), ], maxWt = max(unlist(wtatage)), 
            meanvec = meanvec, main = main.title,
            addText = T, byYr = 2, byAge = 2, textRnd = 0, height = 7, width = 7)
            # doPNG = paste0('figs', '/', scenario, '_wtatage', '.png'))
  dev.off()
}