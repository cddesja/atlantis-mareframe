plot2DTracers<-function(outPath, plotPath, names=NULL, year0=0,  fishingStartYear=0,  numStepsPerYear=1, readNC=TRUE, addVLines=FALSE,VLinesValues="",VLinesNames="",numYears=NULL){
  #use names for the nutrients you want to plot, if all=FALSE
#     this_run<-"TBGB_06_fourth_runb"
#     outPath<-paste(DIR$'Base',"\\ATLANTIS\\Model_runs\\TBGB\\",this_run,"\\output\\",sep="")
#     plotPath<-paste(DIR$'Base',"\\ATLANTIS\\Model_runs\\TBGB\\",this_run,"\\figures\\",sep="")
#     year0=0
#     fishingStartYear=0
#     numStepsPerYear=364
#     all=TRUE
#     name=NULL
  #####################
  #####################
  
  if(readNC){
    print('**** OPENING THE NETCDF FILE CAN TAKE 5 MINUTES ***** ')
    ThisNC.nc<-open.ncdf(paste(outPath,"output.nc",sep=""))
  }
  
  temp<-prepTracerPlot(ThisNC.nc, year0,  fishingStartYear,  numStepsPerYear)

  areaData<-temp$'areaData'
  this_xaxis<-temp$'this_xaxis'
  simYears<-temp$'simYears'
  this_xat<-temp$'this_xat'
  volumeData<-temp$'volumeData'
  
  numYears<-trunc(length(this_xat)/numStepsPerYear)
  
  nN<-length(names) #number of nutrients to be plotted
  
  for (n in 1:nN )      
  { 
    print(names[n])
    
    thisTracer<-names[n]
    
    this_VLinesValue<-VLinesValues[n]
    this_VLinesValues<-this_VLinesValue+364*seq(0,numYears-1)
    
    new.graph(0.5,quiet=TRUE,filename=paste(plotPath, "TracerPlot",thisTracer,".jpg",sep=""))
    
    #NOW DEFINE Y VALUES AND LABELS FOR THE PLOT
    maxYForPlot<-0   # set up a y limit max, which will change below
    thisData <- get.var.ncdf( ThisNC.nc,thisTracer)  # Extract data from variable
    
    thisY<-apply(thisData*areaData,2,sum)*(5.7*20/10^9)
    yLabString<-'Biomass, metric tons'
    
    #MAKE THE PLOT
    YYear1 <- thisY[1]    
    # Calculate max Y value needed to show in plot       
    maxYForPlot<-max(thisY*1.1)
    
    plot(this_xaxis,thisY,lwd=4,type='l', col='black',lty='dashed',ylab="",main=thisTracer,xlab='Year',ylim=c(0,maxYForPlot ),xaxt="n")
    axis(side=1,labels=simYears,at=this_xat)
    par(las=0)
    mtext(side=2,text=yLabString,line=2)
    
    #add vertical lines
    if(addVLines){
      abline(v=this_VLinesValues,col=myGrey_trans,lwd=2)
      mtext(side=3,adj=0.1,text=VLinesNames[n],col=myGrey)
    }
    
    lines(x=c(0,max(this_xat,na.rm=TRUE)),y=c(YYear1,YYear1),lty='dashed',lwd=2,col='green')
    lines(x=c(fishingStartYear, fishingStartYear ),y=c(0,maxYForPlot),lty='dashed',lwd=2,col='red')
    
    dev.off()  # close the device
  }
  
}