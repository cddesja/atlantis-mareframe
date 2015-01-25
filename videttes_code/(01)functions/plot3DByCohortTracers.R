plot3DByCohortTracers<-function(ThisNC.nc,tracer="Nums",nc=10,plotPath,name,this_xaxis,simYears,this_xat){
  theseTracers<-paste(name,seq(1,nc),"_",tracer,sep="")
  
  new.graph(0.5,quiet=TRUE,filename=paste(plotPath, "TracerPlot",name,"_",tracer,".jpg",sep=""))
  par(oma=c(1,1,1,1),mar=c(2,10,1,1))
  
  #NOW DEFINE Y VALUES AND LABELS FOR THE PLOT
  maxYForPlot<-0  # set up a y limit max, which will change below
  for (ageclass in 1:nc) # loop over all 10 age classes
  {
    #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
    thisData <- get.var.ncdf( ThisNC.nc,theseTracers[ageclass])  # Extract data from variable
    thisData[thisData==0]<-NA  # Replace 0's with NA
    #     thisDataNums<-apply(thisData,3,sum,na.rm = TRUE)#Get nums over time, summing over depth and location 
    if(tracer=="Nums"){
      thisDataNums<-apply(thisData,3,sum,na.rm = TRUE)#Get nums over time, summing over depth and location 
      thisY<-thisDataNums
    } else{
      thisDataMeanMg<-apply(thisData,3,mean,na.rm = TRUE)#Get mean size over time, averaging over depth and location 
      thisY<-thisDataMeanMg/thisDataMeanMg[1]    
    }   
    maxYObserved<-max(thisY,na.rm=TRUE)
    maxYForPlot<-max(c(maxYForPlot,1.1*maxYObserved),na.rm=TRUE) # Calculate max Y value needed to show in plot
  }
  #NOW DO THE PLOT
  for (ageclass in 1:10) # loop over all 10 age classes
  {
    #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
    thisData <- get.var.ncdf( ThisNC.nc,theseTracers[ageclass])  # Extract data from variable
    thisData[thisData==0]<-NA  # Replace 0's with NA
    if(tracer=="Nums"){
      thisDataNums<-apply(thisData,3,sum,na.rm = TRUE)#Get nums over time, summing over depth and location 
      thisY<-thisDataNums
      thisYLab<-tracer
    } else{
      thisDataMeanMg<-apply(thisData,3,mean,na.rm = TRUE)#Get mean size over time, averaging over depth and location 
      thisY<-thisDataMeanMg/thisDataMeanMg[1] 
      thisYLab<-paste(tracer,"/initial ",tracer,sep="")
    }      
    
    rainbowColors<-rainbow(10)    # Make a rainbow color palette for plotting.     
    if (ageclass==1)  # If we are plotting age class 1, start a new plot
    {
      plot(this_xaxis,thisY,lwd=4,type='l', col=rainbowColors[ageclass],lty=ageclass,ylab="",main="",xlab='Year',ylim=c(0,maxYForPlot ),xaxt="n")
      axis(side=1,labels=simYears,at=this_xat)
      
      par(las=0)
      mtext(text=thisYLab,side=2,line=3)
      mtext(text=name,side=3,adj=0)
      
      print(name)
    } else  # If we are doing age class 2 or higher, just add lines to the existing plot made for Age class 1. 
    {
      lines(this_xaxis,thisY,lwd=1.5,type='l',lty=ageclass,col = rainbowColors[ageclass])
      
    }
    
  } 
  par(xpd=TRUE) #so can put legend outside plotting area
  legx<-par("usr")[1]-0.35*par("usr")[2]
  legy<-0.5*(par("usr")[3]+par("usr")[4])
  legend(legend=seq(1,10),col=rainbow(10),lty=seq(1,10),x=legx,y=legy,bty="n")
  
  dev.off()  # close the device   
  
}