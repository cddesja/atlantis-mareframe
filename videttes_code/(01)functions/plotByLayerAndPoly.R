plotByLayerAndPoly<-function(outPath, plotPath, names, year0,  fishingStartYear,  numStepsPerYear, readNC=FALSE,layers="ALL",polys="ALL"){
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

  
  #the first non-zero entry in dz for each box is layer 0. (aka l==1 here).  It is layer 0 on the plots.
  #it is the layer closest to the sediment. 
  #which in shallow cases is also perhaps closest to the surface (if there is only 1 layer)
  thisDepthIndex<-matrix(NA,ncol=dim(dz)[2],nrow=dim(dz)[1])
  for(b in 1:numpolys){
    tempIndex<-dz[,b,1]>0
    tempSeq<-0*tempIndex
    nTRUE<-length(tempIndex[tempIndex==TRUE])
    tempSeq[tempIndex]<-seq(1,nTRUE)
    thisDepthIndex[,b]<-tempSeq
  }
  #the last layer is always sediment - calling it layer 6 here
  thisDepthIndex[6,]<-6
  
  for(n in 1:(length(names))){
    thisName<-names[n]
    cat("\n",thisName,"\n")
    thisData <- get.var.ncdf( ThisNC.nc,thisName)
    #plot by polygon and layer
    if(length(dim(thisData))==2){
      thisData<-array(data=thisData,dim=c(nl,np,nt))
    }
    nl<-dim(thisData)[1]
    np<-dim(thisData)[2]
    nt<-dim(thisData)[3] 
    
    ts<-seq(1,nt)
    colR<-colorRampPalette(colors=c(myRed,myBlue))
    cols<-colR(nl)
    spatialPlotPath<-paste(plotPath,"SpatialBiomassPlot//",sep="")
    for(p in 1:np){
      cat(p)
      thisMax<-max(thisData[,p,])
      thisMin<-min(thisData[,p,])
      new.graph(0.5,filename=paste(spatialPlotPath,thisName,"p",p,".jpg",sep=""),quiet=TRUE)
      par(oma=c(1,1,1,1),mar=c(2,10,1,1))
      plot(x=ts,y=thisData[1,p,],type="n",ylab="quanitity",xlab="time",ylim=c(thisMin,thisMax),xaxt="n")
      axis(side=1,labels=simYears,at=this_xat)
      if(nl==1){
        points(x=ts,y=thisData[1,p,],lty=l,lwd=2,type="l",col=myGrey)
      } else {
        for(l in 1:nl){       
          thisLIndex<-thisDepthIndex[,p]==l
          if(length(thisData[thisLIndex,p,])>0){
            points(x=ts,y=thisData[thisLIndex,p,],lty=l,lwd=2,type="l",col=cols[l])
          }    
        }
        
        par(xpd=TRUE) #so can put legend outside plotting area
        legx<-par("usr")[1]-0.35*par("usr")[2]
        legy<-0.5*(par("usr")[3]+par("usr")[4])
        legend(title="layer 0 \nis closest \nto sediment",legend=c(seq(0,nl-2),"Sediment"),col=cols,lty=seq(1,nl),x=legx,y=legy,bty="n",title.adj =0)
      }

      
      dev.off()
    }
  }
  
}
