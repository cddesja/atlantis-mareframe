prepTracerPlot<-function(thisNC.nc, year0,  fishingStartYear,  numStepsPerYear){
  returnList<-NULL
  
  volumeData <- get.var.ncdf( ThisNC.nc,"volume") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
  returnList$'volumeData'<-volumeData
  volDims<-dim(volumeData)  # Just use volume to see how many time steps are in the data
  volBottomCells<-volumeData[volDims[1],,] # Because dz (height) of lowest boxes is 1m,I can say that Area is equal to the volume of the lowest boxes 
  dz <- get.var.ncdf( ThisNC.nc,"dz") 
  returnList$'dz'<-dz
  numDepths<-dim(dz)[1]
  zBottomCell<- dz[numDepths,1,1]
  returnList$'areaData'<-volBottomCells/zBottomCell
  numTimeSteps<-volDims[3]
  
  lastYear<-trunc(numTimeSteps/numStepsPerYear)
  
  returnList$'simYears'<-(year0+(seq(0,lastYear)))
  
  returnList$'this_xaxis'<-seq(1,numTimeSteps)
  returnList$'this_xat'<-seq(1,length(returnList$this_xaxis),by=numStepsPerYear)[1:length(returnList$'simYears')]
  
  return(returnList)
  
  
}