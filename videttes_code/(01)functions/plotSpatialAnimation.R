plotSpatialAnimation<-function(this_path, biol_input_file, names,  year0,  fishingStartYear,  numStepsPerYear, readNC=FALSE,showMax=FALSE){
  # example parameters
  #   names<-c("Barracouta_N")
  #   year0<-0   # or whatever is appropriate. 
  #   fishingStartYear<-0  #All this does is draw a red vertical line on the plots. 
  #   numStepsPerYear<-1  #Number of output intervals per year. With output intervals of 122 days, this is = 3. 
  #   this_run<-"TBGB_06_fourth_runb"
  #   this_path<-paste(DIR$'Base',"\\ATLANTIS\\Model_runs\\TBGB\\",this_run,sep="")
  #   biol_input_file<-paste(this_path,"\\create_biol_input\\TBGB_input.txt",sep="")
  #   showMax<-FALSE
  
  readNC<-FALSE #set this to true if want to read the nc file in.
  
  col2<-rgb("blue"=0.4,"green"=0,"red"=0.3)
  col1<-rgb("blue"=1,"green"=0.9,"red"=0.6)
  thisColRamp<-colorRampPalette(colors=c(col1,col2))
  thisCols<-c("white",thisColRamp(10))
  
  if(readNC){
    print('**** OPENING THE NETCDF FILE CAN TAKE 5 MINUTES ***** ')
    ThisNC.nc<-open.ncdf(paste(outPath,"output.nc",sep=""))
  }
  
  temp<-prepTracerPlot(ThisNC.nc)
  
  areaData<-temp$'areaData'
  this_xaxis<-temp$'this_xaxis'
  simYears<-temp$'simYears'
  this_xat<-temp$'this_xat'
  volumeData<-temp$'volumeData'
  dz<-temp$'dz'
  
  temp<-get_depth_layer_data(thisFile=biol_input_file)
  numlayers_by_poly<-temp$'numlayers_by_poly'
  numpolys<-temp$'numpolys'
  depths<-temp$'depths'
  numlayers<-temp$'numlayers'
  dl<-temp$'dl'
  
  bgm<-read_boxes(fnm=paste(this_path,"\\TBGB.bgm",sep=""))
  
  #just for plotting
  thisYlim<-c(1380000, 1510000)
  thisXlim<-c(640000, 770000)
  
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
  
  
  for (n in 1:(length(names))){
    thisName<-str_trim(names[n],side="both")
    
    print(thisName)
    
    thisData <- get.var.ncdf( ThisNC.nc,thisName)
    
    thisDim<-dim(thisData)
    
    thisDfMax<-max(thisData)
    thisDfMin<-min(thisData)
    
    #log scale
    if(length(thisDim)==2){
      logData<-apply(thisData,c(1,2),function(x){ifelse(x>0,log(x),NA)})
    }else {
      logData<-apply(thisData,c(1,2,3),function(x){ifelse(x>0,log(x),NA)})
    }
    thisColIndex<-ceiling(((logData-min(logData,na.rm=TRUE))/(max(logData,na.rm=TRUE)-min(logData,na.rm=TRUE)))*10)+1
    
    thisTime<-ifelse(length(thisDim)==2,dim(thisData)[2],dim(thisData)[3])
    
    htmlFile<-paste(thisName,".html",sep="")
    
    ani.options(interval=1)
    
    saveHTML({
      
      for(t in 1:thisTime){
        if(length(thisDim)==2){
          thisDf<-thisData[,t]
        } else{
          thisDf<-thisData[,,t]
        }      
        
        if(length(thisDim)==2){
          par(mfcol=c(1,1),mar=c(0,0,0,0),oma=c(0,0,2,0))
          plot(x=1,y=1,ylim=thisYlim,xlim=thisXlim,type="n",xlab="",ylab="",xaxt="n",yaxt="n",asp=1)
          mtext(paste("Layer epibenthic",sep=""),side=3,adj=0.01,line=-1.5)
          for(b in 1:numpolys){
            this_box<-bgm$verts[[b]]
            x<-unlist(str_split(this_box," "))
            this_x<-as.double(x[seq(1,length(x),by=2)])
            this_y<-as.double(x[seq(2,length(x),by=2)])
            thisCol<-thisCols[thisColIndex[b,t]]
            polygon(x=this_x,y=this_y,col=thisCol)  
          } 
        } else {
          par(mfcol=c(3,2),mar=c(0,0,0,0),oma=c(0,0,2,0))    
          for(l in 1:(dim(dz)[1])){
            printLayer<-ifelse(l==6,"Sediment",l-1)        
            plot(x=1,y=1,ylim=thisYlim,xlim=thisXlim,type="n",xlab="",ylab="",xaxt="n",yaxt="n",asp=1)
            mtext(paste("Layer ",printLayer,sep=""),side=3,adj=0.01,line=-1.5)
            
            for(b in 1:numpolys){
              #get the cell in this box that corresponds with this layer
              thisLIndex<-thisDepthIndex[,b]==l
              #get the correct colour vector, which is specific for this species group
              thisCol<-thisCols[thisColIndex[thisLIndex,b,t]]
              # the coordinates for this box
              this_box<-bgm$verts[[b]]
              #polygon line colour is black if the box has this layer in it and grey otherwise
              polyLineColour<-ifelse(length(thisColIndex[thisLIndex,b,t])==0,"grey","black")
              x<-unlist(str_split(this_box," "))
              this_x<-as.double(x[seq(1,length(x),by=2)])
              this_y<-as.double(x[seq(2,length(x),by=2)])
              polygon(x=this_x,y=this_y,col=thisCol,border=polyLineColour)  
            }
          }
        }   
        if(showMax){
          thisMax<-ifelse(thisDfMax>100,round(thisDfMax,-2),ifelse(thisDfMax>1,round(thisDfMax),ifelse(thisDfMax>0.0001,round(thisDfMax,4),round(thisDfMax,6))))
          mtext(paste("Year: ",year0+t-1,", Maximum: ",thisMax,sep=""),side=3,adj=0,outer=TRUE)
        }
      }
    }
    ,  htmlfile=htmlFile, verbose=FALSE,autobrowse=FALSE, img.name=thisName)
    
    rm(logData,thisData,thisColIndex)
  }
}
