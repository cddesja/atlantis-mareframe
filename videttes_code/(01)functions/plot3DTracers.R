plot3DTracers<-function(outPath, plotPath, names="ALL", isVertebrate=NULL, year0=0,  fishingStartYear=0,  numStepsPerYear=1, readNC=TRUE,nc=10, add_N2Name=TRUE){
  ## example variables
#       this_run<-"TBGB_06_fourth_runb"
#       outPath<-paste(DIR$'Base',"\\ATLANTIS\\Model_runs\\TBGB\\",this_run,"\\output\\",sep="")
#       plotPath<-paste(DIR$'Base',"\\ATLANTIS\\Model_runs\\TBGB\\",this_run,"\\figures\\",sep="")
#       year0=0
#       fishingStartYear=0
#       numStepsPerYear=364
#       names<-c("Barracouta")
#       nc=10 #number of cohorts. Will change this later if need to have varying by group
#   #####################
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

cat("length xaxs=",length(this_xaxis),"\n")

  names_verts<-NULL
  index_verts<-NULL


  if(length(names)==1){
    if(names=="ALL"){
      #READ IN NAMES FROM GROUPS.CSV FILE
      temp<-read.csv(paste(outPath,"..\\TBGB_Groups.csv",sep=""))
      #all 3d
      index<-temp$InvertType %in% c("FISH","SHARK","LG_ZOO","MED_ZOO","SM_ZOO","SED_BACT","CARRION","DINOFLAG","LAB_DET","REF_DET","MICROPHTYBENTHOS","PL_BACT","MAMMAL","LG_PHY","SM_PHY","BIRD")
      names<-lapply(temp$Name[index],str_trim,side="both")
      #vertebrates
      index_verts<-temp$InvertType %in% c("FISH","SHARK")
      names_verts<-lapply(temp$Name[index_verts],str_trim,side="both")
    }
    
  }
  
  nN<-length(names) #number of nutrients to be plotted
  
  for (n in 1:nN )      
  { 
    print(names[n])
    
    #if it's a vertebrate, there are 4 plots for each. Otherwise, just one
    ## they all have this plot
    if(add_N2Name){
      thisTracer<-paste(names[n],"_N",sep="")
    } else{
      thisTracer<-names[n]
    }
    
    
    isVertebrate<-ifelse(names[n] %in% names_verts,TRUE,FALSE)
                      
    new.graph(0.5,quiet=TRUE,filename=paste(plotPath, "TracerPlot",thisTracer,".jpg",sep=""))
    
    #NOW DEFINE Y VALUES AND LABELS FOR THE PLOT
    maxYForPlot<-0   # set up a y limit max, which will change below
    thisData <- get.var.ncdf( ThisNC.nc,thisTracer)  # Extract data from variable
    
    thisY<-apply(thisData*volumeData,3,sum)*(5.7*20/10^9)
    yLabString<-'Biomass, metric tons'
    
    cat("length thisY=",length(thisY),"\n")
    
    #MAKE THE PLOT
    YYear1 <- thisY[1]    
    # Calculate max Y value needed to show in plot       
    maxYForPlot<-max(thisY*1.1)
    
    plot(this_xaxis,thisY,lwd=4,type='l', col='black',lty='dashed',ylab="",main=thisTracer,xlab='Year',ylim=c(0,maxYForPlot ),xaxt="n")
    axis(side=1,labels=simYears,at=this_xat)
    par(las=0)
    mtext(side=2,text=yLabString,line=2)
    
        
    lines(x=c(0,max(this_xat,na.rm=TRUE)),y=c(YYear1,YYear1),lty='dashed',lwd=2,col='green')
    lines(x=c(fishingStartYear, fishingStartYear ),y=c(0,maxYForPlot),lty='dashed',lwd=2,col='red')
    
    dev.off()  # close the device              
                        
    if(isVertebrate){
      #it is a vertebrate, and there are 3 plots and for each (numbers, relative structural mgN, and relative reserved mgN), 
      # there is a line for each cohort
      ###############################
      ## first plot
      plot3DByCohortTracers(ThisNC.nc,tracer="Nums",nc=10,plotPath,name=names[n],this_xaxis,simYears,this_xat)
   
      ###################
      ## second plot
      ###############################
      plot3DByCohortTracers(ThisNC.nc,tracer="StructN",nc=10,plotPath,name=names[n],this_xaxis,simYears,this_xat)
  
      ###################
      ## third plot
      plot3DByCohortTracers(ThisNC.nc,tracer="ResN",nc=10,plotPath,name=names[n],this_xaxis,simYears,this_xat)
      
    }
  
  }
      
  
}