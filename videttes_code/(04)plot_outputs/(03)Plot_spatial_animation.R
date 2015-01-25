source(paste(DIR$'General functions',"prepTracerPlot.R",sep=""))
source(paste(DIR$'General functions',"get_depth_layer_data.R",sep=""))
source(paste(DIR$'General functions',"plotSpatialAnimation.R",sep=""))
source(paste(DIR$'General functions',"read_boxes.R",sep=""))


year0<-0   # or whatever is appropriate. 
fishingStartYear<-0  #All this does is draw a red vertical line on the plots. 
numStepsPerYear<-1  #Number of output intervals per year. With output intervals of 122 days, this is = 3. 
this_run<-"TBGB_06_fourth_runb"
this_path<-paste(DIR$'Base',"\\ATLANTIS\\Model_runs\\TBGB\\",this_run,sep="")
biol_input_file<-paste(this_path,"\\create_biol_input\\TBGB_input.txt",sep="")

ThisNC.nc<-open.ncdf(paste(outPath,"output.nc",sep=""))

names<-c("Diatom_N","MicroPB_N")
names<-c("Seabird_N")
  
plotSpatialAnimation(this_path, biol_input_file, names,  year0,  fishingStartYear,  numStepsPerYear, readNC=FALSE,showMax=FALSE)


