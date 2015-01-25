source(paste(DIR$'General functions',"plotNutrients.R",sep=""))
source(paste(DIR$'General functions',"plot2DTracers.R",sep=""))
source(paste(DIR$'General functions',"plot3DTracers.R",sep=""))
source(paste(DIR$'General functions',"plot3DByCohortTracers.R",sep=""))
source(paste(DIR$'General functions',"prepTracerPlot.R",sep=""))
source(paste(DIR$'General functions',"get_par.R",sep=""))

this_run<-"TBGB_06_fourth_runb"
outPath<-paste(DIR$'Base',"\\ATLANTIS\\Model_runs\\TBGB\\",this_run,"\\output\\",sep="")
plotPath<-paste(DIR$'Base',"\\ATLANTIS\\Model_runs\\TBGB\\",this_run,"\\figures\\",sep="")

numStepsPerYear<-7
year0<-0
fishingStartYear<-0

ThisNC.nc<-open.ncdf(paste(outPath,"output.nc",sep=""))

plotNutrients(outPath, plotPath, year0=0,readNC=FALSE, fishingStartYear=0, numStepsPerYear, all=TRUE, names=NULL)
 
##########################################################
#read in parameter values to add to plot
thisFile<-paste(DIR$'Base',"\\ATLANTIS\\Model_runs\\TBGB\\",this_run,"\\TBGB_biol.prm", sep="")

#Time_Spawn
par_names<-paste(rep(c("j",""),6),sort(rep(c("SCA","MUS","OYS","SCL","IVH","IVS"),2)),"_Time_Spawn",sep="")
par_values<-unlist(lapply(par_names,get_par,thisFile=thisFile))

#Time_Age
par_names<-paste("Time_Age_",rep(c("j",""),6),sort(rep(c("SCA","MUS","OYS","SCL","IVH","IVS"),2)),sep="")
par_values<-unlist(lapply(par_names,get_par_vector,thisFile=thisFile,vectorLength=2))

##############################################################
#plot 2D tracers
ThisNC.nc<-open.ncdf(paste(outPath,"output.nc",sep=""))

names<-c("Invert_comm_Herb_N1","Invert_comm_Herb_N2", "Invert_comm_Scav_N1","Invert_comm_Scav_N2", "Mussels_N1","Mussels_N2", "Dredge_oysters_N1","Dredge_oysters_N2", "Scallops_N1","Scallops_N2","Surf_clams_N1","Surf_clams_N2")
plot2DTracers(outPath, plotPath, year0, fishingStartYear, numStepsPerYear, names=names,readNC=FALSE,addVLines=FALSE,VLinesValues=NULL,VLinesNames=NULL)

#other 2D that need doing
names<-c("Seagrass_N","Benthic_Carniv_N","Deposit_Feeder_N","Filter_Other_N","Benthic_grazer_N","Macrobenth_Other_N","Meiobenth_N","Macroalgae_N")
plot2DTracers(outPath, plotPath, year0, fishingStartYear, numStepsPerYear, names=names,readNC=FALSE,addVLines=FALSE,VLinesValues=NULL,VLinesNames=NULL)

###########################################################
## 3d tracers
names<-c("Barracouta")
names<-"ALL"
plot3DTracers(outPath, plotPath, names, isVertebrate=NULL, year0,  fishingStartYear,  numStepsPerYear, readNC=FALSE,nc=10)

plot3DTracers(outPath, plotPath, names=c("Diatom","MicroPB"), isVertebrate=NULL, year0,  fishingStartYear,  numStepsPerYear, readNC=FALSE,nc=10)


###
#just get data
thisData <- get.var.ncdf( ThisNC.nc,"Light")
MBData <- get.var.ncdf( ThisNC.nc,"MicroPB_N")
dzData<-get.var.ncdf( ThisNC.nc,"dz")
