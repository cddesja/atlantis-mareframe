#####################################################################
# start with clean slate: 
# rm(list = ls())

#####################################################################

# Set paths and working directories

# this function in fishmod
make.filename<-function(file="",path="",add.terminal=F) {
  if(path != "") {
    plc <- substring(path, nchar(path))
    if(!(plc == "\\" | plc == "/")) path <- paste(path, "\\", sep = "")
  }
  filename <- paste(path, file, sep = "")
  if(add.terminal==T) {
    plc <- substring(filename, nchar(filename))
    if(!(plc == "\\" | plc == "/")) filename <- paste(filename, "\\", sep = "")
  }
  return(filename)
}


assign.directories<-function(base="C:\\projects\\2013\\FIFI1401\\")
	  {
	  DIR<-list()
	  
	  # base directory
	  DIR[["Base"]]<-base
	 
	  ## R directory
	  #DIR[["R"]]<-make.filename("R",DIR[["Base"]],T)
	  
	  ## R functions
	  DIR[["Functions"]]<-make.filename("R",DIR[["Base"]],T)
	  ## General functions
	  DIR[["General functions"]]<-make.filename("(01)functions",DIR[["Functions"]],T)
    
	  DIR[["Reports"]]<-make.filename("reports",DIR[["Base"]],T)
	
	  # Data
	  DIR[["Data"]]<-make.filename("data",DIR[["Base"]],T)


	  # Figures
	  DIR[["Figures"]]<-make.filename("figures",DIR[["Base"]],T)

	  # Tables
	  DIR[["Tables"]]<-make.filename("tables",DIR[["Base"]],T)
    
	  #backup
	  DIR[["Backup"]]<-make.filename("backup",DIR[["Base"]],T)

    
    #testing
	  DIR[["Testing"]]<-make.filename("testing",DIR[["Base"]],T)

	
	  return(DIR)
	}

DIR<-assign.directories(base="C:\\projects\\2013\\FIFI1401")

DIR

# tidyup
rm(assign.directories)

#set colours
myRed<-rgb("red"=0.8,"blue"=0.5,"green"=0)
myRed_trans<-rgb("red"=0.8,"blue"=0.5,"green"=0,"alpha"=0.3)
myBlue<-rgb("red"=0,"blue"=0.8,"green"=0.5)
myBlue_trans<-rgb("red"=0,"blue"=0.8,"green"=0.5,"alpha"=0.3)
myGreen<-rgb("red"=0.5,"blue"=0,"green"=0.8)
myGreen_trans<-rgb("red"=0.5,"blue"=0,"green"=0.8,"alpha"=0.3)
myOrange<-rgb("red"=1,"blue"=0,"green"=0.7)
myOrange_trans<-rgb("red"=1,"blue"=0,"green"=0.7,"alpha"=0.3)
myPurple<-rgb("red"=0.63,"blue"=0.94,"green"=0.12)
myPurple_trans<-rgb("red"=0.63,"blue"=0.94,"green"=0.12,"alpha"=0.3)
myGrey<-rgb("red"=0.5,"blue"=0.5,"green"=0.5)
myGrey_trans<-rgb("red"=0.5,"blue"=0.5,"green"=0.5,"alpha"=0.3)

library(ncdf)
library(sm)  # for pause()
library(stringr) #for str_trim
library(shiny)
library(fishmod) #new.graph
library(animation)

source(paste(DIR$'General functions',"\\new.graph.R",sep=""))
