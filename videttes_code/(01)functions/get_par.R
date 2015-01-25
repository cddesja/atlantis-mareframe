get_par<-function(thisFile,par_name){
  #getting a single value parameter from the biology input file biol.prm
#   par_name<-"SCA_Time_Spawn"
#   this_run<-"TBGB_06_fourth_runb"
#   thisFile<-paste(DIR$'Base',"\\ATLANTIS\\Model_runs\\TBGB\\",this_run,"\\TBGB_biol.prm", sep="")
  
  par_name<-str_trim(par_name,side="both") #trim white space
  
  data<-readLines(thisFile)
  x<-grep(par_name,data)
  
  if(length(x)>1){
    for(i in 1:(length(x))){
      this_temp_par<-str_trim(unlist(str_split(data[x][i]," "))[1],side="both")
      if(this_temp_par==par_name){
        par_value<-as.double(unlist(str_split(data[x][i]," "))[2])
      }
    }
  } else if(length(x)==1){
    par_value<-as.double(unlist(str_split(data[x]," "))[2])
  } else {
    quit(paste("no value for parameter ", par_name,sep=""))
  }
  
  return(par_value)
  
}
