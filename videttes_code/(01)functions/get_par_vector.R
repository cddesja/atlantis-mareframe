get_par_vector<-function(thisFile,par_name,vectorLength=Inf){
  #getting a vector of parameters from the biology input file biol.prm
  #   par_name<-"Time_Age_OYS"
  #   this_run<-"TBGB_06_fourth_runb"
  #   thisFile<-paste(DIR$'Base',"\\ATLANTIS\\Model_runs\\TBGB\\",this_run,"\\TBGB_biol.prm", sep="")
  #   vectorLength<-2
  
  data<-readLines(thisFile)
  par_name<-str_trim(par_name,side="both") #trim white space
  x<-grep(par_name,data)+1
  
  cat(paste("length=",length(x),"par_name=",par_name,"\n"))

  if(length(x)>1){
    for(i in 1:(length(x))){
      this_temp_par<-str_trim(unlist(str_split(data[x-1][i]," "))[1],side="both")
      if(this_temp_par==par_name){
        par_value<-as.double(unlist(str_split(str_trim(data[x][i],side="both")," ",n=vectorLength)))
      }
    }
  } else if(length(x)==1){
    par_value<-as.double(unlist(str_split(str_trim(data[x],side="both")," ",n=vectorLength)))
  } else {
    cat(paste("no value for parameter ", par_name,sep=""))
    par_value<-NULL
  }
  
  return(par_value)
  
}