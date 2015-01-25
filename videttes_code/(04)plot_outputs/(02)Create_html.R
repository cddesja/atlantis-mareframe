this_run<-"TBGB_06_fourth_runb"
this_path<-paste(DIR$'Base',"\\ATLANTIS\\Model_runs\\TBGB\\",this_run,sep="")
figures_path<-paste(this_path,"\\figures\\",sep="")
html_path<-paste(this_path,"\\html\\",sep="")

#read in groups file
GROUPS<-read.csv(paste(this_path,"\\TBGB_Groups.csv",sep=""))

#create a blank page
this_file<-paste(html_path,"blank.html",sep="")
cat("<html><html/>",file=this_file,append=FALSE)

#create diagnostics page with 2 frames one at the top with links for each group, the bottom to display the diagnostics plots
this_file=paste(html_path,"diagnostics_ALL.html",sep="")
#start the file 
cat("<html>\n",file=this_file,append=FALSE)
frames<-paste("<frameset rows=\"10%,90%\">\n
              <frame src=\"diagnostics_top_frame.html\", name=\"diagnostics_top\" />\n
              <frame src=\"blank.html\", name=\"diagnostics_bottom\" />\n
              </frameset>",sep="")
cat(frames, file=this_file,append=TRUE)
cat("</html>",file=this_file,append=TRUE)

#Create diagnostics top page, with the links for each group
this_file=paste(html_path,"diagnostics_top_frame.html",sep="")
cat("<html>",file=this_file,append=FALSE)
table<-paste("<body><table><tr><td colspan=19>Diagnostic plots for all groups</td></tr><tr>\n",sep="")
cat(table,file=this_file,append=TRUE)
#add link for each year
for(group in GROUPS$Code){
  cat("<td>",file=this_file,append=TRUE)
  link<-paste("<a href=diagnostics_",group,"_frames.html"," target=\"diagnostics_bottom\">",group,"</a>",sep="")
  cat(link,file=this_file,append=TRUE)
  cat("</td>",file=this_file,append=TRUE)
}


#create the diagnostics page for each group
image_height<-400
for(g in 1:(length(GROUPS$Code))){
  group<-GROUPS$Code[g]
  name<-str_trim(GROUPS$Name[g],side="both")
  InvertType<-GROUPS$InvertType[g]
  nc<-GROUPS$NumCohorts[g]
  
  #first make page with 2 cols, the first of which will be the diagnostics_group page about to be created, the second which will be the parameters page for this group
  this_file<-paste(html_path,"diagnostics_",group,"_frames.html",sep="")
  #start the file 
  cat("<html>\n",file=this_file,append=FALSE)
  frames<-paste("<frameset cols=\"60%,40%\">\n
                <frame src=\"diagnostics_",group,".html\", name=\"diagnostics_left\" />\n
                <frame src=\"parameters_",group,".html\", name=\"diagnostics_right\" />\n
                </frameset>",sep="")
  cat(frames, file=this_file,append=TRUE)
  cat("</html>",file=this_file,append=TRUE)
  
  
  this_file<-paste(html_path,"diagnostics_",group,".html",sep="")
  cat(paste("<html><body><H4>",name, " (", group,")</H4>",sep=""),file=this_file,append=FALSE)
  table<-"<table><tr><td>"
  cat(table,file=this_file,append=TRUE)
  if(nc==1){
    #BIOMASS
    figure_file<-paste(figures_path,"TracerPlot",name,"_N.jpg",sep="")
    image=paste("<img src=\"",figure_file,"\" height=\"",image_height,"\"></img>\n",sep="")
    cat(image,file=this_file,append=TRUE)
  } else if(nc==2 & InvertType!="FISH"){
    #BIOMASS N1
    figure_file<-paste(figures_path,"TracerPlot",name,"_N1.jpg",sep="")
    image=paste("<img src=\"",figure_file,"\" height=\"",image_height,"\"></img>\n",sep="")
    cat(image,file=this_file,append=TRUE)
    #BIOMASS N2
    cat("</td><td>",file=this_file,append=TRUE)
    figure_file<-paste(figures_path,"TracerPlot",name,"_N2.jpg",sep="")
    image=paste("<img src=\"",figure_file,"\" height=\"",image_height,"\"></img>\n",sep="")
    cat(image,file=this_file,append=TRUE)
  } else {
    #BIOMASS
    figure_file<-paste(figures_path,"TracerPlot",name,"_N.jpg",sep="")
    image=paste("<img src=\"",figure_file,"\" height=\"",image_height,"\"></img>\n",sep="")
    cat(image,file=this_file,append=TRUE)
    #Numbers
    cat("</td><td>",file=this_file,append=TRUE)
    figure_file<-paste(figures_path,"TracerPlot",name,"_Nums.jpg",sep="")
    image=paste("<img src=\"",figure_file,"\" height=\"",image_height,"\"></img>\n",sep="")
    cat(image,file=this_file,append=TRUE)
    #ResN
    cat("</td></tr><tr><td>",file=this_file,append=TRUE)
    figure_file<-paste(figures_path,"TracerPlot",name,"_ResN.jpg",sep="")
    image=paste("<img src=\"",figure_file,"\" height=\"",image_height,"\"></img>\n",sep="")
    cat(image,file=this_file,append=TRUE)
    #StructN
    cat("</td><td>",file=this_file,append=TRUE)
    figure_file<-paste(figures_path,"TracerPlot",name,"_StructN.jpg",sep="")
    image=paste("<img src=\"",figure_file,"\" height=\"",image_height,"\"></img>\n",sep="")
    cat(image,file=this_file,append=TRUE)
  }
  #close the file
  cat("</td></tr></table></body></html>",file=this_file,append=TRUE)
}



#####################################################################################
#####################################################################################
############################### HAVE NOT CHECKED PASS HERE ##########################
#####################################################################################
#####################################################################################



###############################
## PARAMETERS
pars<-c("Predator-Prey")
image_height<-900
#create parameters page with 2 frames one top, one bottom, for each group
for(g in 1:(length(unique(FuncGroupNamesInPlotOrder$Code)))){
  group<-unique(FuncGroupNamesInPlotOrder$Code)[g]
  name<-str_trim(unique(FuncGroupNamesInPlotOrder$Name)[g],side="both")
  
  #Create the parameter pages for this group
  for(p in pars){
    this_file<-paste(html_path,"parameters_",group,"_",p,".html",sep="")
    cat(paste("<html><body><H5>",p,"</H5>\n",sep=""),file=this_file,append=FALSE)
    figure_file<-paste(figures_path,"DIET_check\\",group,".jpg",sep="")
    image=paste("<img src=\"",figure_file,"\" height=\"",500,"\"></img><BR>\n",sep="")
    cat(image,file=this_file,append=TRUE)
    
    figure_file<-paste(figures_path,"mortality_PREY_",group,".jpg",sep="")
    image=paste("<img src=\"",figure_file,"\" height=\"",500,"\"></img>\n",sep="")
    cat(image,file=this_file,append=TRUE)
    cat("</body></html>",file=this_file,append=TRUE)
  }
}


## PARAMETERS
other_pars<-c("Predator-Prey","Spawning","EatsMe","Mortality","SpatialBiomass")
#create parameters page with 2 frames one top, one bottom, for each group
for(g in 1:(length(unique(FuncGroupNamesInPlotOrder$Code)))){
  group<-unique(FuncGroupNamesInPlotOrder$Code)[g]
  name<-str_trim(unique(FuncGroupNamesInPlotOrder$Name)[g],side="both")
  InvertType<-FuncGroupNamesInPlotOrder$InvertType[g]
  nc<-FuncGroupNamesInPlotOrder$NumCohorts[FuncGroupNamesInPlotOrder$Code==group][1]
  
  this_file=paste(html_path,"parameters_",group,".html",sep="")
  #start the file 
  cat("<html>\n",file=this_file,append=FALSE)
  frames<-paste("<frameset rows=\"10%,90%\">\n
                <frame src=\"parameters_",group,"_top_frame.html\", name=\"parameters_top\" />\n
                <frame src=\"blank.html\", name=\"parameters_bottom\" />\n
                </frameset>",sep="")
  cat(frames, file=this_file,append=TRUE)
  cat("</html>",file=this_file,append=TRUE)
  
  #Also create parameters top page, with the links for each parameter
  this_file=paste(html_path,"parameters_",group,"_top_frame.html",sep="")
  cat("<html>",file=this_file,append=FALSE)
  table<-paste("<body><table><tr><td colspan=19>Parameters for group ",name,"</td></tr><tr>\n",sep="")
  cat(table,file=this_file,append=TRUE)
  #add link for each year
  for(p in other_pars){
    if(p == "SpatialBiomass"){
      #       if(group %in% c("SCA","SCL","MUS","OYS","IVH","IVS","CEP")){
      if(nc==2 & InvertType!="FISH"){                     
        cat("<td>",file=this_file,append=TRUE)
        link<-paste("<a href=..\\..\\..\\..\\..\\R\\",name,"_","N1.html"," target=\"parameters_bottom\">",p,"_N1</a>",sep="")
        cat(link,file=this_file,append=TRUE)
        cat("</td>",file=this_file,append=TRUE)
        cat("<td>",file=this_file,append=TRUE)
        link<-paste("<a href=..\\..\\..\\..\\..\\R\\",name,"_","N2.html"," target=\"parameters_bottom\">",p,"_N2</a>",sep="")
        cat(link,file=this_file,append=TRUE)
        cat("</td>",file=this_file,append=TRUE)
      } else {
        cat("<td>",file=this_file,append=TRUE)
        link<-paste("<a href=..\\..\\..\\..\\..\\R\\",name,"_N.html"," target=\"parameters_bottom\">",p,"</a>",sep="")
        cat(link,file=this_file,append=TRUE)
        cat("</td>",file=this_file,append=TRUE)
      }
    } else{
      cat("<td>",file=this_file,append=TRUE)
      link<-paste("<a href=parameters_",group,"_",p,".html"," target=\"parameters_bottom\">",p,"</a>",sep="")
      cat(link,file=this_file,append=TRUE)
      cat("</td>",file=this_file,append=TRUE)     
    }    
  }
  close<-paste("</tr></table></body></html>\n",sep="")
  cat(close,file=this_file,append=TRUE)
  
}

#create the mortality pages
for(g in 1:(length(unique(FuncGroupNamesInPlotOrder$Code)))){
  group<-unique(FuncGroupNamesInPlotOrder$Code)[g]
  this_file<-paste(html_path,"parameters_",group,"_",p,".html",sep="")
  cat(paste("<html><body><H5>",p,"</H5>\n",sep=""),file=this_file,append=FALSE)
  figure_file<-paste(figures_path,"mortality_",group,".jpg",sep="")
  image=paste("<img src=\"",figure_file,"\" height=\"",500,"\"></img>\n",sep="")
  cat(image,file=this_file,append=TRUE)
  cat("</body></html>",file=this_file,append=TRUE)
}
