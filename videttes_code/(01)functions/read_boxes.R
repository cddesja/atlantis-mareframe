read_boxes<-function(fnm){
  #fnm is the file name (including path)
  #   example (test) file
#     my_nc_path<-"C:\\Projects\\2013\\FIFI1401\\Resources\\ATLANTIS_models\\TBGB\\TBGB_first_test\\create_biol_input\\"
#    
#     fnm<-paste(my_nc_path,"TBGB.bgm",sep="")
  
  verts=NULL
  iface=NULL
  b_area=NULL
  realarea=NULL #this doesn't seem to be used
  b_botz=NULL
  cent =NULL
  bid =NULL
  ibox =NULL #id for each of the boxes
  nbox=NULL  #number of boxes
  nface =NULL #number of faces
  
  this_grep<-function(pattern, x){
    temp<-x[grep(pattern,x)]
    temp<-str_trim(temp,side="both")
    temp<-unlist(strsplit(temp," "))
    this_out<-as.double(temp[length(temp)])
    return(this_out)
  }
    
  #Read in number of box faces and number of boxes
  alldata<-readLines(fnm)
  nbox<-this_grep("nbox",alldata) 
  nface<-this_grep("nface",alldata)  
 
  #box ids
  bid<-seq(0,nbox-1)
  for(b in 1:nbox){
    this_boxid<-bid[b]
    this_data<-alldata[grep(paste("box",this_boxid,".",sep=""),alldata,fixed=TRUE)]
    #verts. all vertices for each box
    vertids<-grepl(".vert ",this_data,fixed=TRUE)
    cc       <- strsplit(this_data[vertids],' ')
    part1    <- unlist(cc)[3*(1:length(this_data[vertids]))-1]
    part2    <- unlist(cc)[3*(1:length(this_data[vertids]))  ]
    verts[[b]]<-paste(part1,part2)
    rm(vertids,cc,part1,part2)
    #iface. ids for all faces
    iface[b]<-unlist(strsplit(this_data[grepl(".iface",this_data,fixed=TRUE)],"\t"))[2*(1:length(this_data[grepl(".iface",this_data,fixed=TRUE)]))]
    #.area
    b_area[b]<-unlist(strsplit(this_data[grepl(".area",this_data,fixed=TRUE)],"\t"))[2*(1:length(this_data[grepl(".area",this_data,fixed=TRUE)]))]
    #cent. is this a point inside thebox..?
    cent[b]<-unlist(strsplit(this_data[grepl(".inside",this_data,fixed=TRUE)],"\t"))[2*(1:length(this_data[grepl(".inside",this_data,fixed=TRUE)]))]
    #ibox. box identiry
    ibox[b]<-unlist(strsplit(this_data[grepl(".ibox",this_data,fixed=TRUE)],"\t"))[2*(1:length(this_data[grepl(".ibox",this_data,fixed=TRUE)]))]
    #b_botz. box depth
    b_botz[b]<-unlist(strsplit(this_data[grepl(".botz",this_data,fixed=TRUE)],"\t"))[2*(1:length(this_data[grepl(".botz",this_data,fixed=TRUE)]))]
        
  }
  out<-list(nbox, nface, bid,cent,b_area,verts,iface, b_botz, ibox)
  names(out)<-c("nbox", "nface", "bid","cent","b_area","verts","iface", "b_botz", "ibox")
  return(out)
  
}